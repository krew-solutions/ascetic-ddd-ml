# ADR-0034: A scope that absorbed a failed statement is aborted, not committed

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0028, 2026-10-23), whose findings about
the server are its own, made by `verify/pg/silent-rollback/exp.sh` there
(EXP 1 to 5) on PostgreSQL 14.24; the port's tests, named below, hold on
PostgreSQL 16.13. Where the reference's design rests on what its driver
can and cannot do, this port rests on what Caqti can, which differs, and
the decisions say where.

Amends ADR-0003, whose close of a scope was "a failing `COMMIT` becomes
`Session_error.Commit`; a failing rollback does not replace the error that
caused it", and the shape of `Session_error.t`.

## Context

**The incident, in the reference.** An application stored a role
assignment inside a larger transaction. A unique index refused the row: the
user held the role already. The store mapped the refusal to "already
assigned", the reaction treated that as expected and returned `Ok`, and
`atomic` closed the scope with `COMMIT`. The caller was told the sign-in was
durable. None of it was.

**What the server does.** PostgreSQL aborts a transaction on its first
failed statement, whatever the failure: every later statement fails with
`25P02` (`in_failed_sql_transaction`) until the block ends (EXP 1, 2). A
`COMMIT` sent to an aborted block is answered with the command tag
`ROLLBACK` and no error (EXP 1): everything the transaction did is gone,
and the only trace on the wire is a tag. `INSERT … ON CONFLICT (id) DO
NOTHING` does not cover a second unique index: the refusal is an error, so
the abort follows.

**What Caqti lets through.** Caqti 2.3's PostgreSQL driver runs `COMMIT`
as a prepared statement and checks the result's status, `PGRES_COMMAND_OK`
for a `COMMIT` and a `ROLLBACK` alike; the command tag is not read. The
libpq connection is not handed out (`driver_connection` is `None`), and the
`postgresql` binding, 5.4, has no `PQtransactionStatus` in any case: the
status byte of `ReadyForQuery`, the direct signal, is out of reach, as it is
for the reference's driver. Every request goes through the extended
protocol, `PQsendQueryParams` or a prepared statement, which takes one
statement at a time: the reference's `SELECT 1; COMMIT` in one message,
which the server refuses at the probe before it reaches the `COMMIT`,
cannot be sent through Caqti; the simple protocol is used by the driver for
its own `DEALLOCATE` only.

**What the session did.** `Scope.Make` sent `COMMIT` when the scope
returned `Ok` and reported success unless the driver returned an error,
which it does not in that state. A refused `RELEASE SAVEPOINT` became
`Commit`, but the savepoint was not rolled back, so the enclosing
transaction stayed aborted, against the contract that a failing nested
scope leaves the surrounding transaction alive. The observer heard the
scope end as failed on a refused close already, and a failed rollback
already abandoned the session (ADR-0003): two of the reference's holes were
not here.

**What is legitimate.** Some refusals are expected and must be absorbable
while the transaction goes on. The server allows one way: a savepoint around
the statement, rolled back to when the statement is refused (EXP 3).

**What a round trip costs here.** Measured with pgbench on PostgreSQL
16.13, one client over a Unix socket, the data on tmpfs, two rounds of five
seconds, each statement a message of its own as Caqti sends them:

| Transaction | `COMMIT` | `SELECT 1`, then `COMMIT` |
| --- | --- | --- |
| empty | 22–23 µs | 36–38 µs |
| one `INSERT` | 47–54 µs | 62–67 µs |

A probe sent with every commit is about 15 µs on this socket, and a full
round trip on a network, a tenth to a whole millisecond.

## Decision

1. **The server's verdict closes every scope it has reason to doubt.** A
   nested scope that succeeded is closed with `RELEASE SAVEPOINT`, which the
   server refuses with `25P02` in an aborted transaction (EXP 3). The
   outermost scope is closed with `COMMIT`, after a probe, `SELECT 1`,
   refused the same way, where a statement run through the connection
   failed since the transaction began; where none did, the transaction is
   whole by the server's own rule - it is aborted by a failed statement and
   by nothing else - and the `COMMIT` goes without the probe. The verdict is
   the server's: a failure the scope recovered from itself, to a savepoint
   of its own, leaves the transaction whole, and the probe is answered.

2. **A refused close leaves nothing open behind it.** Whatever refused it,
   the scope is rolled back - `ROLLBACK`, or `ROLLBACK TO SAVEPOINT` so that
   the enclosing transaction goes on. After a `COMMIT` the server refused
   for a reason of its own (a deferred constraint, a serialization failure)
   the block is already ended, and the `ROLLBACK` is a warning (EXP 5). A
   rollback that fails abandons the session, as it did.

3. **A refusal of `25P02` is `Session_error.Aborted cause`**: the scope
   reported success over a failure it absorbed. It is never transient:
   whatever the absorbed failure was, retrying runs the same code, which
   absorbs it again - and an absorbed failure is a defect whether or not a
   retry happens to pass. Any other refusal stays `Session_error.Commit`.
   The algorithm is `Scope.Make`'s, over a backend that answers its close
   with `Scope.Refused` or `Scope.Aborted`: the in-memory session models the
   server's abort and holds the paths in a journal, the Caqti session asks
   the server.

4. **The connection the session hands out remembers; it never decides.**
   `Caqti_session.connection` is the driver's connection behind
   `Remembering`, a `Caqti_eio.CONNECTION` of its own that passes every
   request on and keeps the first failure the server reported through it
   since the transaction began or a scope last closed: its SQLSTATE and
   message, `Session_error.cause`. The memory names the cause of an
   absorbed failure and says whether the server is asked before a
   `COMMIT`; the answer is the server's. The driver runs one statement at a
   time on a connection, so the first failure reported is the first that
   happened, and the reference's placeholder for a `25P02` reported out of
   order is not needed. Statements of the session's own, `BEGIN`, the
   savepoints, the close, go to the driver's connection and are not
   remembered: the memory is of the scope's statements alone.

5. **The classification travels with the error**, as it did: `Driver_error.t`
   in `Begin`, `Commit`, `Abandoned`, and `Session_error.is_transient`
   answers at the port without a driver; `Aborted` answers false. The inbox
   and the outbox read it as they did, and change nothing.

The rule this gives a consumer: **a server error inside a transaction is
never absorbed in the scope that met it.** A refusal meant to be absorbed is
met in a nested `atomic` of its own - the scope fails, its savepoint is
rolled back, and the enclosing scope maps the failure to its own meaning
and goes on. Or the statement makes the refusal an outcome rather than an
error (`ON CONFLICT … DO NOTHING` covering the index that refuses), where
the outcome alone tells what happened.

## Consequences

- Code that absorbed a server error and carried on - the defect class - now
  fails loudly with the cause named, instead of losing its transaction.
- The probe costs a round trip where a statement failed, and nothing where
  none did; the common transaction pays nothing. What the memory cannot
  see is a statement run past the connection the session hands out, on the
  driver's connection the session was created over: such a statement's
  failure aborts the transaction unseen, and the `COMMIT` goes without the
  probe, as it did before this decision. A repository never holds that
  connection; a composition root or a test harness does, and runs its
  statements outside the session's scopes.
- A nested scope that absorbed a failure is rolled back to its savepoint and
  refused, so the enclosing scope stays alive - as the port promised and
  the old close broke. A refused `COMMIT` of the outermost scope is now
  followed by a `ROLLBACK`, a warning on the server; the in-memory journal
  records it, and the composite session's tests say so.
- Breaking: `Session_error.t` has a new case, `Aborted of cause option`,
  which an exhaustive match outside the library must take; the
  `Scope.BACKEND` signature's `commit` and `release` answer with
  `Scope.refusal`; `Memory_session.run` is new beside `record`.
- `Aborted None` is returned where the probe was refused and the memory
  holds nothing, which cannot happen through the connection the session
  hands out: the probe is sent only where the memory holds a failure.

## Alternatives rejected

- **The probe with every commit**, the reference's decision 1 as it stands.
  Exact without the memory, and what the reference could afford in the same
  message as the `COMMIT`; here a round trip of its own, a fifth to a half
  of an empty transaction on a Unix socket and a whole network round trip
  elsewhere, on every transaction, for a verdict the server gives for free
  where no statement failed. Kept as the step to take if a statement past
  the handed-out connection is ever found to have aborted a transaction.
- **The connection's memory as the verdict**, the reference's rejected
  alternative: exact for statements through the wrapper and free of the
  probe, but a second state machine of the server's - a `ROLLBACK TO` of the
  scope's own savepoint restores a block the memory would still hold
  aborted. It names the cause and says when to ask; the server decides.
- **Reading the status byte from the driver.** The direct signal, and free;
  neither Caqti nor the `postgresql` binding exposes it. The path upstream
  stays open, and the probe goes when it lands.
- **A savepoint around every transaction's body**, released before the
  commit: it detects the abort at the release, but costs two round trips
  per transaction, more than the probe, and adds nothing the probe does
  not.
- **An absorbed failure classified by its cause** (a deadlock retried, a
  unique violation parked). It makes a retry pass where the deadlock does
  not recur, and so hides the code that absorbed it; the cause is named in
  the error, which is what fixing it needs.
- **Handing out the driver's connection and remembering nothing**, as
  before, with `Aborted None` always. The probe would then have to go with
  every commit, and the error would not say what was absorbed.

## Executors

`test/session/test_pg.ml`, against a live PostgreSQL:

- "a scope that absorbed a refused statement is aborted and keeps nothing" -
  `Aborted` with the `23505` and its constraint in the message, nothing
  durable, the observer hearing the scope end as failed, the connection
  serving the next scope;
- "a nested scope that absorbed a refusal is aborted and the outer one goes
  on" - the nested scope refused, the outer one committing the rest;
- "a failed rollback abandons the session" - a scope that released its own
  savepoint: the rollback finds none, the session is abandoned, the
  enclosing scope refused with `Abandoned`.

Three guards hold the decision's edges: "a failure the scope recovered from
itself does not abort it" (the memory decides nothing: the probe is sent and
answered), "a commit the server refuses is a failed commit and the
connection stays", "a failure before a scope does not abort it". The scope
algorithm's paths are held in `test/session/test_memory.ml`, "an absorbed
failure": the aborted scope rolled back and refused with the statement
named, every statement after the abort refused, the nested case, a refused
close rolled back, a failure before a scope. The classification and the
text of the error are held in `test/session/test_transient.ml`.
