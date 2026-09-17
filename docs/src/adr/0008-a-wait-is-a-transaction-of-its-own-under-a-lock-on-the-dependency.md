# ADR-0008: A wait is a transaction of its own, serialized with the dependency's mark by a lock on its identity

## Status

Accepted (2026-09-17). Follows the decision taken first in the Rust
reference implementation, reached there through a discriminator pass: five
designs from different premises, eleven two-session experiments in
PostgreSQL, checked objections, synthesis. Ported with the inbox; the tests
named under Consequences pass here.

## Context

ADR-0005 made a message ahead of its dependency wait out of the queue, woken
by the dependency's mark in the same statement. A first implementation
decided and recorded that wait in two statements of a transaction that then
went on: the check a plain `SELECT`, the set an `UPDATE`, both from the walk,
which took the next head of the slot and ran its subscriber before the
transaction committed. Transactions are plain `BEGIN`, READ COMMITTED. Under
READ COMMITTED an `UPDATE` "will only find target rows that were committed as
of the command start time" (the PostgreSQL manual, "Read Committed Isolation
Level", chapter 13, Concurrency Control, unchanged from PostgreSQL 14 to 18:
https://www.postgresql.org/docs/current/transaction-iso.html). This is the
definition of the isolation level, not a defect of a version. So a
dispatcher B marking the dependency `d` in another slot ran its `woken`
update against a version of `m` in which `waiting_for` was still null, woke
nothing, and committed; A committed its wait afterwards; `m` waited for a
processed dependency for ever, or, with `max_wait`, was parked with
"dependency never arrived", which is false. Two paths: `d` present and in
flight, and `d` not yet arrived at A's check. Preconditions: two or more
slots and a dependency across slots; with one slot the dispatchers serialize
on it. The model did not see it: `WaitIn` in `verify/tla/Inbox.tla` checks
and sets in one step.

Two facts bound every fix. **F1**: blocking inside a statement does not
refresh its snapshot: an advisory lock taken in a CTE of the mark statement
blocked until A committed, and the `woken` update of the same statement still
missed `m`; the next statement saw it. **F2**: the one read that returns the
latest committed version is a row lock on the row itself, `FOR SHARE`, and a
qualification on the changing column defeats it; a row that does not exist
yet cannot be locked.

Constraints: no lost wake on either path; a deadlock is a database error and
stops every loop of `run`, so lock cycles must be impossible by argument;
PgBouncer in transaction pooling, so nothing session-scoped; slots stay
independent; the happy path counts round trips; the model must describe what
happens.

## Decision

1. **A wait is a transaction of its own.** A dispatch that finds its head
   `m` ahead of an unprocessed dependency `d` takes
   `pg_advisory_xact_lock(hashtext(<table>), hashtext(<identity of d>))`,
   then in one fresh statement re-checks and sets:
   `UPDATE ... SET waiting_for = d WHERE <m> AND NOT EXISTS (<d processed>)
   RETURNING 1`, and commits. An empty `RETURNING` means `d` was marked in
   between: nothing to wait for, take again. One advisory lock per
   transaction, taken last; after it, nothing else is acquired.
2. **The mark takes the same lock on its own identity**, as a statement of
   its own before the mark statement (F1 forbids folding it in), only when
   the table has more than one slot; `resolve` takes it always and runs in a
   transaction, since it is the one marker that runs beside a slot holder
   even with one slot. The lock orders {re-check, set, commit} against
   {mark, commit} totally: whoever comes second reads, in a fresh statement,
   what the first committed.
3. **The expiry sweep leaves the dispatch transaction** and runs as a
   statement of its own. Otherwise a cycle exists: an expirer holds a row
   that waited for `d` while its own head needs the lock of `d`, and the
   marker of `d` waits in `woken` for that row.
4. **Deadlock-freedom, by argument.** The slot row is `SKIP LOCKED` and
   never waits. A queue head is locked by nobody but its slot's holder:
   `publish` takes no lock on an existing row (`ON CONFLICT DO NOTHING`
   returns at once against a row held `FOR UPDATE`); expire, resolve and
   unpark touch waiting or parked rows only. The `woken` update can wait
   only for an expirer, which holds expired rows and waits for nothing
   (`FOR UPDATE SKIP LOCKED`). Every transaction holds at most one advisory
   lock and acquires only woken-row locks after it. No cycle.

Who waits for whom: a wait for the marker of its `d` between the marker's
lock and its commit; a marker for a wait between its lock and its commit, a
re-check, one update, `COMMIT`; an operator's `resolve` for as long as the
operator's transaction. Unrelated slots share no lock. Transaction-level
advisory locks end with the transaction (the manual's "Advisory Locks",
https://www.postgresql.org/docs/current/explicit-locking.html); PgBouncer's
transaction pooling forbids only session-level ones.

## The shape of `dispatch`

Two variants were weighed: a loop of wait-transactions inside one call
until a head is processable, the signature unchanged; or one call, one
transaction, a dispatch that sets its head aside committing and returning
`Outcome.Set_aside`, the next call taking the slot's next head. The second
was chosen: it keeps "one transaction per call" true, it makes the set-aside
visible where every other thing a dispatch does is visible, and `run` treats
it as work done, no poll sleep.

## Consequences

- One statement more per mark on tables with more than one slot, measured in
  the reference implementation at the cost of `SELECT 1`.
- Advisory locks are a database-global namespace; a foreign application
  using the same key stalls a wait or a mark, never corrupts. Bounded by the
  two-key form with the table's name as the class.
- One SQL expression for the identity hash, shared by wait, mark and
  resolve; two expressions drifting apart would reopen the race silently.
- The `waiting_since` stamp is the commit of the wait, so `max_wait` counts
  from when the wait became visible, not from a call that may still be
  running a subscriber.
- Model: `WaitIn` and `Commit` are literally the implementation. A constant
  `AtomicWait`, `TRUE` everywhere but in `InboxSplitWait.cfg`, where a wait
  is settled a step after its check and a mark in between cannot see it: TLC
  finds `WaitingIsAside` violated, a message waiting for a processed
  dependency. The model records why the lock exists, as `InboxNoWaiting.cfg`
  records why waiting exists.
- `verify/tla/InboxPg.tla`, the statements against PostgreSQL under READ
  COMMITTED, snapshots per statement, versions stamped by commits, the
  slot's lock, the advisory lock, refines `Inbox.tla` by a mapping TLC
  checks, so the protocol model's two assumptions, one holder per slot and
  an atomic wait, are earned rather than posited. Its switches replay the
  designs this ADR weighed: without the lock, the lost wake; with the head
  read in the take's statement, the stale head of ADR-0007; walking on
  after a set-aside with the lock held, the lock cycle, each a required
  violation in `check.sh`.
- Tests, all in `test/inbox/test_inbox.ml`: "a wait set while its dependency
  arrives and is marked elsewhere is woken" (dependency not yet arrived) and
  "a wait set while its dependency is being marked elsewhere is woken"
  (dependency in flight; `served_at` biased so the dependent's slot is taken
  first); "two slots are worked at once" shows the slots stay independent;
  the wait visible from another session once the call returned is the first
  assertion of "a dependency that never arrives parks the message after
  max_wait"; "loops over slots with dependencies across them lose no wake"
  is the stress run, three loops over four slots, 120 messages with random
  acyclic dependencies across slots, published in random order while the
  loops run, small random sleeps, `max_wait`, one poison message parked and
  resolved by hand, which must end in `Ok` with everything processed and no
  row waiting for a processed dependency: the lost wake as one query.
- The stress run found the take of ADR-0007 wrong in the reference
  implementation: one statement locked the slot and read its head, but its
  snapshot predated the lock, and a head the previous holder processed in
  between failed the re-check `FOR UPDATE` makes on the latest version; the
  slot came back without a head, a database error that stopped the loops.
  The head is read in a statement of its own after the lock, one round trip
  more per dispatch; ADR-0007's point 2 is amended so.
- A receipt names the slot the row landed in, so the trace model places
  every arrived message exactly instead of guessing the untouched ones.
  Runs with several dispatchers at once are not trace-checked: the order
  their events are logged in is not the order of their commits, and the
  rules that reordered the log by snapshots and transaction ids kept
  growing and missing cases while finding no defect; such runs assert their
  outcome and the table's state instead (`verify/tla/README.md`).

## Alternatives rejected

| Design | Both paths | No deadlock | Slots independent | Happy path | Model exact |
|---|---|---|---|---|---|
| SERIALIZABLE + retry on 40001 | yes | yes | **no** | retries | yes |
| Lock the dependency inside the long transaction | advisory: yes | **no** | **no** | 0 | yes |
| **This decision** | yes | yes | yes | +1 statement, slots > 1 | yes |
| The waiter re-checks after its own commit | yes | yes | yes | 0 | **no** |
| The marker finds dependents by their dependency list | yes | **no** | **no** | GIN index | yes |

**SERIALIZABLE.** Two takes of *different* slots both read `<table>_slots`
and both write `served_at`; the second commit fails as a pivot, so every
concurrent pair of dispatchers conflicts and ADR-0007's parallelism is gone.
`FOR UPDATE SKIP LOCKED` on a slot row updated after the snapshot fails with
"concurrent update". The subscriber inherits the level and can abort the
dispatch with its own reads.

**A lock on the dependency held through the dispatcher's transaction.** The
walk sets several heads aside in one transaction, so a checker accumulates
locks; two checkers each holding one and wanting the other's deadlock
without any cycle in the dependencies. A row lock instead of the advisory
one cannot cover a dependency not yet arrived, and even `FOR KEY SHARE`
waits for the whole processing transaction of the dependency.

**The waiter wakes itself after committing**: re-check `d` with `FOR SHARE`
outside any transaction, un-wait on success. Zero cost and no marker
change, but the wait is no longer atomic: `WaitingIsAside` holds only
eventually, the trace check of exact `woken` sets fails for a mark whose
snapshot predates the wait's commit, `max_wait` can park a row whose
dependency was processed because `waiting_since` is stamped before a
subscriber runs, and the re-check blocks on the dependency's processing
transaction for its whole length.

**The marker finds dependents by their immutable dependency list**,
`metadata->'causal_dependencies' @> [d]`, and lets EvalPlanQual re-check the
new version. It blocks for the checker's whole transaction on every mark
with an unfinished dependent, rewrites every unprocessed dependent on every
mark, needs a GIN index the DDL lacks, and deadlocks with two dependents of
`d` in one slot separated by a waiting head.
