# ADR-0009: Which failures stop the loops, which wait, and which park at once

## Status

Accepted (2026-09-17). Follows the decision taken first in the Rust
reference implementation, and was ported with the outbox and the inbox,
with the classification placed in the session's Caqti backend.

## Context

`run` stopped every loop on any `Error` from `dispatch`: in the inbox any
error of the database; in the outbox any error of the database and any error
of the subscriber, since `dispatch` returned the subscriber's error. So a
broker away for a second stopped the outbox dispatcher process, and a lock
cycle between the subscriber's own writes and application traffic, possible
after ADR-0008 all the same, since the subscriber writes inside the
dispatcher's transaction, stopped the inbox loops with `40P01`. Supervisors
restart processes, so the failure mode was churn rather than loss; but a
loop that cannot tell an error of the moment from a defect can do no better
than die on both.

A subscriber's failure in the inbox was of one kind: tried again after a
backoff, parked after `max_attempts` (ADR-0004). A payload that cannot be
read was tried that many times, holding its slot's head meanwhile, for an
outcome known at the first attempt.

ADR-0002 had given the ports `Connection`, `Request` and `Malformed`, and
noted that `Request` conflated a transient deadlock with a permanent bad
statement because no classification was available at that layer. The
PostgreSQL driver of Caqti keeps the SQLSTATE in its error message, so the
classification is available where the backend is.

## Decision

1. **Errors of the database are told apart by SQLSTATE**, in one place for
   both adapters and the session, `Ascetic_session_caqti.Transient`. Of the
   moment: class `08` (connection), class `53` (resources), `40001`,
   `40P01`, `40003`, `57P01`–`57P03` (the server going down or not yet up);
   and, without a code, the connection's, a communication error of the
   driver, a connection that could not be made, a failure the client library
   raises on a connection whose server is gone. Everything else is a
   defect: a statement, the schema, a value that could not be encoded or
   decoded.

2. **The verdict travels with the error.** `Driver_error.t`, in the session
   port, is the driver's text and whether the failure is of the moment; the
   backend fills it where the driver's error is at hand. `Session_error.t`
   carries it in `Acquire`, `Begin`, `Commit` and `Abandoned`;
   `Outbox_error.t` and `Inbox_error.t` carry it in `Database` and the
   session error in `Session`, with `is_transient` reading the verdict. The
   ports name no driver type, as ADR-0002 and ADR-0003 asked.

3. **A loop meeting an error of the moment waits and goes on.** The wait
   starts at `poll_interval` and doubles with each such error in a row, up
   to `Loops.max_pause`, sixty seconds by default. The transaction was
   rolled back, so the message, or the batch, comes back; the attempt is not
   counted, as ADR-0004 counts only what the subscriber returned. A defect
   stops every loop, and `run` returns it.

4. **In the outbox a failing subscriber is, for the loop, a failure of the
   moment**: the batch rolled back, the loop waits the same way and delivers
   it again. `dispatch` still returns `Error (Subscriber e)` to a caller of
   its own. A message no subscriber will ever take stalls its slot, as
   before; a dead-letter for the outbox is a decision not taken, the case
   being judged unlikely.

5. **The inbox subscriber returns `(unit, Failure.t) result`.**
   `Failure.transient` is tried again as before. `Failure.permanent` is the
   subscriber's verdict that no retry will succeed, a payload it cannot read,
   an invariant the message breaks, and the message is parked at once, the
   attempt recorded, `last_error` set; the observer's `failed` carries the
   failure with its verdict.

## Consequences

- Tests: "a loop outlives a lost connection", the subscriber has the server
  terminate its connection; the loop waits, goes on and processes the
  message; "a defect stops the loops", the subscriber renames the table, the
  mark fails on an undefined table, `run` returns `Error (Database _)` and
  the rolled-back transaction leaves the table as it was; "a permanent
  failure parks the message at once"; in the outbox, "run outlives a failing
  subscriber"; the SQLSTATE table has unit tests in `test/session`.
- Bounds of a wrong class: an unknown code is a defect and stops the loops,
  which is loud; an error of the moment that never ends is a pause per
  failure up to `max_pause`, and every `dispatched` carries the error to the
  observer, which is where a deployment logs it.
- `Loops.t` has `max_pause`; the inbox's `Inbox_error.t` has no subscriber
  case, and `Malformed` names what the inbox reads back and cannot use, a
  transaction id, a snapshot, a table cut otherwise.
- The lost-connection test found that the session, on a rollback that
  failed, disconnected the pooled connection, after which the Caqti pool
  raised when it checked the connection on its return and every later
  statement of the scope raised too: an exception where the loop expected
  an error. The session no longer disconnects; an abandoned scope rolls back
  on its way out, a connection whose server is gone fails the pool's check
  by itself, and a failure the client library raises rather than returns is
  turned into the scope's error by the backend (ADR-0003, amended).
- The models are unchanged. A permanent verdict is the model's `Fail` with
  parking at the first attempt, `MaxAttempts = 1`; the loop's waits are the
  process's, outside the protocol.

## Alternatives rejected

**Stop on every error**, as before: churn under errors of the moment, and
the outbox process dying on a broker's hiccup.

**Try again on every error**: a defect, the schema drifted, a value of the
wrong type, would loop for ever, making no progress, with nothing but the
observer to say so.

**Count an error of the database as an attempt**: ADR-0004 counts only what
the subscriber returned; an outage would park messages that nobody failed.

**Leave the classification to the subscriber**, returning "later" for
errors of the database it meets: it sees its own statements only, and the
dispatcher's mark can fail on the same connection.

**Parse the SQLSTATE out of the rendered text in the ports**, keeping the
session's reasons as strings: fragile, and the text of another driver would
carry none; the backend has the structured error and the port has a record
with the verdict, which is what the ports needed and could not have at
ADR-0002.
