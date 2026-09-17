# ADR-0004: A failed message holds its slot, and is parked after its attempts

## Status

Accepted (2026-09-17). Follows the decision taken first in the Rust
reference implementation of these building blocks, and was ported with the
inbox.

## Context

The inbox dispatcher took the oldest unprocessed row of its partition whose
dependencies were processed, ran the subscriber in the transaction that
marked the row, and rolled everything back when the subscriber failed. The
row was then the oldest eligible again, so the next call took it first: a
message whose subscriber never succeeds held its partition for ever.
`verify/tla/Inbox.tla` recorded this as a fact.

A retry topic, the common answer around Kafka, moves the failed message onto
a side path and lets the partition go on. It returns later than newer messages
and loses the order the bridge proves end to end (`EndToEndOrder`,
`verify/tla/Bridge.tla`). NATS JetStream keeps per-message state on the server
instead: delivery counts, `Nak`, `Term`, `MaxDeliver`. The inbox already keeps
per-row state, `processed_position`, so it can do what JetStream does without
giving up order.

## Decision

1. **The attempt is recorded in the marking transaction.** The subscriber runs
   in a nested scope, a savepoint; when it fails, its writes roll back to the
   savepoint and the outer transaction commits `attempts + 1`, `last_error` and
   `next_attempt_at`. The row stays locked for the whole of it, so two calls
   never count the same attempt twice, and a database error inside the
   subscriber does not end the transaction: `ROLLBACK TO SAVEPOINT`
   restores it.

2. **A failed message waiting for its backoff holds its slot.** The walk
   over the slot stops at the first row not yet due; rows behind it wait.
   Stepping over it would process newer messages first, the retry-topic order
   loss. A row whose dependencies are not processed is set aside instead
   (ADR-0005): there, waiting in place could hold the slot for ever; here,
   time passes by itself.

3. **After `max_attempts` the message is parked, in place.** `parked_at` is set
   and the selection excludes the row; nothing moves to another table, so a
   later arrival of the same message is still the same row and still a
   duplicate. A parked message's dependents wait, as they wait for a dependency
   that never arrived.

4. **Two operator actions on the adapter**, as `set_position` is on the
   outbox: `unpark`, which resets the attempts and lets the message be taken
   again, and `resolve`, which marks it processed without the subscriber's
   effects and so releases its dependents.

5. **The policy is the inbox's.** The outbox's subscriber is the bridge; its
   failures are infrastructure and are retried without limit. What counts as
   poison is known where the message is processed.

6. **Off by default.** `Retries.unlimited` is unlimited attempts with no
   backoff: the behaviour before this decision, with the attempts now written
   to the row. A failed attempt is an outcome of `dispatch`, not an error, so a
   loop goes on to the next row instead of stopping.

## Consequences

- Columns `attempts`, `last_error`, `next_attempt_at`, `parked_at`.
- `dispatch` returns `Outcome.t`: `Nothing`, `Processed`,
  `Failed { attempts; parked }`, and `Set_aside` from ADR-0005; `Error` is
  for the database. `Inbox_error.t` has no subscriber case.
- One savepoint per message on the happy path, two statements. Measured in
  the reference implementation, draining 200 messages with a subscriber that
  does nothing over localhost: 1.39 ms per message before, 1.54 ms after;
  about 150 µs, a tenth with a subscriber that does nothing.
- Only errors the subscriber returns are counted. A message that kills the
  process is retried on restart with the count unchanged: counting at delivery
  would need a lease with a timeout, which the inbox does without on purpose.
  A crash loop is visible to an operator without a counter.
- The model has `Fail`, `Elapse`, `Unpark`, `Resolve`, the invariants
  `ParkedIsAside` and `ArrivalOrder`, the liveness `EventuallyParked`;
  `EventuallyProcessed` excludes poison messages. `InboxPoison.cfg` holds;
  `InboxPoisonNoParking.cfg` shows the slot held for ever;
  `InboxSkipNotDue.cfg` shows the order lost when a message in backoff is
  stepped over. `check.sh` requires both violations.
- Observer events `failed`, `unparked`, `resolved`; trace validation follows.

## Alternatives rejected

**A retry topic or a dead-letter table.** Order is lost, and a row moved out
of the table is no longer a duplicate when it arrives again.

**Counting attempts in a separate transaction after the rollback.** Nothing on
the happy path; but the row is unlocked between the rollback and the count, so
a second call may take it meanwhile, and the count then waits on that call's
lock.

**Counting at delivery, before the subscriber runs.** Catches the message
that kills the process. Needs the count committed before the work, hence a
lease with a timeout, and the redelivery-by-timeout the inbox avoids by
letting the row lock end with the transaction.

**Classifying errors**, parking at once for some and retrying for others,
was rejected here for want of a second consumer, and taken up by ADR-0009
once the subscriber had a verdict to give.
