# ADR-0016: A subscription cancelled is a handler that is not running

## Status

Accepted (2026-09-19).

## Context

`Subscription.cancel` detached a handler and returned. What it detached was
the calls to come; a call already made went on, and with it whatever it
held. On the channels over a database that is a transaction and a pooled
connection: cancelling the dispatcher of the outbox resolved a promise, the
loop saw it before its next batch, and the batch in hand went on after the
cancel had returned. The Kafka adapter did the opposite and cancelled its
delivery fiber wherever it was, inside the handler included.

Neither lets an application stop in good order. After a cancel that does not
wait, the application cannot know when what the handler uses may be taken
down; and the usual next step, leaving the switch, cancels a loop in the
middle of its batch, so the batch is rolled back and delivered again instead
of being finished. ADR-0015 made that harmless. This decision is about
making it unnecessary.

The reference implementation's `cancel` does not wait either. It is
synchronous there, and awaiting the end of a task is a separate thing its
runtime offers. Here a fiber that waits is an ordinary call, and the type
of `cancel` does not change.

## Decision

1. **`Subscription.cancel` returns when no call of the handler is in
   flight.** It detaches the handler, once, and then waits, every time it is
   called: a second cancel detaches nothing and waits like the first, first
   for the detaching to be over if another fiber is still at it, then for the
   calls. A detaching that raises or is cut short has not detached, and the
   next cancel runs it again.
2. **From inside the handler it does not wait.** A handler may cancel its
   own subscription; the message in hand is its last. That a fiber is
   inside a call is told by a fiber-local mark, inherited by the fibers a
   handler forks.
3. **The mechanism is one module of the bus, `Handling`**: the calls of one
   subscription in flight, `admit`, `run`, `quiesce`, and `loop`, a
   subscription served by a daemon loop of its own whose whole life is one
   call in flight and whose cancel resolves a promise the loop watches.
   Every adapter of this library makes its calls through it:
   - the outbox and the inbox channels are `Handling.loop` over `run`, which
     already finished its batch when told to stop: the cancel now waits for
     that, so it returns on a batch acknowledged and committed and a
     connection given back;
   - the in-memory broker admits each call under the lock it detaches
     handlers under, so a handler is either waited for or not called, and
     detaches only its own subscription's handler;
   - the Kafka adapter stops between messages, never inside the handler: the
     fetch races with the order to stop, fetch first, and so does the pause
     between two tries of a handler that fails.
4. **The bus depends on Eio by name.** It did in fact: a handler is "a plain
   function on an Eio fiber". Promises and fiber-local marks need the
   library.

## Objections considered

1. *A handler that cancels its own subscription would wait for itself, for
   ever.* It would, without the mark. Before this change no call of `cancel`
   in this repository was inside a handler, but "unsubscribe after the first
   message" is an ordinary thing to write. The mark makes it work: a test
   has a handler cancel itself under the in-memory broker and under a loop
   of its own, and another shows that a call of one subscription does wait
   for another's.
2. *A cancel that waits can wait for ever: a handler that does not return
   holds it.* It can, and the caller is the one who knows how long is too
   long: it races the cancel with a timeout and then leaves its switch,
   which cancels the loop where it is. That path is the one ADR-0015 makes
   safe, and it is why this decision does not replace that one: graceful
   stopping is a request, and a request can go unanswered. What is refused
   is the opposite default, a cancel that returns while the handler still
   runs, because nothing can be built on it.
3. *It parts from the reference, where cancel returns at once.* In what is
   observed, not in the signature. The reference gets the waiting from its
   runtime, a task handle to await, which this port's handles never were; a
   port that kept "returns at once" would have to grow a second operation
   to wait with, and every caller that stops in good order would call both.
   The README of the bus names the difference.
4. *A cancel between the read of the handlers and the call could still let
   one call through after it returned.* In the in-memory broker the read and
   the call are apart, so the call is admitted a second time under the lock,
   only if the handler is still this subscription's: admitted before the
   detach, it is waited for; after, it is not made.
5. *A loop that is cancelled by its switch, or that never started, must not
   leave a cancel waiting.* A call is counted out when it returns, raises or
   is cancelled, under a lock that cannot itself be cancelled; a loop on a
   switch that is over is refused by Eio, and the call admitted for it is
   counted out before the exception goes on. Both are tested.

6. *The argument for all this is prose, and prose is what got the inbox's
   wait wrong once (ADR-0008).* So the protocol was modelled,
   `verify/tla/Cancel.tla`, and the model found a flaw the prose and the
   tests had passed: as first written, a cancel that found the detaching
   taken by another went on at once to wait for the calls. While the first
   canceller waited for the lock to detach under, the second found nothing
   in flight and returned, and the handler, still attached, was called
   after it. With the loop flavour there is no such window, its detaching
   takes no lock; with the in-memory broker there is. The fix is the wait in
   decision 1; the flaw stays in the model as a configuration TLC must
   refute, beside three more: admitting outside the lock, registering a
   waiter outside the lock of the count, which is the lost wake of ADR-0008
   in another place, and no mark of being inside a call.

## Consequences

- The protocol is model-checked: five configurations of `Cancel.tla`, in
  `check.sh`. There is no trace validation for it, which would take an
  observer on every call of every handler; the model checks the design, and
  tests hold the code to it, two of them written for the flaw above and
  failing on the code as it was.
- New tests: eleven of `Handling` and `Subscription`; three of the in-memory broker, the wait, a
  handler cancelling itself, an earlier subscription cancelled after a later
  one was made; one each of the outbox channel, the inbox channel and the
  Kafka adapter, where a cancel is shown to wait while the handler runs and
  what it returns on is read at once, with no polling: the batch
  acknowledged, the message marked. The client of Kafka offers no way to
  read a committed offset back, so there the commit stands on the order of
  the code and is not tested.
- Cancelling an earlier subscription of a group no longer takes the handler
  of a later one from the in-memory broker.
- A cancel of a Kafka subscription no longer cuts a handler short. A message
  whose handler keeps failing when the order to stop comes is left
  uncommitted and comes again when the group next reads from its committed
  offset, as it did before when the fiber was cancelled in it.
- An application stops in good order by cancelling its subscriptions, then
  leaving the loops' switch, then the pool's.
