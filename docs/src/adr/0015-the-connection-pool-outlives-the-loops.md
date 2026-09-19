# ADR-0015: The connection pool outlives the loops

## Status

Accepted (2026-09-19).

## Context

The dispatcher of the outbox and the processing of the inbox run, as
channels of the bus, in daemon fibers on a switch the application gives
them. A loop holds a pooled connection most of the time: it is in a
transaction whenever it is not waiting. The pool is a Caqti pool, connected
on a switch too.

When a switch ends, Eio cancels its daemon fibers wherever they are. When
the switch a Caqti pool was connected on ends, Caqti drains the pool: it
closes the connections that are idle and waits for the ones in use to come
back. A connection comes back through a fiber Caqti forks on the pool's own
switch. If that switch is the one ending, the fiber no longer runs: the
connection a cancelled daemon had in hand never comes back, the drain waits
for ever, and the process hangs on its way out.

This was measured. A program of twenty lines with no code of this library,
a daemon fiber inside `Pool.use` in a statement when the body of the pool's
switch ends, hangs every time; with the pool on a switch outside the
daemon's it ends every time, and the pool still serves afterwards. In this
repository the fault showed as two test suites, the bridges of the outbox
and of the inbox, hanging under load, never alone: their fixtures had the
pool and the loops on one switch, and a test ended while a loop was in a
batch. Under the same load the outbox suite hung in 5 to 7 runs of 25, at
the commit before the work that met it as well as after. The reference
implementation has no such fault, its runtime and its pool draining nothing
on the way out; it is a matter of this port's libraries.

## Decision

1. **A pool outlives every fiber that takes a session from it.** A loop
   that runs as a daemon goes on a switch of its own inside the pool's. The
   rule is the application's to keep, at its composition root, and is stated
   where it is met: on `Caqti_session_pool.of_pool`, on
   `Outbox_channel.adapter`, on `Inbox_channel.consumer`, and in the three
   READMEs.
2. **The library's own fixtures keep it**: the two bridge suites connect
   their pool on a switch outside the one the loops run on.
3. **A test holds the property the rule stands on**: a daemon cancelled in
   the middle of a statement, on a switch inside the pool's, leaves its work
   rolled back and a pool that still serves.

## Objections considered

1. *A rule kept by documentation is broken sooner or later; the library
   should make the mistake impossible.* It cannot see it: a channel is given
   a switch and a pool that was connected elsewhere, and neither Eio nor
   Caqti tells which switch a pool is on. Making the loops ordinary fibers
   instead of daemons would trade the hang for a switch that cannot end
   until somebody stops every loop, which is the same hang with another
   cause. Taking the pool's construction into the library, so that it could
   nest the switches itself, would take from the application the choice of
   driver, of pool size and of TLS that Caqti gives it. The rule costs one
   line at the composition root, and the fault it prevents is loud and
   immediate, a process that does not exit, not a silent one.
2. *The fault is Caqti's, and belongs fixed there.* It may well be: giving a
   connection back on a switch that is ending cannot work. But a fix there
   is not ours to schedule, the installed version is what applications run
   today, and the rule is sound whatever Caqti does later: a resource
   outliving its users is how structured concurrency is meant to be laid
   out.
3. *Nesting hides a cost: the inner switch now waits for the loops.* It
   does, and that is the point. A loop cancelled in a statement ends when
   the statement has ended on the server, because the rollback is protected
   from the cancellation and goes after it; the test shows it, a second for
   a statement of a second. That bound is the database's statement, not
   ours, and an application that needs a harder one sets
   `statement_timeout`.
4. *The stress that showed the fault is not in the suite, so it can come
   back unseen.* A test of a hang is a test that hangs, and one that needs
   load is one that flakes. What is in the suite is the property itself, run
   every time; the measurements are recorded here.

## Consequences

- The two bridge suites ran 25 times each under the load that hung them
  before, with no hang.
- Stopping a loop before its switch ends, so that it finishes its batch
  rather than being cancelled in it, is a separate matter: the rule makes
  shutdown safe, not graceful. ADR-0016 is the graceful half.
