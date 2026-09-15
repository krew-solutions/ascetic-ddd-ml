# ADR-0003: A session is an opaque handle with one operation

## Status

Accepted (2026-09-15). Implemented as `lib/session`, beside
`lib/unit_of_work`, which the outbox and the inbox keep using for now.

## Context

`Unit_of_work.S` exposed `commit` and `rollback` and no way to begin, so a
transaction boundary was either hand-rolled from the driver at the call
site, or absent: an application built on it ran every statement in
autocommit and a use case of several writes could half-succeed. The outbox
and the inbox each carried a private bracket of their own.

The goal is that the application and domain layers know nothing of the
session beyond an opaque handle, while the infrastructure has everything it
needs. The Rust and Python ports of these blocks solve it with a `Session`
trait of one method, `atomic`, and a capability (`PgAccess`, `IPgSession`)
that only repositories name.

## Decision

A port of one operation:

```ocaml
module type S = sig
  type t
  val atomic : t -> lift:(Session_error.t -> 'e) -> (t -> ('a, 'e) result) -> ('a, 'e) result
end
```

- The scope receives a child session one level deeper; a scope on it is a
  savepoint, a scope on the parent is refused at run time
  (`Scope_already_open`). Savepoint names come from a counter shared by
  the session tree.
- The scope chooses its error type; `lift` says how a session failure is
  carried in it, applied once per use case. `Session_error.t` names the
  phase that failed (`Acquire`, `Begin`, `Commit`) and carries the driver's
  text; it names no driver type.
- The rollback runs under `Eio.Cancel.protect`, so a cancelled scope still
  leaves the connection clean. A rollback that fails abandons the session:
  further scopes are refused, an enclosing scope is refused at commit, and
  the connection is disconnected so that a pool drops it.
- The scope algorithm is one functor over a backend of six statements; the
  PostgreSQL session and the in-memory session are its two instances, so a
  journal test proves the algorithm for both.
- Observers are a record of two functions with a neutral element and a
  composition; they are synchronous and must not raise.
- The connection is reachable only through `Caqti_session.connection`,
  which exists only in the concrete module: a repository that pins
  `type uow = Caqti_session.t` sees it, application code polymorphic in the
  session type cannot.
- A new library rather than a rewrite of `unit_of_work`, so that the
  outbox, the inbox and applications on the old port migrate one at a time.
- Three libraries, not one: `ascetic_ddd.session` holds the port and the
  scope algorithm with Eio as its only dependency; `.caqti` and `.memory`
  are the backends. Application code and its tests never link the driver,
  which is the dependency direction the port exists to enforce.

## Objections considered

1. *`lift` on every `atomic` is noise; polymorphic variants would compose
   session errors into the scope's without it, as Caqti's own errors do,
   and OCaml 5 effects would remove the session parameter altogether.*
   Polymorphic variants were rejected for the messaging ports in ADR-0002
   and the reasons hold: explicit mapping at the boundary is what
   Railway Oriented Programming prescribes, and one partial application per
   use case is the whole cost. Effects would make a repository called
   outside a transaction a run-time failure, where the explicit handle
   makes it a compile-time one; for a code base that puts verification
   first, the handle wins. Kept.

2. *The identity map of the other ports is missing, so two loads of one
   aggregate in a transaction are two instances.* They are two equal
   values: aggregates here are immutable, there is no shared mutation to
   protect, and conflicting saves are refused by version checks the
   repositories already do. What remains of the map is a read cache and a
   memory of absent rows; both are performance, to be added on a
   measurement, and neither needs weak references. Not ported.

3. *The run-time guard and the abandonment flag are the Rust design's
   answer to `&self` and to dropped futures; OCaml has neither problem.*
   OCaml has the first: nothing stops a scope from capturing the parent
   session and opening a second scope beside its own, and the savepoint
   stack breaks the same way. It has the second in another form:
   cancellation raises inside the scope, and the rollback it triggers is
   a suspension point that cancellation would cut short. `Cancel.protect`
   answers most of it, which the Rust design cannot do on drop, and the
   abandonment flag covers the rollback that fails anyway. Both kept, one
   of them smaller than in Rust. Writing the test showed a third form: a
   cancellation that lands inside a driver call leaves Caqti's own guard
   against concurrent use set, so the connection cannot be used again, not
   even to roll back. A `BEGIN` or `COMMIT` that raises therefore abandons
   the session, and the pool drops the connection.

## Consequences

- `Unit_of_work.S`, `Caqti_unit_of_work` and `Caqti_connection_provider`
  are unchanged; the outbox and the inbox still use them. Migrating them
  and the applications is a later, separate change.
- Sessions run inside an Eio fiber, the in-memory one included.
- REST sessions of the other ports are not here; they come with a
  consumer.

## Revisited (2026-09-15): composite sessions

The composite session is added as `ascetic_ddd.session.composite`, for
the data generator that writes to a target database and to its own
bookkeeping in one operation. The Rust port keeps three shapes, a named
pair, a tuple of up to eight and a run-time list, because Rust has no
variadic generics and reaches nested delegates through method chains.
Only the pair is ported: a functor `Make (A) (B)` whose handle is
`A.t * B.t`, nesting for more delegates with the pattern `(a, (b, c))`
as flat access, so the tuple has nothing to add; the run-time list waits
for a consumer with shards. The capability newtype the Rust port
requires is not needed either: a repository pins `type uow` to the
product and picks its delegate by position. The order of delegates is
part of the design: the one whose rollback must undo the other goes
first, since the inner delegate commits first and the composite is not a
distributed transaction.
