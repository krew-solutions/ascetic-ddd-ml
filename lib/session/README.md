# Session

Unit of Work as a session: an opaque handle with one operation, `atomic`,
that runs a scope inside a transaction. Nested scopes are savepoints. The
application layer sees the handle and `atomic`; everything the
infrastructure needs, the connection, the depth, the observer, lives in the
concrete module and is reachable only where the type is known.

Three libraries: `ascetic_ddd.session` is the port and the scope algorithm,
with no database behind it and Eio as its only dependency;
`ascetic_ddd.session.caqti` is PostgreSQL through Caqti;
`ascetic_ddd.session.memory` is the journal-recording session for tests.
Application code and its tests link the first two of those, never the
driver.

This library stands beside `ascetic_ddd.unit_of_work`, which the outbox and
the inbox still use; it is the successor, and the older one stays for
compatibility.

---

## Quick start

```ocaml
open Ascetic_session
open Ascetic_session_caqti

(* The application's error type: its own cases plus one for the session
   machinery, filled by [lift]. *)
type error = Session of Session_error.t | Order_not_found

let atomic = Caqti_session.atomic ~lift:(fun e -> Session e)

let place_order pool order =
  Caqti_session_pool.session pool ~lift:(fun e -> Session e) (fun session ->
      atomic session (fun session ->                    (* BEGIN *)
          let* () = Orders.save session order in
          let* () =
            atomic session (fun session ->              (* SAVEPOINT sp1 *)
                Outbox.publish session (Order_placed order))
          in                                            (* RELEASE SAVEPOINT sp1 *)
          Ok ()))                                       (* COMMIT *)
```

Wiring, at the composition root:

```ocaml
let pool =
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok pool -> Caqti_session_pool.of_pool pool
  | Error e -> failwith (Format.asprintf "%a" Caqti_error.pp e)
```

A repository in the infrastructure layer pins the session type and reaches
the connection through the capability:

```ocaml
module Orders_pg : Orders.S with type uow = Caqti_session.t = struct
  type uow = Caqti_session.t

  let save session order =
    let module C = (val Caqti_session.connection session : Caqti_eio.CONNECTION) in
    ...
end
```

---

## What each layer sees

| Layer | Sees |
|---|---|
| domain, application | an abstract `t` and `atomic`, through `Session.S` |
| infrastructure | `Caqti_session.connection`, `depth`, `is_abandoned`, the observer, the pool; the `caqti` sub-library |
| composition root | the concrete modules, and wires both sides |

Application code is polymorphic in the session type, so it cannot reach the
connection even by accident: the compiler refuses it. This is the module-system
form of "the repository asks for a capability, the domain never names it".

---

## Errors

A scope returns its own error type. The session only asks how to carry a
`Session_error.t` in it, through `lift`, because opening or closing a scope
can fail on its own. Apply `lift` once per use case, as in the quick start.

| `Session_error.t` | when |
|---|---|
| `Acquire` | no connection could be taken from the pool |
| `Begin` | `BEGIN` or `SAVEPOINT` failed |
| `Commit` | `COMMIT` or `RELEASE SAVEPOINT` failed: the caller believes the work is durable and it is not |
| `Scope_already_open` | a second scope was opened on the session that already has one open |
| `Abandoned` | a rollback failed or was cut short; see below |

A scope that returns `Error` is rolled back and the error comes back as is.
A scope that raises is rolled back and the exception is re-raised. A failing
rollback never replaces the error that caused it.

## Nesting and the guard

The scope receives a session of its own, one level deeper. Opening a scope
on *that* session is a savepoint; opening one on the session that opened
the current scope is refused with `Scope_already_open`, because two scopes
side by side on one connection would share one savepoint stack, and
releasing the older one silently destroys the newer. Sequential scopes on
one session are fine. Savepoint names come from a counter shared by the
whole session tree, not from the depth.

## Cancellation and abandonment

Eio cancels a fiber by raising at its next suspension point, and the
rollback is itself a suspension point. The session rolls back under
`Eio.Cancel.protect`, so a scope cut short by a timeout or by the losing
branch of `Fiber.first` still leaves the connection clean and usable.

A cancellation that lands inside a driver call rather than between two
is different: Caqti's own guard against concurrent use stays set, and the
connection cannot be used again, not even to roll back. That is the case
the abandonment below exists for.

Only a rollback that fails, or is itself cut short, abandons the session:
the transaction's state is unknown, so every further scope on it is
refused, an enclosing scope is refused at commit rather than committing
half-done work, and the connection is disconnected so that a pool drops it
instead of handing it to the next request inside a stale transaction. A
`BEGIN`, `COMMIT` or `RELEASE` that raises instead of returning abandons
the session the same way, because whether it took effect is unknown.

## Observers

`Session_observer.t` is a record of two functions, called when a scope
starts and ends, with the depth, the kind (`Session`, `Transaction`,
`Savepoint`) and the outcome. `none` observes nothing; `all` composes a
list into one. Observers are synchronous and must not raise: they run on the
completion path of every transaction. One that has to do I/O hands the event
to a queue and lets a fiber of its own do the waiting.

## Testing without a database

`Memory_session` records the statements of its scopes into a journal, and a
repository double records its own through `Memory_session.record`, so a test
asserts on the exact sequence:

```ocaml
open Ascetic_session_memory

let pool = Memory_session_pool.create () in
let _ = Memory_session_pool.session pool ~lift (fun session -> place_order session order) in
assert (Memory_session.Journal.entries (Memory_session_pool.journal pool)
        = [ "BEGIN"; "INSERT INTO orders ..."; "SAVEPOINT sp1"; "INSERT INTO outbox ...";
            "RELEASE SAVEPOINT sp1"; "COMMIT" ])
```

`?fail` makes a chosen statement fail, to exercise the paths a real database
takes rarely: a failed commit, a failed rollback. The in-memory session runs
the same scope algorithm as the PostgreSQL one, over another backend, so
what a journal test proves about nesting, failure paths, the guard and
cancellation holds for both; `test/session/test_pg.ml` checks the backend
itself against a live database.

Sessions run inside an Eio fiber, the in-memory one included, because the
rollback is protected from cancellation.

## PostgreSQL

`Caqti_session` issues the outermost scope through the driver's `start`,
`commit` and `rollback`, and savepoints as one-shot statements. A Caqti
connection serves one fiber at a time, so work inside a scope is
sequential; concurrency comes from the pool, one session per fiber.

The identity map of the Python and Rust ports is deliberately not here:
with immutable aggregates, two loads of one row are two equal values with
nothing to share, and conflicting saves are caught by version checks. A
per-transaction read cache can be added if a measurement asks for it. See
ADR-0003.
