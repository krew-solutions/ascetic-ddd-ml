# Session

Unit of Work as a session: an opaque handle with one operation, `atomic`,
that runs a scope inside a transaction. Nested scopes are savepoints. The
application layer sees the handle and `atomic`; everything the
infrastructure needs, the connection, the depth, the observer, lives in the
concrete module and is reachable only where the type is known.

Five libraries: `ascetic_ddd.session` is the port and the scope algorithm,
with no database behind it and Eio as its only dependency;
`ascetic_ddd.session.caqti` is PostgreSQL through Caqti;
`ascetic_ddd.session.memory` is the journal-recording session for tests;
`ascetic_ddd.session.composite` makes two sessions act as one;
`ascetic_ddd.session.rest` is the session over an HTTP client. Application
code and its tests never link the driver.

This library is the successor of `ascetic_ddd.unit_of_work`, which stays for
compatibility; the outbox, the inbox, the key management service and the
store of data-encryption keys are written over the session.

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
refused, and an enclosing scope is refused at commit rather than committing
half-done work, rolling back on its way out all the same. A connection
still alive therefore comes back with no transaction open on it; one whose
server is gone fails the pool's check and is dropped. A `BEGIN`, `COMMIT`
or `RELEASE` that raises instead of returning abandons the session the same
way, because whether it took effect is unknown; a failure the PostgreSQL
client library raises rather than returns, a connection lost under a
statement, is turned into the scope's error first, so that a scope unwinds
with errors and only a cancellation goes through as an exception.

## Observers

`Session_observer.t` is a record of two functions, called when a scope
starts and ends, with the depth, the kind (`Session`, `Transaction`,
`Savepoint`, or `Logical` for a scope with no transaction behind it) and
the outcome. `none` observes nothing; `all` composes a
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

## Composite sessions

A use case that must write to two stores at once, a data generator's
target database and its own bookkeeping, say, is written against one
session as usual; that the session is a pair is a decision of the
composition root:

```ocaml
open Ascetic_session_composite

module Session = Composite_session.Make (Caqti_session) (Caqti_session)
module Pool = Composite_session_pool.Make (Caqti_session_pool) (Caqti_session_pool)

let generate pool batch =
  Pool.session pool ~lift (fun session ->
      Session.atomic session ~lift (fun (bookkeeping, target) ->
          let* rows = Distribution.next bookkeeping batch in
          Target.insert target rows))
```

A scope on the composite opens a scope on each delegate, the first one
outermost: first open, second open, work, second close, first close.
More than two delegates nest, `Make (A) (Make (B) (C))`, and the handle
`A.t * (B.t * C.t)` is taken apart by the pattern `(a, (b, c))`. A
repository pins `type uow` to the product and reaches its delegate by
position, `fst uow`; the choice is explicit and the application layer
never sees it.

It is not a distributed transaction. The inner delegate commits first; if
the outer one then fails to commit, the two diverge, and no composition
can prevent that. Work that must be undone across stores belongs in a
saga. What the composite guarantees is one failure path: an error inside
the scope, or the inner delegate failing to commit, rolls the outer one
back. So the delegate whose rollback must undo the other's work goes
first: for the generator, the bookkeeping, so that a target that fails to
commit leaves the distribution counters untouched.

Of the three shapes the Rust port keeps, pair, tuple and a run-time list,
only the pair is here: the tuple exists there to avoid method chains that
OCaml's patterns do not have, and the list waits for shards.

## REST sessions

A REST session is the same shape with no transaction behind it: a scope
groups work and reports itself, as `Logical`, and does not pretend that
HTTP calls can be rolled back. The HTTP client is a type parameter,
`'client Rest_session.t`, so the library depends on no HTTP library. A
request is timed by wrapping the call that makes it, which is why any
client works and nothing is hidden:

```ocaml
open Ascetic_session_rest

let fetch_customer session id =
  let url = Printf.sprintf "https://crm.example/customers/%d" id in
  Rest_session.request session ~meth:"GET" ~url (fun () ->
      Http_client.get (Rest_session.http session) url)
```

The client and `request` are the capability "this session speaks HTTP": a
gateway asks for a `'client Rest_session.t` where the client's type is
known, which is the infrastructure layer. Code polymorphic in the session
is given the port, `Rest_session.Of (Client)`, a `Session.S`, and the pool
likewise, `Rest_session_pool.Of (Client)`. One client serves every session:
an HTTP client is itself the pool of its connections.

`Rest_observer.t` carries a `Session_observer.t` for the scopes and two
signals for the requests, started and ended, the latter with the time the
call took, in seconds, and whether it failed; a call that raises is
reported as failed and the exception goes on. The guard against two scopes
side by side is the same as for every session. The key management service
over HashiCorp Vault, `ascetic_ddd.kms.vault`, is written over this
session.

Of what the Rust port's REST session has, the identity map is not here,
as for every session of this library and for the reason given below.

## PostgreSQL

`Caqti_session` issues the outermost scope through the driver's `start`,
`commit` and `rollback`, and savepoints as one-shot statements. A Caqti
connection serves one fiber at a time, so work inside a scope is
sequential; concurrency comes from the pool, one session per fiber.

The pool must outlive every fiber that takes a session from it (ADR-0015).
Caqti drains a pool when the switch it was connected on ends, and waits for
every connection to come back. A daemon fiber cancelled by that same switch
with a connection in hand cannot give it back any more, so the switch never
ends and the process hangs on its way out. A loop that runs as a daemon, a
dispatcher of the outbox, the processing of the inbox, therefore goes on a
switch of its own inside the pool's:

```ocaml
Eio.Switch.run @@ fun pool_sw ->
let pool = Caqti_session_pool.of_pool (connect_pool ~sw:pool_sw uri) in
Eio.Switch.run @@ fun loops_sw ->
let bus = register ~sw:loops_sw pool in
serve bus
```

When the inner switch ends, however it ends, the loops are cancelled where
they are, what they were doing is rolled back, their connections return to
a pool that is still there, and then the pool is drained with nothing out. A
loop cancelled in the middle of a statement ends when the statement does on
the server: the rollback is protected from the cancellation and goes after
it.

Beside the session, the Caqti library carries what every PostgreSQL adapter
over it needs: `Identifier`, a table or sequence name known safe to splice
into SQL, lower-case letters, digits and underscores, at most forty
characters; and `Transient`, which errors of the database are of the
moment, told by SQLSTATE class from the driver's error, so that a loop
meeting one waits and goes on rather than stopping (ADR-0009). Its verdict
travels in `Driver_error.t`, the text the driver rendered and whether the
failure is of the moment, which `Session_error.t` carries in `Acquire`,
`Begin`, `Commit` and `Abandoned`, and `Session_error.is_transient` reads.

The identity map of the Python and Rust ports is deliberately not here:
with immutable aggregates, two loads of one row are two equal values with
nothing to share, and conflicting saves are caught by version checks. A
per-transaction read cache can be added if a measurement asks for it. See
ADR-0003.
