# Outbox

Transactional Outbox on PostgreSQL: a message is committed in the same
transaction as the state change it announces, and a dispatcher sends what
was committed, in order, at least once. A port of the Rust reference
implementation of these building blocks, over the session of
`ascetic_ddd.session.caqti`.

```ocaml
open Ascetic_outbox

(* in the command handler, inside the transaction *)
let place pool order =
  Caqti_session_pool.session pool ~lift (fun session ->
      Caqti_session.atomic session ~lift (fun tx ->
          let* () = Orders.save tx order in
          Pg_outbox.publish outbox tx
            (Outbox_message.make ~uri:"kafka://orders" ~payload ~metadata)))

(* a dispatcher process *)
Pg_outbox.run outbox ~clock ~shutdown (Selection.group "broker") send_to_broker
```

## What it guarantees

* **Atomicity.** `publish` writes into the caller's transaction. Either the
  state change and the message are committed, or neither is.
* **Order.** Rows carry the inserting transaction's id (`xid8`) and are read
  only once that transaction is older than every running one, in
  `(transaction_id, position)` order. A serial alone cannot do this: a slow
  transaction commits after a fast one that took the next number.
* **At least once.** The subscriber runs inside the dispatcher's transaction
  and the position is acknowledged after the batch. A crash in between
  redelivers; consumers deduplicate on `metadata.message_id`, which is unique
  in the table.
* **One dispatcher at a time per slot.** A slot's position row is locked for
  the length of a batch, and a dispatcher is whoever holds that lock.
* **Slots.** `kafka://orders/order-7` names a key; the messages of one full
  URI are in one slot, `hashtext(uri) % slots`, stored with the row, and go
  out in order. One slot by default.

## The port and the adapter

The application layer sees `Outbox_port.S`, one operation, `publish`, over an
opaque `uow`, the session of the caller's transaction. A test double collects
messages. Everything else, `dispatch`, `run`, positions, `setup`, is on
`Pg_outbox`, because dispatching is the business of a separate process.

`Pg_outbox.t` is typed by the subscriber's error, `'e Pg_outbox.t`: the error
a subscriber returns reaches the caller of `dispatch` as
`Outbox_error.Subscriber e` and the observer as it is, so an observer of the
application can log or count its own errors. The adapter is an instance of
the port once that type is chosen, which the composition root does:

```ocaml
module App_outbox :
  Outbox_port.S with type t = App_error.t Pg_outbox.t and type uow = Caqti_session.t =
struct
  type t = App_error.t Pg_outbox.t
  type uow = Caqti_session.t

  let publish = Pg_outbox.publish
end
```

## As a channel of the bus

The outbox is also an adapter of `ascetic_ddd.bus` (ADR-0011),
`Outbox_channel`. Its producer is transactional, built once at the
composition root for a destination on another channel, and publishing takes
the session of the current transaction. Its consumer is the dispatcher: every
committed row reaches the handler as a wire message whose `destination`
header is the row's URI, and a `Bridge` to that header is the whole
dispatcher process.

```ocaml
let* bus = Bus.register Bus.empty ~scheme:Outbox_channel.scheme (Outbox_channel.adapter ~sw ~clock outbox) in
let* bus = Bus.register bus ~scheme:"kafka" kafka in

(* in the command handler, inside the transaction *)
let placed = Outbox_channel.producer outbox ~destination:"kafka://orders/order-7" ~encode:encode_order_placed in
Caqti_session.atomic session ~lift (fun tx -> Transactional.Producer.publish placed tx event)

(* the dispatcher process *)
let* dispatcher = Bridge.run (Bridge.create bus) ~from:"outbox://all" ~group:"dispatcher" (Bridge.Header "destination")
```

Headers travel as string fields of `metadata`, so `message_id` keeps its
unique index. The dispatcher is a daemon fiber on the switch the adapter was
given; cancel the subscription to stop it. `~loops` on the adapter says how
it runs, how many loops in this process, how long one waits, the same
`Loops.t` that `run` takes. On the bus the outbox is typed by the bus's
failure, `Ascetic_bus.Failure.t Pg_outbox.t`: that is the error a wire
handler returns.

## Errors

`'e Outbox_error.t` is a value the caller can act on: `Session` when the
session could not be opened or closed, `Database` when a statement was
refused or the connection failed under it, `Subscriber e` when the subscriber
declined a message, `Malformed` when a stored value could not be read back or
the table is cut into another number of slots. `Database` and `Session` carry
a `Driver_error.t`, the driver's text and whether the failure is of the
moment, so `Outbox_error.is_transient` tells a lock cycle the server broke or
a connection lost from a defect of the schema or of a statement; `run` waits
on the first and stops on the second (ADR-0009).

## Observing the outbox

`Pg_outbox.create ~observer` attaches an `'e Outbox_observer.t`, in the shape
of the session observer: a synchronous, infallible record of functions,
composed with `Outbox_observer.all`, fixed when the outbox is built. It is
told of five things: a message published, with the id of the writing
transaction and the row's position; a batch fetched, with the visibility
horizon of the statement that read it; a message handed to the subscriber,
with the outcome; the position acknowledged; the dispatcher's transaction
closed, committed or rolled back. These are the actions of the protocol
model in `verify/tla/Outbox.tla`, so a recording observer yields a trace the
model can be checked against. The tests do exactly that, through the
recorder of `ascetic_ddd.trace`: with `ASCETIC_DDD_TRACE_DIR` set they write
one JSON line per event, and `verify/tla/check.sh` replays those files through
the model.

## Slots

A row carries its slot, `hashtext(uri) % slots` with the sign bit cleared,
stored at insert; the number of slots is fixed for the life of the table,
`~slots` before `setup` creates it, one by default. Each
`(consumer_group, uri, slot)` keeps a position of its own. A dispatcher has
no identity: `dispatch outbox selection subscriber` takes whichever slot of
the selection has visible work and is held by nobody, least recently served
first, locks that slot's position for the length of the batch,
`FOR UPDATE SKIP LOCKED`, and moves it. Any number of loops in any number of
processes share a selection through the locks alone,
`run outbox ~clock ~loops:{ concurrency; poll_interval; max_pause } ~shutdown selection subscriber`;
a process that dies releases its slot with its transaction, and the next poll
of a survivor takes it. One slot is one position per group and the order of
the whole selection; more slots are parallelism, and order within a URI
still, since a URI is in one slot. Changing the number of slots moves rows
between positions and is a migration, not a restart: `setup` refuses a table
cut otherwise (ADR-0006).

The fetch is one statement. Measured in the reference implementation on
200,000 rows, with work at three quarters of the table: 0.06 ms for one
slot, 0.20 ms for sixteen, 0.26 ms for sixty-four; idle, 0.03, 0.10 and
0.11 ms.

## The loops

`run` dispatches until the `shutdown` promise is resolved, with
`concurrency` loops as fibers of the calling fiber; a loop that finds nothing
waits `poll_interval` seconds. Shutdown is cooperative: a loop finishes its
batch, commits, and only then stops. A loop whose subscriber failed, the
batch rolled back, to be delivered again, or that met an error of the moment
in the database, waits, longer with each failure in a row up to `max_pause`,
and goes on. A defect stops every loop and `run` returns it (ADR-0009). The
loops run on the Eio clock they are given; sessions run inside an Eio fiber.

## Logging

What must not be silent when no observer is attached goes to a `Logs` source
of the outbox's own, `Ascetic_outbox.Log.src`, named `ascetic_ddd.outbox`: a
loop that waits after a failure, the subscriber's or one of the moment in
the database, and a dispatcher on the bus that starts again after a defect.
The subscriber's own error is not printed there, its type being the
caller's; it reaches the observer as it is.

## What is not here

The outbox keeps every row it ever stored; nothing deletes acknowledged
messages. Retention is the deployment's: the table is meant to be rotated by
partitions, which is simpler and cheaper than a cleaner inside the library.
The fetch does not slow down with the history, because it reads from the
group's position on.

## Deviations from the reference implementation

* The transaction id, the positions and the slots are `int`: an OCaml `int`
  holds every `xid8` PostgreSQL will assign in practice, and a value it
  cannot hold is `Malformed` rather than wrong.
* Durations are seconds as `float`, the unit of `Eio.Time`; the shutdown is
  an `Eio.Promise.t` rather than a future.
* The outbox and its observer are typed by the subscriber's error, where the
  reference boxes it: OCaml has no universal error type, and the caller's
  own type is what a supervisor matches on (ADR-0002).
* `created_at` is a `Ptime.t`, not the database's text.

## Testing

```bash
docker compose up -d
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
dune test test/outbox
```
