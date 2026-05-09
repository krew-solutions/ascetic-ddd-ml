# Transactional Outbox

Reliable message publishing for systems that need to update a database
**and** publish to external systems atomically.

The pattern: business state changes and outgoing messages are written
to the same Postgres transaction. A separate dispatcher reads committed
messages and forwards them to the broker / HTTP endpoint / whatever.
Either both happen or neither does — no orphan messages, no orphan state.

For the schema and design rationale (xid8 ordering, visibility rules,
consumer groups, URI-based partitioning) see [`init.sql`](./init.sql).

---

## Quick start

```ocaml
open Ascetic_outbox

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  (* 1. Open a connection (or a pool — see below). *)
  let stdenv = (env :> Caqti_eio.stdenv) in
  let uri = Uri.of_string "postgresql://user:pass@localhost/app" in
  let conn =
    match Caqti_eio_unix.connect ~sw ~stdenv uri with
    | Ok c -> c
    | Error e -> failwith (Format.asprintf "%a" Caqti_error.pp e)
  in

  (* 2. Build the outbox. *)
  let provider = Connection_provider.of_connection conn in
  let outbox = Outbox.create ~provider () in

  (* 3. Make sure the schema exists (idempotent). *)
  let uow = Ascetic_unit_of_work.Caqti_unit_of_work.of_connection conn in
  match Outbox.setup outbox uow with
  | Error e -> failwith e
  | Ok () -> ()
```

`Outbox.publish` adds a message inside the caller's unit of work;
`Outbox.dispatch` / `run` / `Iter` read committed messages back out.

---

## Connection providers

The dispatcher needs to acquire its own connections (separately from
the publisher's UoW), so the outbox holds a `Connection_provider.t`
rather than a single connection. Two common shapes:

### Single connection (development, tests)

```ocaml
let provider = Connection_provider.of_connection conn
```

Fine when there is exactly one fiber driving the dispatcher. **Will
fail at runtime** with `Invalid concurrent usage of PostgreSQL
connection detected` if you set `concurrency > 1` — Caqti rejects
shared use of a single connection from multiple fibers.

### Caqti pool (production, `concurrency > 1`)

`Caqti_eio.Pool.use` constrains its callback's error type to
`[> Caqti_error.t]`, but we want to surface our own `(_, string) result`.
The cleanest way is to smuggle the error through an exception:

```ocaml
let provider_of_pool pool : Connection_provider.t =
  (module struct
    exception Outbox_error of string

    let with_connection f =
      try
        Caqti_eio.Pool.use
          (fun conn ->
            match f conn with
            | Ok v -> Ok v
            | Error msg -> raise (Outbox_error msg))
          pool
        |> Result.map_error (fun e ->
               Format.asprintf "%a" Caqti_error.pp e)
      with Outbox_error msg -> Error msg
  end)

(* Wire it: *)
let pool =
  match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
  | Ok p -> p
  | Error e -> failwith (Format.asprintf "%a" Caqti_error.pp e)
in
let outbox = Outbox.create ~provider:(provider_of_pool pool) ()
```

Pool size defaults to a small number; raise it via
`Caqti_pool_config` when you need `concurrency > 1`. Plan for at least
`concurrency + 1` connections — each dispatcher fiber holds one for
its batch, and `ensure_consumer_group` briefly takes another.

---

## Publishing in a unit of work

The whole point of the pattern is atomicity with business state, so
`publish` must run inside the same transaction as everything else.
Sketch with a hand-rolled UoW:

```ocaml
module Uow = Ascetic_unit_of_work.Caqti_unit_of_work

let create_order outbox conn order =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let uow = Uow.of_connection conn in
  match C.start () with
  | Error e -> Error (Format.asprintf "%a" Caqti_error.pp e)
  | Ok () ->
      let result =
        let open Result in
        let* () = save_order_row conn order in   (* your repository *)
        Outbox.publish outbox uow
          (Outbox_message.make
             ~uri:"webhook://order-created"
             ~payload:(`Assoc [ ("order_id", `String order.id) ])
             ~metadata:(`Assoc [ ("event_id", `String (Uuidm.to_string (Uuidm.v `V4))) ])
             ())
      in
      match result with
      | Error e -> Uow.rollback uow; Error e
      | Ok () -> Uow.commit uow
```

The message is invisible to dispatchers until commit — readers filter
on `transaction_id < pg_snapshot_xmin(pg_current_snapshot())`.

---

## Consuming: three flavours

### `Iter.iter` (streaming, recommended)

Simplest for "process each message and move on":

```ocaml
let subscriber (msg : Outbox_message.t) =
  Logs.info (fun m -> m "got %s" msg.uri);
  (* do work; raise / Error → message redelivered *)
in
Outbox.Iter.iter
  ~clock:(Eio.Stdenv.mono_clock env)
  ~consumer_group:"my-service"
  outbox
  subscriber
```

Backed by OCaml 5 effect handlers; each batch is fetched in one
transaction and acks happen per message. Keep the subscriber fast — the
batch transaction stays open for the whole batch.

### `Outbox.dispatch` (manual single-batch)

Useful for cron-style dispatchers or test code:

```ocaml
match Outbox.dispatch outbox subscriber with
| Ok true  -> (* processed at least one message *)
| Ok false -> (* nothing pending *)
| Error e  -> Logs.err (fun m -> m "%s" e)
```

### `Outbox.run` (callback loop with concurrency)

Long-running daemon with optional fan-out:

```ocaml
let stop = ref false in
Outbox.run
  ~clock:(Eio.Stdenv.mono_clock env)
  ~consumer_group:"my-service"
  ~concurrency:3                       (* requires a pool provider! *)
  ~poll_interval:0.5
  ~stop:(fun () -> !stop)
  outbox
  subscriber
```

When `concurrency > 1`, work is partitioned by `hashtext(uri) %% N`,
so messages for the same URI always land on the same worker.

---

## Real example: HTTP webhook publisher

This is the dispatcher process. It reads outbox messages and POSTs
them to the URL embedded in the message URI.

```ocaml
open Ascetic_outbox

let post_webhook ~sw ~client (msg : Outbox_message.t) =
  (* uri is e.g. "webhook://hooks.example.com/orders" *)
  let path = String.sub msg.uri 10 (String.length msg.uri - 10) in
  let target = Uri.of_string (Printf.sprintf "https://%s" path) in
  let body =
    Cohttp_eio.Body.of_string (Yojson.Safe.to_string msg.payload)
  in
  let headers =
    Cohttp.Header.of_list
      [
        ("Content-Type", "application/json");
        ( "X-Event-Id",
          match msg.metadata with
          | `Assoc fs -> (
              match List.assoc_opt "event_id" fs with
              | Some (`String s) -> s
              | _ -> "")
          | _ -> "" );
      ]
  in
  let resp, body =
    Cohttp_eio.Client.post client ~sw ~headers ~body target
  in
  let _ = Eio.Buf_read.(of_flow ~max_size:1_000_000 body |> take_all) in
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if Cohttp.Code.is_success code then Ok ()
  else Error (Printf.sprintf "HTTP %d for %s" code (Uri.to_string target))

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let stdenv = (env :> Caqti_eio.stdenv) in
  let db_uri = Uri.of_string (Sys.getenv "DATABASE_URL") in
  let pool =
    Caqti_eio_unix.connect_pool ~sw ~stdenv db_uri
    |> Result.fold ~ok:Fun.id ~error:(fun e ->
           failwith (Format.asprintf "%a" Caqti_error.pp e))
  in
  let outbox =
    Outbox.create ~provider:(provider_of_pool pool) ()
  in
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in

  Outbox.Iter.iter
    ~clock:(Eio.Stdenv.mono_clock env)
    ~consumer_group:"webhook-publisher"
    ~uri:"webhook://"     (* only handle webhook:// URIs *)
    outbox
    (fun msg ->
      match post_webhook ~sw ~client msg with
      | Ok () -> ()
      | Error e ->
          Logs.warn (fun m -> m "delivery failed: %s" e);
          (* exception propagation here would close the iterator;
             swallowing means the message is acked despite failure.
             For at-least-once delivery, raise instead. *)
          ())
```

Same shape works for any broker:

- **Kafka**: replace the HTTP client with your Kafka client; route by
  URI prefix (`kafka://orders` → topic `orders`).
- **AMQP**: parse `amqp://exchange/routing-key` from the URI.
- **NATS**: subject = `String.sub msg.uri 7 ...` from `nats://subject`.

The dispatcher is broker-agnostic — only the subscriber callback knows
which broker is involved.

---

## Graceful shutdown

`run` and `Iter.iter` accept a `?stop : unit -> bool` predicate. Wire
it to a signal handler:

```ocaml
let stop_flag = ref false in
let handler _ = stop_flag := true in
Sys.set_signal Sys.sigint  (Sys.Signal_handle handler);
Sys.set_signal Sys.sigterm (Sys.Signal_handle handler);

Outbox.run
  ~clock:(Eio.Stdenv.mono_clock env)
  ~stop:(fun () -> !stop_flag)
  outbox
  subscriber
```

`stop` is checked between batches; the in-flight batch always finishes.
For hard cancellation use `Iter.close` (rolls back the open
transaction).

---

## Retry semantics

Returning `Error _` from a subscriber rolls back the dispatcher
transaction — the messages of that batch stay in the outbox and will
be redelivered on the next `dispatch`. There is no per-message dead
letter queue: handle non-recoverable errors inside the subscriber
(e.g. log + return `Ok` to ack and skip).

```ocaml
let subscriber msg =
  match try_publish msg with
  | Ok () -> Ok ()
  | Error e when is_transient e ->
      (* roll back the batch — try again later *)
      Error (Printf.sprintf "transient: %s" e)
  | Error e ->
      (* poison message: log and ack so we don't loop forever *)
      Logs.err (fun m -> m "drop %s: %s" msg.uri e);
      Ok ()
```

---

## Multi-worker partitioning

When `concurrency > 1`, the framework runs that many fibers, each
processing a distinct partition:

| `process_id` | `concurrency` | `worker_id` (per fiber) |
|:-:|:-:|:-:|
| 0 | 3 | 0, 1, 2 |
| 1 | 3 | 3, 4, 5 |

Messages are routed by `hashtext(uri) %% N = worker_id`, so all
messages for a given URI land on the same worker — order is preserved
within a URI even under fan-out.

For multi-process deployment (e.g. Kubernetes replicas), set
`process_id` and `num_processes` to the replica index and replica
count. Each replica then fans out to `concurrency` fibers internally.

**Always pair `concurrency > 1` with a pool-based provider** — Caqti
fails loudly if multiple fibers share one connection.

---

## Operational notes

- **Backpressure**: `batch_size` (default 100) bounds how many messages
  a single dispatcher transaction holds. Larger batch = less SQL
  overhead, longer transaction window — see `init.sql` for the
  trade-offs around xmin horizon.
- **Cleanup**: processed messages stay in the outbox table. Run a
  periodic job to delete rows below `MIN(last_processed_transaction_id)`
  across all consumer groups. SQL example in `init.sql`.
- **Idempotency**: the `metadata->>'event_id'` UNIQUE INDEX prevents
  accidental duplicate publishes within the producer. Consumers must
  still be idempotent — at-least-once delivery is part of the contract.
