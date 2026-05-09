# Transactional Inbox

Reliable ingestion of incoming integration messages with **idempotency**
and **causal consistency**.

The pattern: every message coming from outside (Kafka, HTTP webhook,
AMQP, etc.) is first persisted into a single staging table. The
business logic runs on rows from that table, never on the raw broker
input. This buys you:

- **Idempotency**. Duplicate deliveries are silently dropped by the
  primary-key uniqueness on
  `(tenant_id, stream_type, stream_id, stream_position)`. The receiver
  doesn't have to be idempotent — the inbox is.
- **Causal ordering**. A message can declare `causal_dependencies` —
  earlier messages whose processing must complete first. The dispatcher
  skips a message until its dependencies are stamped as processed.
- **Backpressure independence**. Receiving and processing are decoupled
  — slow business logic doesn't backpressure the broker; fast bursts
  don't overwhelm the receiver.

For the schema and design rationale see [`init.sql`](./init.sql).

---

## Quick start

```ocaml
open Ascetic_inbox

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let stdenv = (env :> Caqti_eio.stdenv) in
  let uri = Uri.of_string "postgresql://user:pass@localhost/app" in
  let conn =
    match Caqti_eio_unix.connect ~sw ~stdenv uri with
    | Ok c -> c
    | Error e -> failwith (Format.asprintf "%a" Caqti_error.pp e)
  in

  let provider =
    Ascetic_unit_of_work.Caqti_connection_provider.of_connection conn
  in
  let inbox = Inbox.create ~provider () in

  let uow = Ascetic_unit_of_work.Caqti_unit_of_work.of_connection conn in
  match Inbox.setup inbox uow with
  | Error e -> failwith e
  | Ok () -> ()
```

The inbox shares the same {!Connection_provider} abstraction as the
outbox — see the [outbox README](../outbox/README.md#connection-providers)
for the Caqti-pool setup recipe (you need it once `concurrency > 1`).

---

## Publishing (ingestion)

`Inbox.publish` writes a row in its own transaction (separate from the
caller's UoW — the broker side has no business state to commit
together with). Duplicate primary keys are dropped silently.

```ocaml
let msg =
  Inbox_message.make
    ~tenant_id:"tenant-1"
    ~stream_type:"Order"
    ~stream_id:(`Assoc [ ("id", `String "order-42") ])
    ~stream_position:1
    ~uri:"webhook://orders.example.com"
    ~payload:(`Assoc [ ("type", `String "OrderCreated") ])
    ~metadata:
      (`Assoc
        [ ("event_id", `String "550e8400-e29b-41d4-a716-446655440000") ])
    ()
in
match Inbox.publish inbox msg with
| Ok () -> ()
| Error e -> Logs.err (fun m -> m "publish failed: %s" e)
```

`stream_id` is `Yojson.Safe.t` so it can be a primitive
(`` `String "abc" ``, `` `Int 42 ``) or composite
(`` `Assoc [ ... ] ``).

---

## Causal dependencies

A later message can declare that earlier messages must already be
processed before it becomes eligible. The dispatcher checks dependencies
on every fetch; messages whose dependencies are unmet are simply skipped
until they are.

```ocaml
let parent =
  Causal_dependency.make
    ~tenant_id:"tenant-1"
    ~stream_type:"Order"
    ~stream_id:(`Assoc [ ("id", `String "order-42") ])
    ~stream_position:1
in
let dependent =
  Inbox_message.make
    ~tenant_id:"tenant-1"
    ~stream_type:"Order"
    ~stream_id:(`Assoc [ ("id", `String "order-42") ])
    ~stream_position:2
    ~uri:"webhook://shipments.example.com"
    ~payload:(`Assoc [ ("type", `String "OrderShipped") ])
    ~metadata:
      (`Assoc
        [
          ( "causal_dependencies",
            `List [ Causal_dependency.to_json parent ] );
        ])
    ()
in
Inbox.publish inbox dependent
```

If `OrderShipped` arrives at the inbox before `OrderCreated`, the
dispatcher leaves it alone until `OrderCreated` is stamped processed —
then picks it up on the next poll cycle.

---

## Consuming: three flavours

The dispatcher runs the subscriber **inside the same database
transaction** as the row read. This means a subscriber can write
business state in the same UoW; if it returns `Error`, both the business
write and the `processed_position` stamp roll back, and the message is
retried.

### `Iter.iter` (streaming, recommended)

```ocaml
Inbox.Iter.iter
  ~clock:(Eio.Stdenv.mono_clock env)
  inbox
  (fun uow msg ->
    match msg.uri with
    | "webhook://orders.example.com" -> handle_order uow msg
    | "webhook://shipments.example.com" -> handle_shipment uow msg
    | _ -> Logs.warn (fun m -> m "unknown uri: %s" msg.uri))
```

### `Inbox.dispatch` (single-batch)

```ocaml
match Inbox.dispatch inbox subscriber with
| Ok true  -> (* one message processed *)
| Ok false -> (* nothing eligible right now *)
| Error e  -> Logs.err (fun m -> m "%s" e)
```

### `Inbox.run` (long-running daemon with concurrency)

```ocaml
Inbox.run
  ~clock:(Eio.Stdenv.mono_clock env)
  ~concurrency:3                       (* requires a pool provider *)
  ~poll_interval:0.5
  ~stop:(fun () -> !stop_flag)
  inbox
  subscriber
```

---

## Real example: HTTP webhook ingestion + business processing

Two-fiber setup: one accepts HTTP POSTs and stuffs them into the inbox,
the other processes the inbox and writes domain state.

```ocaml
open Ascetic_inbox

(* ─── HTTP server: receive webhooks, publish to inbox ─────────────── *)

let handle_webhook inbox req body =
  let payload = Yojson.Safe.from_string body in
  let event_id =
    match payload with
    | `Assoc fs -> (
        match List.assoc_opt "event_id" fs with
        | Some (`String s) -> s
        | _ -> Uuidm.to_string (Uuidm.v `V4))
    | _ -> Uuidm.to_string (Uuidm.v `V4)
  in
  let order_id = req_query req "order_id" in
  let position = int_of_string (req_query req "position") in
  let msg =
    Inbox_message.make
      ~tenant_id:"default"
      ~stream_type:"Order"
      ~stream_id:(`Assoc [ ("id", `String order_id) ])
      ~stream_position:position
      ~uri:(Printf.sprintf "webhook://orders/%s" order_id)
      ~payload
      ~metadata:(`Assoc [ ("event_id", `String event_id) ])
      ()
  in
  match Inbox.publish inbox msg with
  | Ok () -> respond_status req 202   (* Accepted *)
  | Error _ -> respond_status req 500

(* ─── Worker: process inbox messages, write domain state ──────────── *)

let process_order (uow : Ascetic_unit_of_work.Caqti_unit_of_work.t)
    (msg : Inbox_message.t) =
  (* Subscriber runs inside the inbox transaction.
     Any side effect on [uow] is committed atomically with the
     processed_position stamp. Returning Error rolls back BOTH. *)
  match msg.payload with
  | `Assoc fs -> (
      match List.assoc_opt "type" fs with
      | Some (`String "OrderCreated") -> Order_repo.save uow msg
      | Some (`String "OrderShipped") -> Order_repo.mark_shipped uow msg
      | _ -> Ok ())
  | _ -> Ok ()

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->

  let stdenv = (env :> Caqti_eio.stdenv) in
  let db_uri = Uri.of_string (Sys.getenv "DATABASE_URL") in
  let pool =
    match Caqti_eio_unix.connect_pool ~sw ~stdenv db_uri with
    | Ok p -> p
    | Error e -> failwith (Format.asprintf "%a" Caqti_error.pp e)
  in
  let provider = provider_of_pool pool in    (* same wrapper as outbox *)
  let inbox = Inbox.create ~provider () in

  Eio.Fiber.both
    (fun () ->
      (* HTTP listener — see cohttp-eio docs for details *)
      run_http_server ~sw ~env ~handler:(handle_webhook inbox))
    (fun () ->
      Inbox.Iter.iter
        ~clock:(Eio.Stdenv.mono_clock env)
        inbox
        (fun uow msg ->
          match process_order uow msg with
          | Ok () -> ()
          | Error e -> Logs.err (fun m -> m "process: %s" e)))
```

The HTTP path commits to the inbox **immediately** — no business logic
runs synchronously with the webhook. The worker fiber processes messages
on its own schedule, and writes that fail are rolled back and retried.

The same shape works for any source — Kafka consumer, AMQP, NATS — the
only difference is what code calls `Inbox.publish`.

---

## Partitioning strategies

When `concurrency > 1`, the dispatcher needs to know how to spread
messages across workers. Two built-in strategies:

### `Partition_strategy.uri` (default)

Hash by `uri`. All messages with the same URI land on the same worker.
Use when ordering follows broker partitions
(`kafka://orders/order-42` always goes to the same worker).

```ocaml
let inbox =
  Inbox.create ~provider ~partition:Partition_strategy.uri ()
```

### `Partition_strategy.stream`

Hash by stream identity. All messages for the same
`(tenant_id, stream_type, stream_id)` land on the same worker.
Use when causal ordering is a property of the stream itself (most
common for aggregate-event streams):

```ocaml
let inbox =
  Inbox.create ~provider ~partition:Partition_strategy.stream ()
```

### Custom strategy

```ocaml
module Tenant_partition : Partition_strategy.S = struct
  let sql_expression = "tenant_id"
end
let inbox =
  Inbox.create ~provider ~partition:(module Tenant_partition) ()
```

The expression must be valid Postgres SQL evaluated against inbox
columns — it's plugged directly into
`hashtext(<expression>) %% N = worker_id`.

**Always pair `concurrency > 1` with a pool-based provider** — Caqti
fails loudly if multiple fibers share one connection.

---

## Idempotency tiers

Two layers of dedup are active:

1. **Primary key**
   `(tenant_id, stream_type, stream_id, stream_position)` —
   `INSERT ... ON CONFLICT DO NOTHING` silently drops duplicates. This
   is the canonical idempotency boundary; design your stream-position
   to be deterministic from the source.
2. **`metadata->>'event_id'` UNIQUE INDEX** — catches accidental
   duplicates where the same logical event reaches you with a different
   `(stream_type, stream_id, stream_position)` triple. Insert fails
   loudly with a unique-violation error; treat that as a sign of
   misconfiguration rather than expected duplication.

`event_id` must be a valid UUID — the index casts via
`((metadata->>'event_id')::uuid)`.

---

## Operational notes

- **Cleanup**: processed messages stay in the inbox table. Run a
  periodic job to delete rows where `processed_position` is set and
  older than your retention window.
- **Stuck messages**: a message whose dependencies never satisfy will
  be re-tested on every dispatch — cheap, but it does count against
  the offset scan. For long-stuck dependencies, add monitoring on
  `MAX(received_position) - MAX(processed_position)`.
- **At-least-once contract**: a subscriber that fails halfway through
  side-effecting external systems will see the message again. Either
  make the side-effects idempotent, or guard via the inbox itself
  (e.g. write a follow-up message in the same transaction so the
  next dispatch can detect "already started").
