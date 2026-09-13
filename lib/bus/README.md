# Message Bus

Scheme-dispatched publish/subscribe over opaque wire payloads.

The pattern: application code names a destination by URI —
`in-memory://orders.placed`, `kafka://orders.placed` — and the bus
routes the call to whichever adapter was registered for that URI's
scheme. Swapping transports is a one-line change in the composition
root; producers and consumers never mention the transport.

The bus carries `string` payloads only. A producer says how to
serialize its values; each consumer says how to deserialize what it
reads. Two consumers on the same URI may decode the same bytes into
different OCaml types — the wire format is the contract, the types are
local to each side. This is what lets two bounded contexts talk without
sharing a module: each keeps its own copy of the contract.

Two libraries:

- `ascetic_ddd.bus` — `Ascetic_bus.Bus`, the dispatcher and the
  `Adapter` signature. Depends on nothing but the standard library.
- `ascetic_ddd.bus.in_memory` — `Ascetic_bus_in_memory.In_memory`, a
  process-local adapter built on `Eio.Stream`.

---

## Quick start

```ocaml
module Bus = Ascetic_bus.Bus
module In_memory = Ascetic_bus_in_memory.In_memory

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->

  (* 1. Composition root: bind the "in-memory" scheme to one broker. *)
  let bus = Bus.create () in
  Bus.register bus ~scheme:"in-memory" (In_memory.adapter (In_memory.create ~sw));

  (* 2. Consumer side: declare how the wire is read. *)
  let consumer =
    Bus.consumer bus ~uri:"in-memory://orders.placed" ~group:"billing"
      ~deserialize:Yojson.Safe.from_string
  in
  let subscription =
    Bus.subscribe consumer (fun json -> print_endline (Yojson.Safe.to_string json))
  in

  (* 3. Producer side: declare how the wire is written. *)
  let producer =
    Bus.producer bus ~uri:"in-memory://orders.placed" ~serialize:Yojson.Safe.to_string
  in
  Bus.publish producer (`Assoc [ ("order_id", `String "o-1") ]);

  (* Delivery runs on the broker's dispatch fiber; yield to let it run. *)
  Eio.Fiber.yield ();
  Bus.unsubscribe subscription
```

```lisp
; dune
(libraries ascetic_ddd.bus ascetic_ddd.bus.in_memory eio eio_main yojson)
```

---

## Concepts

### URI = scheme + topic

A URI is `scheme://rest`. The scheme selects the adapter; the adapter
decides what the rest means. The in-memory adapter keys topics by the
full URI string. A network adapter would map it onto its own notion of
a topic or channel.

`Bus.consumer` and `Bus.producer` resolve the scheme eagerly. A URI
whose scheme has no adapter — or that has no `scheme:` prefix at all —
raises `Bus.Unknown_scheme` at construction time, so a wiring mistake
fails at startup rather than at the first publish. Registering two
adapters for one scheme on the same bus raises `Bus.Already_registered`.

### Consumer groups

Groups have the semantics of Kafka consumer groups: every group
registered on a URI sees every message (fan-out across groups), and a
message is handled once within a group.

The in-memory adapter enforces a stronger invariant on top: at most
one consumer per `(uri, group)` per broker. A second registration
raises `In_memory.Already_registered_in_group`. In a single process
each logical role is one instance, so a duplicate is a configuration
bug and should fail fast rather than silently split the stream.

### Wire contract

The bus never inspects payloads. Producers serialize to `string`,
consumers deserialize from `string`, and nothing in between knows the
OCaml types on either side. Consequences:

- there is no translation layer between contexts — each side owns its
  own (de)serializer;
- consumers in different groups on the same URI may use different
  deserializers, and a payload one of them cannot decode is dropped
  for that group only.

### Per-bus, per-broker state

`Bus.create` and `In_memory.create` return isolated instances. There
is no global registry: two buses do not share adapters, two brokers do
not share topics, even when both are bound to the same scheme. Tests
can therefore run in parallel, each with its own bus and broker.

---

## In-memory adapter semantics

- **One dispatch fiber per topic**, forked as a daemon on the switch
  passed to `In_memory.create`. Every consumer on the broker tears down
  when that switch is released.
- **FIFO per topic.** Messages reach every group in publish order.
- **Synchronous hand-off.** The dispatch fiber invokes each group's
  callback in turn and does not take the next message until all of
  them return. A slow callback delays the whole topic; fork a fiber
  inside the callback if the work is heavy.
- **Bounded queue.** A topic buffers 1024 messages. `Bus.publish`
  suspends the publishing fiber while the buffer is full.
- **Subscribe before you publish.** A message that arrives while a
  group has no active callback is dropped for that group; nothing is
  replayed on `subscribe`.
- **Failures are contained, not retried.** A deserialization exception
  drops the message for that group and logs a warning through `Logs`.
  A callback exception is logged the same way and dispatch continues
  with the next group and the next message. The adapter is
  at-most-once and holds nothing across restarts. For delivery
  guarantees, feed it from the outbox dispatcher and consume through
  the inbox.
- **`unsubscribe` detaches the callback but keeps the group.** The
  same consumer handle can `subscribe` again; a second `unsubscribe`
  on the same handle is a no-op.

---

## Writing an adapter

An adapter is a first-class module of signature `Bus.Adapter`:

```ocaml
module type Adapter = sig
  type 'a adapter_consumer
  type 'a adapter_producer
  type adapter_subscription

  val consumer :
    uri:string -> group:string -> deserialize:(string -> 'a) -> 'a adapter_consumer

  val producer : uri:string -> serialize:('a -> string) -> 'a adapter_producer
  val publish : 'a adapter_producer -> 'a -> unit
  val subscribe : 'a adapter_consumer -> ('a -> unit) -> adapter_subscription
  val unsubscribe : adapter_subscription -> unit
end
```

Connection state lives in the closure that builds the module, the way
`In_memory.adapter broker` captures its broker. Register it under the
scheme it serves:

```ocaml
Bus.register bus ~scheme:"kafka" (Kafka_adapter.adapter client)
```

Nothing on the producer or consumer side changes when the scheme in
their URIs changes.

---

## Testing

Each test creates its own bus and broker under its own
`Eio.Switch.run`. After `Bus.publish`, one `Eio.Fiber.yield ()` is
enough to let the dispatch fiber deliver to every subscribed group.
See [`test/bus/`](../../test/bus/) for the reference suites.
