# Bus

Scheme-dispatched message bus: typed producers and consumers over opaque
wire messages, with an in-memory adapter. A port of the Rust reference
implementation of these building blocks, which itself began as a port of
this library's first version.

```ocaml
open Ascetic_bus
module Broker = Ascetic_bus_in_memory.In_memory_broker

let ( let* ) = Result.bind

let wire ~sw =
  let* bus = Bus.register Bus.empty ~scheme:"in-memory" (Broker.adapter (Broker.create ~sw ())) in
  let* orders = Bus.consumer bus ~uri:"in-memory://orders" ~group:"billing" ~decode:decode_order in
  let* _subscription = Consumer.subscribe orders (fun order -> bill order) in
  let* producer = Bus.producer bus ~uri:"in-memory://orders" ~encode:encode_order in
  Producer.publish producer order
```

## Design

The bus is a registry of transports keyed by URI scheme; a call site names
the scheme, never the transport, so replacing a broker with the in-memory one
is one `register` line at the composition root. The registry is a value:
`register` gives a new bus, and there is no global one. The bus carries
opaque wire messages: each producer encodes with a function of its own and
each consumer decodes with one of its own, so two consumers of one topic may
read the same bytes as different types. The wire format is the contract; the
types are local to each side.

* **A message has a payload, a key and headers.** `Message.t` carries bytes,
  an optional key and flat headers, name to bytes, as a broker's are. A
  transport that partitions needs the key to keep an aggregate's messages in
  order, and the key belongs to the message, not to the URI, because a
  producer serves many aggregates. A URI may still carry one,
  `scheme://channel/key`: a producer built for it stamps the key on messages
  that have none.
* **Push, not pull.** A consumer runs a handler for every message. A handler
  is a plain function on an Eio fiber, `'a -> (unit, Failure.t) result`.
* **Errors are values.** Every operation returns a `Bus_error.t`. A handler's
  error means the message was not handled: a transport that can, redelivers
  it. `Failure.Transient` is the ordinary failure; `Failure.Permanent` is the
  one verdict the bus carries, a failure no retry will mend, from whoever can
  tell, a stage or a handler, to a transport that can act on it: the inbox
  parks such a message at once.
* **A transport is a record of functions**, `Adapter.t`: a consumer of wire
  messages and a producer of them. The registry holds transports of any kind
  side by side.

A subscription is cancelled explicitly, never by discarding its handle: the
composition root discards most handles. A subscription cancelled is a
handler that is not running and will not be run (ADR-0016):
`Subscription.cancel` detaches the handler and waits for the call in flight,
and on a channel over a database for the loop to finish the batch it has in
hand, committed and its connection given back. Then what the handler uses
may be taken down: that is what stopping in good order needs. A handler may
cancel its own subscription; it does not wait for itself, and the message in
hand is its last. A transport gets this from `Handling`, which counts a
subscription's calls in flight: every adapter here makes its calls through
it. A message a consumer cannot decode
is reported and skipped: a poison message must not stop the rest.

A message may go through *stages* between the typed layer and the
transport: `Producer.through producer stage` sends every message out through
the stages in order, after encoding; `Consumer.through consumer stage`
brings every message back through them in reverse, before decoding. Sealing
goes here (ADR-0001), so that nothing between the two ends sees a payload in
the clear; the bus does not know what a stage does. A stage that fails on
the way out fails the publish; one that fails on the way in fails the
handling, so the message is kept and tried again, never skipped.

A producer or consumer that is transactional by nature, the outbox, the
inbox, is obtained from its adapter rather than from the registry, and names
the transaction at the call: `Transactional.Producer.publish producer
session value` publishes inside the caller's transaction,
`Transactional.Consumer.subscribe consumer (fun session value -> ...)` runs
the handler inside the transaction that acknowledges the message. The bus
never sees a session; it passes one through (ADR-0011).

`Bridge` is a Messaging Bridge: what arrives on one channel is published on
another, bytes, key and headers untouched, to one fixed URI or to the URI a
header names. It acknowledges a message only after the target accepted it.
An outbox dispatcher is a bridge from the outbox channel to the destination
each message names; an inbox intake is a bridge from a broker channel to the
inbox channel.

## Adapters

`In_memory_broker` (`ascetic_ddd.bus.in_memory`) is the monolithic
transport: topics in a process-local registry, a delivery fiber per topic on
the switch the broker was given, one consumer per `(uri, group)`, ordered
delivery, a bounded queue whose fullness makes producers wait, a handler
that fails or raises loses its message and not the topic. Cancelling a
subscription waits for its handler if it is running; the topic goes on for
the other groups.

`Kafka_broker` (`ascetic_ddd.bus.kafka`) is the same surface over Kafka, on
[`kafka-eio`](https://github.com/loganbnielsen/kafka-eio), an Eio client
built on librdkafka. A URI `kafka://orders` names the topic `orders`; a
consumer group of the same name shares the topic's partitions; a message's
key is its Kafka key, and messages with one key stay in order. Delivery is
at least once: the offset of a message is committed after its handler
returns. A handler that fails is retried until it succeeds, so the partition
waits and keeps its order; a handler that raises, a message that cannot be
decoded, and a failure that is `Failure.Permanent` are reported and skipped,
because a poison message must not stop the partition. Cancelling a
subscription stops its loop between messages, never inside the handler, and
waits for it: the message in hand is handled and its offset committed; one
whose handler keeps failing is left uncommitted, to come again. TLS, SASL and tuning
come from the `security` and `properties` the broker is built with.

```ocaml
let broker = Kafka_broker.create ~sw ~clock ~brokers:[ "localhost:9092" ] () in
let* bus = Bus.register Bus.empty ~scheme:"kafka" (Kafka_broker.adapter broker)
```

The adapter is optional: it is built only where `kafka-eio` is installed, so
the rest of the library does not depend on it. `kafka-eio` is not on opam
yet and needs the system's librdkafka:

```bash
sudo apt-get install -y librdkafka-dev
opam pin add kafka-eio git+https://github.com/loganbnielsen/kafka-eio#784bf37c32e141406cf6a1837150d38d2cb2919e
```

The commit above is the one the adapter was written and tested against; the
client is young and its interface has changed between releases, so pin it.
A consumer and the broker's producer are not domain-safe: share them between
fibers of one Eio domain only.

## Logging

What the bus cannot return it reports through a `Logs` source of its own,
`Ascetic_bus.Log.src`, named `ascetic_ddd.bus`: a message that could not be
decoded, a handler that failed on a transport with nobody to tell. An
application routes or silences it by that name.

## Testing

```bash
dune test test/bus

# the Kafka tests need a live broker, and are skipped without one
docker compose up -d redpanda
TEST_KAFKA_BROKERS=localhost:59092 dune test test/bus/kafka --force
```
