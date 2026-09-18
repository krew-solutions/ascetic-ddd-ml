# ADR-0010: The bus carries wire messages with a key and headers, and its errors are values

## Status

Accepted (2026-09-19). Follows the Rust reference implementation of the
bus, which began as a port of this library's first version and then grew
headers, fallible handlers, keyed URIs, a bridge, transactional producers
and consumers, and stages of the wire. Ported back with the shapes OCaml
gives them.

## Context

The first bus carried a `string` per message, raised exceptions for a
scheme unknown or registered twice, took handlers of type `'a -> unit`, and
described a transport as a first-class module with abstract consumer,
producer and subscription types. It could not carry what the outbox and the
inbox need to be channels of it (ADR-0011): a destination and an identity
beside the payload, a key for transports that partition, and a handler's
refusal, so that a message not handled is delivered again rather than lost.
Sealing a payload (ADR-0001) had no place between the typed layer and the
transport.

## Decision

1. **A wire message is a payload, an optional key and flat headers**,
   `Message.t`, a value with functional updates. The key belongs to the
   message, because a producer serves many aggregates; a URI may carry one
   too, `scheme://channel/key`, and a producer built for it stamps the key
   on messages that have none.
2. **Errors are values.** Every operation returns `Bus_error.t`:
   `Unknown_scheme`, `Already_registered`, `Already_in_group`, `Transport`,
   `Stage`. The registry is a value as well: `Bus.register` gives a new bus.
3. **A handler may fail, and says whether trying again can help.** A handler
   is `'a -> (unit, Failure.t) result`; `Failure.Transient` is the ordinary
   failure, `Failure.Permanent` the one verdict the bus carries, from whoever
   can tell, a stage or a handler, to a transport that can act on it. A
   message that cannot be decoded is reported and skipped.
4. **A transport is a record of functions**, `Adapter.t`, a consumer and a
   producer of wire messages; the typed layer, `Consumer` and `Producer`,
   encodes and decodes around it, and sends every message through its
   `Stage`s, out in order and back in reverse.
5. **`Transactional` producers and consumers pass a session through**, of a
   type the bus does not know (ADR-0011). **`Bridge`** publishes on one
   channel what arrives on another, and acknowledges after the target
   accepted.
6. **What cannot be returned is logged**, through a `Logs` source of the
   bus's own: the bus has no observer, and a delivery fiber has no caller.

## Objections considered

1. *A record of closures gives up the abstract types the module type had.*
   Those types were packed existentially at registration and no caller ever
   saw them; the registry needs transports of different kinds side by side,
   which is what a record of functions is. The outbox and inbox adapters are
   fifteen lines each as records; as first-class modules with three abstract
   types each they were not shorter and no safer.
2. *One closed failure type erases the handler's own error.* At the wire
   level the handlers of every consumer of a topic are held in one table, so
   their error type must be one. What is read downstream is the text and the
   verdict: the in-memory broker logs it, the outbox returns it to its
   caller, the inbox records the text in the row and parks on the verdict. A
   universal open type, exceptions as values, would carry more and nothing
   would read it.
3. *An immutable registry rules out registering a transport late.* It does,
   on purpose: registration belongs to the composition root, and the Rust
   registry enforces the same by exclusive borrow before sharing. A test
   builds a bus of its own in one expression.
4. *Logging from a library.* Decoding failures and the failures of handlers
   on the in-memory transport have no caller to return to. The source is
   named, `ascetic_ddd.bus`, so an application routes or silences it. The
   outbox and the inbox have observers, which the bus does not, and log on
   sources of their own only what must not be silent without one
   (ADR-0009).

## Consequences

- Breaking change of the whole surface: `Bus.create`, `Bus.publish`,
  `Bus.subscribe` and the exceptions go; `In_memory.create` becomes
  `In_memory_broker.create ~sw ?capacity ()`.
- The in-memory broker keeps its semantics, one consumer per group, ordered
  delivery by a fiber per topic, a bounded queue, and gains keys from URIs
  and handlers that fail without stopping the topic.
- The network transport is Kafka, as in the reference, an optional
  sub-library (ADR-0012).
- Tests: the registry and URI shapes, the in-memory broker, the bridge over
  two brokers, stages out and back in, and the transactional pass-through,
  `test/bus`.
