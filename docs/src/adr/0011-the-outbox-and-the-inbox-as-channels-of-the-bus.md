# ADR-0011: The outbox and the inbox as channels of the bus, joined by bridges

## Status

Accepted (2026-09-19). Follows the decision taken first in the Rust
reference implementation; depends on ADR-0001 and ADR-0010. Implemented in
`lib/bus/bridge.ml`, `lib/outbox/outbox_channel.ml` and
`lib/inbox/inbox_channel.ml`, with the outbox-to-inbox path without a broker
covered by `test/inbox/test_bridge.ml`.

## Context

ADR-0001 makes the outbox and the inbox wire-level stages that store and
relay bytes plus metadata. The open question was how a transaction reaches
them, given that the bus is also used by services without a database.

Watermill's *transactional events* example treats the database as a pub/sub
of its own: a table is a channel with the same publisher and subscriber
interfaces as Kafka, a publisher is built *with* the transaction, and a
router handler subscribes to the table's channel and publishes to Kafka's:
Hohpe's Messaging Bridge. Its `forwarder` variant carries the destination
inside the message, so one table serves every topic.

## Decision

- **The outbox and the inbox are adapters of the bus**, each with a channel
  of its own, beside a broker and the in-memory one. Reliability is not a
  scheme over another scheme; it is which channel a message is published to.
- **The transaction enters at the call.** Producers are built once, at the
  composition root, before any transaction exists; the transaction is opened
  later, by the command handler or by the inbox. So `Outbox_channel.producer`
  gives a `Transactional.Producer.t` whose `publish producer session value`
  takes the session. It is obtained from the outbox, not from the registry,
  so the bus stays free of sessions and plain adapters do not change; a
  service without a database never meets one.
- **The destination travels in the message.** An outbox row keeps the final
  URI, `kafka://orders/order-1`; on the wire it is a header, so a bridge can
  read it.
- **A bridge is one generic component**: a consumer of one channel and a
  producer of another, with a pass-through handler; the outbox's consumer
  group acknowledges after the producer has accepted the message, so
  delivery is at least once. The outbox dispatcher is a bridge from the
  outbox channel to the destination named in each message; the inbox intake
  is the same bridge from a broker channel to the inbox channel.
- **The inbox's consumer hands the handler its transaction.**
  `Inbox_channel.consumer` gives a `Transactional.Consumer.t` whose handler,
  `session -> value -> _`, runs inside the transaction that marks the
  message processed. It comes from the inbox, as the transactional producer
  comes from the outbox; a bus consumer of `inbox://...` is refused, since
  through it the handler's writes would fall outside the mark's transaction.
  This is where the design goes beyond Watermill, whose SQL subscriber
  acknowledges in a transaction of its own and leaves the handler's writes
  outside it.
- **The one verdict the bus carries reaches the inbox.** A handler's error
  is a failure of the moment, retried as the inbox's retries say;
  `Failure.Permanent`, from a handler or a stage, parks the message at once
  (ADR-0009).

## Refutations attempted

- The session in every `publish` call is noise for a producer that is
  transactional by nature. It is the only moment the session exists; a
  producer bound at construction would have to be rebuilt per transaction,
  which is the same argument in a longer form.
- Two kinds of producer, `Bus.producer` with `publish producer value` and
  `Outbox_channel.producer` with `publish producer session value`. The extra
  argument is the guarantee itself, named at the call site.
- The destination header is a second source of truth beside the channel. It
  is the only source: the outbox channel has no destination of its own,
  exactly as the forwarder's envelope.
- The channels need a switch and a clock the rest of the adapters do not.
  The dispatcher and the processing loop are fibers that outlive the call
  that started them, and wait between polls; in Eio both are capabilities
  passed in, not ambient. They are arguments of `Outbox_channel.adapter`
  and `Inbox_channel.consumer` only, so the ports and `publish` stay free
  of them.

## Consequences

- `Message.t` carries headers: `destination`, stamped by the outbox and
  read by the bridge and the inbox; the inbox's identity, `tenant_id`,
  `stream_type`, `stream_id`, `stream_position`; and whatever else the
  metadata holds, `message_id`, `causal_dependencies`, as text, structured
  again on the way into the inbox.
- Both channels run as daemon fibers on the switch they are given; a
  subscription's `cancel` stops the loop cooperatively, after its batch or
  its message. A defect that stops `run` is reported through the bus's log
  source and the loop starts again after the poll interval.
- On the bus the outbox is `Ascetic_bus.Failure.t Pg_outbox.t`: the error of
  a wire handler is the subscriber's error.
- `ascetic_ddd.outbox` and `ascetic_ddd.inbox` depend on `ascetic_ddd.bus`;
  the bus stays free of database dependencies.
- Encryption remains a wire-level stage keyed per tenant from a header,
  placed before the outbox channel, as ADR-0001 decides.
- Tests: `test/outbox/test_bridge.ml`, a committed message crosses the
  bridge and a rolled-back one does not, a failing subscriber gets the batch
  again; `test/inbox/test_bridge.ml`, a message from a broker processed once
  in the marking transaction, the outbox feeding the inbox without a broker,
  a failing handler retried with its writes rolled back, a permanent verdict
  parking at once. Their recorded runs are checked against the models.
