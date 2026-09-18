# ADR-0012: The Kafka adapter stands on kafka-eio, and is optional

## Status

Accepted (2026-09-19).

## Context

The bus needs a transport over a network, as the reference implementation
has: Kafka, on librdkafka. The OCaml clients on opam, `kafka`, `kafka_lwt`
and `kafka_async`, are bindings for Lwt and Async; none runs on Eio, which
the rest of this library is written for. Three ways to get one were weighed.

What the adapter must carry is fixed by the rest of the library: a key, so
that an aggregate's messages stay in order; headers in both directions,
because the outbox stamps `destination` and the inbox reads its identity
from `tenant_id`, `stream_type`, `stream_id` and `stream_position`
(ADR-0011); consumer groups that share a topic; and acknowledgement after
the handler, so that delivery is at least once.

## Decision

1. **The adapter is `Kafka_broker` on `kafka-eio`**, an Eio client built
   on librdkafka: consumer groups, keys, headers, an offset committed after
   the handler returns, a producer that waits for the broker's
   acknowledgement. A handler that fails is retried until it succeeds, so
   the partition keeps its order; a handler that raises, a message that
   cannot be decoded, and a failure that is `Failure.Permanent` are reported
   and skipped.
2. **It is an optional sub-library**, `ascetic_ddd.bus.kafka`: built only
   where `kafka-eio` is installed, a `depopt` of the package, its tests
   built under the same condition and skipped without a live broker. The
   rest of the library neither links librdkafka nor knows of it.
3. **The client is pinned to a commit**, the one the adapter was written
   and tested against, named in `lib/bus/README.md`.

## Objections considered

1. *The client is young, has one author, and is not on opam.* True: created
   in August 2026, version 0.3.0, breaking changes between 0.2 and 0.3, one
   maintainer. The exposure is bounded on our side instead: the adapter is
   150 lines behind `Adapter.t`, optional, pinned, and covered by seven
   tests against a live broker, so a client that stalls is replaced behind
   the same record of two functions without touching a caller.
2. *A REST proxy would need no C library and no young client: an adapter
   of our own over HTTP, on `cohttp-eio`.* The proxies do not carry the
   contract. Confluent's REST Proxy has record headers only in its v3
   produce endpoint; its v2 API, the only one that consumes, has `key`,
   `value`, `partition` and `offset` and no headers, and Redpanda's HTTP
   proxy follows v2. Strimzi's bridge documents headers on produce and shows
   none on consume. Without headers on the way in, the inbox cannot read an
   identity, and the destination and identity would have to be wrapped into
   the payload, a wire format of our own that no other consumer of the topic
   reads. Besides: a consumer instance is state held by one proxy node, so
   every request of a consumer must reach that node; the proxy is one more
   service to run and to secure, outside Apache Kafka; there is no
   idempotent producer and no transaction through it; and polling over HTTP
   with base64 bodies costs latency. It diversifies the client library, not
   the broker, and pays for it with the contract. Rejected; to be looked at
   again if a proxy gains headers on consume.
3. *A second broker, NATS JetStream, would spread the risk wider.* The
   client on opam, `nats-client`, is a codec of the text protocol only: no
   JetStream, no runtime for Eio, and its reader of header blocks is not
   exported. An adapter would mean an Eio client, reconnection, the
   request-reply multiplexer, the JetStream API over `$JS.API.*` and the
   acknowledgement protocol of our own, five to six hundred lines of
   protocol code to maintain, for three hundred lines of codec gained. And a
   second broker lowers the risk only for a deployment that would run it.
   Dropped.
4. *An optional library may rot unbuilt.* Where `kafka-eio` is absent the
   adapter is not compiled, so a breaking change of the bus could go
   unnoticed there. The build was checked both ways, with and without the
   client; continuous integration does not install it, so the check with it
   is a local one for now, which the README says.

## Consequences

- `docker-compose.yml` has a Redpanda service, a Kafka-API broker in one
  container, for the tests: `TEST_KAFKA_BROKERS=localhost:59092`.
- The Kafka tests of the reference are ported, a message from producer to
  consumer, every group receiving, order with one key, a cancelled
  subscription receiving nothing more, with three more: key and headers
  crossing the wire, a failing handler given the message again while what
  follows waits, a permanent failure skipped.
- A deviation from the reference: the offset is committed synchronously
  after each message, where the reference stores it and lets the driver
  commit in the background; `kafka-eio` exposes the first and not the
  second. At least once either way, one round trip more per message.
- A deviation from the reference: a permanent failure is skipped, where the
  reference, written before the bus carried that verdict, would retry it for
  ever and hold the partition.
- A consumer and the broker's producer are not domain-safe, as the client
  says: fibers of one Eio domain only.
