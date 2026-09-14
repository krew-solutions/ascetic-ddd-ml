# ADR-0001: Serialize and encrypt before the outbox

## Status

Accepted (2026-09-14). Follows the decision taken first in the Rust port
of these building blocks, so that one wire contract holds across ports.

## Context

The outbox and the inbox stored `payload` as JSONB and left serialization
for the broker to the dispatcher, as the Python source did. That choice
bought a queryable table and format-agnostic rows, and it was made before
encryption at rest was a requirement. Systems built on these blocks carry
personal and device data that must not rest in clear, and integration
events cross the process boundary as bytes of a contract the receiving
side owns.

The bus separates a typed layer, producers with an encoder and consumers
with a decoder, from a wire layer that carries opaque bytes. The question
was which side of the outbox the typed layer sits on.

## Decision

Serialize, and encrypt where the deployment requires it, before the
outbox. `payload` is `BYTEA` in both tables; the outbox and the inbox are
wire-level stages that store and relay bytes plus metadata and never
inspect the payload. `metadata` stays JSONB for routing and support.

The identifier carried in `metadata` for idempotency is `message_id`, not
`event_id`: the outbox carries command messages as well as event
messages, and the receiver deduplicates on the message, whatever it says.
The inbox's `uri` column is 255 characters, as the outbox's is, since a
keyed URI such as `kafka://orders/order-<uuid>` exceeds the former 60.

## Consequences

- Plaintext never reaches the database. Encryption at the origin is the
  strongest at-rest guarantee and the easiest to audit.
- One serialization, at the one place where the type is known: no JSON
  round trip and no lossy re-typing of decimals, enums or versions.
- The dispatcher knows no schemas and depends on no domain code: bytes
  in, bytes out, routing from `uri` and `metadata`.
- The outbox table is no longer readable in `psql`. Under encryption at
  rest that is the goal, not a loss; metadata stays in clear.
- A row is bound to one wire contract. A message that must reach a second
  audience in another format is published twice, to two URIs, at the
  origin, where the type is known. Nothing is transcoded downstream.
- The command's transaction pays for serialization and encryption. Both
  are microseconds for a message of a few kilobytes; a key-management
  outage fails the command, which is failing closed.
- `Outbox_message.payload` and `Inbox_message.payload` are `string`,
  holding the wire bytes. Callers that carried `Yojson.Safe.t` serialize
  with `Yojson.Safe.to_string` on the way in and parse in the subscriber.

## Open

- What metadata may travel in clear, since stream ids in causal
  dependencies are identifiers too.
- Whether encryption is per message (envelope) or per topic key.
