# ADR-0024: The bus's key is one per tenant on a mount of its own, derived per channel; the adapter makes no key, custody is a port beside, and a refusal is read once

## Status

Accepted (2026-10-15). Follows the decision taken first in the Rust
reference implementation (its ADR-0017, 2026-10-13, amended the same day
with the composition of the stages). Held by
`test/kms/vault/test_vault_transit.ml` (no key made under managed keys,
the context with every operation and `derived` on a key made, a `400` and
a `403` on a KEK operation explained by one read, custody's creation and
retirement), `test/kms/vault/test_vault.ml` (the same on a Vault dev
server: a derived key binds the DEK to its context, managed keys are not
made on first contact, custody makes the key once and retires its
versions, a gone key is `Kek_not_found` and another tenant's key
`Decrypt`), `test/kms/test_pg.ml` (the PostgreSQL adapter: managed keys,
the context bound into the wrap, custody), `test/kms/test_domain.ml` (a
KEK serving a context binds what it wraps to it; the binding's bytes) and
`test/dek/test_stage.ml` (two stages built apart from one channel constant
seal and open; another channel's stage is refused for good; the sealing
of a service's channels is built up front and answers for those alone).

## Context

`Envelope_stage.Make` seals a message's payload under a DEK wrapped by the
tenant's KEK through `Kms_port.S`, as ADR-0001 decided; the KEK is the
tenant's, one on one mount, named by the tenant's id (ADR-0023). A
platform running the reference's stage on its bus found four things the
library did not say or did wrongly, and this port had each of them, having
been written the same way:

- **The adapter made keys for whoever wrapped.** `encrypt_dek` and
  `generate_dek` called `POST keys/{name}` first. Transit creates a key on
  `encrypt` too, under a token that may `create` *or* `update` on
  `keys/+`. So a producer that wrapped for a tenant whose key had been
  deleted, crypto-shredded at off-boarding, made the key again at version
  1, and the tenant's data flowed again; and a token that may not make
  keys failed every wrap with a `403`.
- **A gone key was tried again, for ever.** Transit answers `400` for a
  key it has not got on `decrypt`, `datakey` and `rewrap`, and `404` only
  on `GET keys/{name}`; the adapter mapped `404` alone to `Kek_not_found`,
  so a gone key reached the stage as `Vault`, which its verdict leaves of
  the moment, and the inbox, whose default retries are unbounded
  (ADR-0004), held the message's slot, shared by every tenant of the topic,
  until an operator looked.
- **One key per tenant served every channel alike.** A consumer that may
  unwrap on a tenant's key may unwrap any channel's DEK of that tenant,
  stored data's too; the broker's ACL is the only thing telling channels
  apart. Transit's derived keys derive a key per caller-supplied `context`,
  and a policy can pin the `context` a caller may pass, which gives a
  per-channel gate at the KMS without a key object per channel, but only
  on a key that is derived: on a plain key Transit ignores `context`, so a
  policy's pin on a mount holding both kinds opens the plain keys' DEKs.
  `derived` cannot be changed after creation.
- **Nothing retired a version.** `min_decryption_version` makes what an
  earlier version wrapped unopenable for good, and a bus's ciphertext lives
  no longer than its channel's retention, so the bound of a message's life
  is one call once the retention has passed; the port had no such call.

## Decision

1. **The bus's key is a derived key per tenant on a mount of its own.** A
   deployment builds two `Vault_transit` instances: one on the mount of
   ADR-0023 for the store's KEK, the signing and the MAC keys; one on a
   second mount, every key of it derived, one per tenant. `?mount` exists
   and the port does not change: the purpose is the adapter instance. A
   mount, not a name label, because the per-channel gate of point 2 is void
   on a path shared with plain keys, and a policy's prefix names a mount.
2. **The channel is the derivation context, taken from the contract.**
   `?derivation_context` on either adapter's `create`: on Vault every KEK
   operation carries the context, base64, and a KEK the service makes is
   made `derived`; on PostgreSQL the context is bound into the wrap's
   associated data beside the tenant, `Kek.binding`, the tenant's id
   length-prefixed by four big-endian bytes and then the context, so
   neither runs into the other, the same bytes as the reference writes.
   Either way a DEK wrapped for one channel does not unwrap for another:
   `Decrypt`. The composition root builds one instance, one cache and one
   stage per channel, at the producer's end and at the consumer's, from
   the channel's name in the contract both are generated from; the context
   never comes from a header. One stage per channel also keeps `Reuse`
   sound: the DEK in service is keyed by the tenant, and would otherwise be
   drawn under one channel and reused for another's message. The mechanism
   lives beside the rule as `Sealing.Make (Sessions) (Kms)`: the stages of
   a service's channels built up front from the list its contracts name,
   each over the KMS instance the caller's function makes for the channel,
   behind a `Cached.Make` of its own, with the parameters the deployment's;
   `of_channel` answers `None` for a channel the sealing was not built for,
   so a composition root refuses to start rather than seal under a typo.
3. **The adapter makes no key for whoever wraps.** `?managed_keys:true` on
   either adapter: no `POST keys/{name}` before a wrap; a tenant without a
   KEK is `Kek_not_found` on every wrap, parked at once by the stage. A
   producer's or a consumer's token may then be denied `create` and
   `update` on `keys/+`, and a shredded tenant stays shredded. The default
   stays as it was, for the store and for the tests: a key on first contact
   (ADR-0014).
4. **Custody is a port beside, not on, `Kms_port.S`.** `Key_custody.S`:
   `create_kek tenant_id options`, idempotent, answering the latest version,
   a key that exists with another `derived` being `Conflict`, since that
   cannot be changed, and the rotation period, which can, set by `config`;
   and `retire_kek_versions_before tenant_id version`, Transit's
   `min_decryption_version`. Beside the KMS port for the reason ADR-0023
   gave for signing: the DEK store and the stage are functors over
   `Kms_port.S` and must not grow a bound they do not use, and a producer
   or a consumer of the bus must not hold the custody of its keys.
   `Cached.With_custody` passes it through and forgets the tenant's
   unwrapped keys on a retirement, as on a deletion. The PostgreSQL adapter
   serves it as the simple adapter: `create_kek` under the tenant's lock,
   `derived` with nothing to enforce, the binding being the wrap's,
   `auto_rotate_period` refused as `Unsupported` rather than ignored, a
   table having no scheduler, retirement a deletion of the rows below the
   version.
5. **A refusal of a KEK operation is explained by one read of the key, and
   what the read does not explain is the request's own fault.** ADR-0023's
   reading of a `400` extended to the KEK: on a `400` from `encrypt`,
   `decrypt`, `datakey` or `rewrap` the adapter reads `GET keys/{name}`
   once; a `404` is `Kek_not_found`, a version outside what the key serves
   `No_key_of_version`; and, the key there and the version served, the
   `400` no longer stays Vault's: on `decrypt` and `rewrap` it is `Decrypt`,
   a wrong context, another tenant's key, bytes changed since they were
   sealed, elsewhere `Malformed` with Vault's words. A `403` is read the
   same way: the key there, `Forbidden`, the key not there,
   `Kek_not_found`, the read refused too, the `403` as it came, for the
   operator whose policy lacks the read. Vault's texts are not matched. The
   stage's verdict parks `Forbidden`, `Conflict` and `Unsupported` with the
   rest it parks: no retry under the same identity and context mends them.
6. **The in-memory keys do not serve the KEK port**, as in the reference:
   the stage's and the custody's tests run on the PostgreSQL adapter, the
   consumer's on Vault.

Refined by ADR-0025 the same day: at an inbox that receives several
channels, one subscriber for every row and one stage per `through`, the
stage is chosen per row by its `destination`, the intake's name for the
channel received on, `Sealing.by_destination`, built up front from the
received channels, refusing a row it has no stage for before any KMS is
asked.

## Objections considered

1. *The derivation context and managed keys are optional arguments of
   `create`, where the reference has builder methods that read as a
   deliberate step; an argument left out is easy to miss.* The instance is
   built once, at the composition root, by `Sealing`'s function for the
   channel, which is the one place the arguments are written; an argument
   left out keeps the default, which is the store's behaviour and the
   reference's default too. The stage test builds its instances through
   one helper, which is what a composition root does.
2. *`Sealing` takes a clock and the reference's does not; one more
   argument for the composition root.* The reuse and the cache are timed,
   and in this port time is a capability passed in, never ambient (ADR-0013
   gave `Cached` its clock for that reason); the reference reads
   `Instant::now()`. One argument, once, where the stage was already given
   its pool.
3. *The reference made `SessionPool` hold for `Arc<P>` so that a service's
   shared pool serves the stages; this port has no such change, so
   something is missing.* Nothing is: an OCaml pool is a value shared by
   reference, and `Sealing.create` takes the service's pool as it is. The
   change was Rust's ownership answering Rust's question, and is not
   relevant here.
4. *`Kek_options.auto_rotate_period` is a float of seconds, which a caller
   may pass fractional, where the reference has a `Duration`.* As
   `Reuse.lifetime` and the cache's time to live are seconds here; Transit
   takes whole seconds and the adapter sends the integer part. A duration
   type of the library's own would be a third spelling of time beside
   `Mtime` and seconds, for one field.

## Consequences

- A tenant has a key object more, the bus's, and one lifecycle more: its
  retirement follows the bus's retention, the store's never; two deletes
  at off-boarding where ADR-0023 counted three.
- The same `Kms_port.S` surface serves a derived key and a plain one; a
  consumer tells them apart by how it builds the adapter, not by what it
  calls.
- The failure path of a KEK operation costs one read where it cost none;
  the success path none. What the stage parks is wider, and what it retries
  narrower: a `400` or a `403` with the key there was a retry before and is
  a parking now. Two tests of the live Vault suite changed their
  expectations for that reason: a gone key is `Kek_not_found` and another
  tenant's key `Decrypt`, where both were `Vault` with status 400.
- `Kek.generate` and `Kek.load` take the context as an explicit option and
  `Master_key.generate_kek` and `load_kek` as an optional argument, so no
  caller of the model changes; `Kek.binding` is public, since the bytes are
  a contract between the ports.
- `create_kek` reads the key back after making it, since Transit leaves a
  key that exists as it is and says nothing: one read on a path that is
  custody's, not the wrap's.
- `Kms_error.t` gains `Conflict`, `Forbidden` and `Unsupported`.

What can go wrong, and what holds it:

- *Two ends spell the channel differently*, a rename in one contract and
  not the other: `Decrypt` at the consumer, parked, for ever. Held by the
  stage test that builds both ends from one constant.
- *A policy admits a channel the consumer no longer reads, or lacks one it
  does*: `Forbidden`, parked. Held by the scripted test of the `403`, and
  by an alert on parked messages in a deployment; a `403` on the read
  itself stays of the moment, and an operator sees the policy lacks `read`.
- *Transit's `400` for a wrong context and for a corrupted ciphertext are
  one `Decrypt`*: so are they in authenticated encryption; a consumer that
  must tell them apart has the key's description and the message's
  `dek_bound_to` to look at, not Vault's words.

## Alternatives rejected

**The context in the stage's associated data instead of the key's
derivation.** Binds the payload, not the DEK: a consumer that may unwrap
the tenant's key opens the DEK of any channel, and the KMS's policy cannot
pin what it does not see.

**A key object per (tenant, channel), and the port's identity generalised
from `tenant_id` to a label.** Tenants × channels rotation schedules,
policies and deletes for a gate the pinned context gives without one.

**Derived contexts on the existing mount.** Measured void by the reference:
Transit ignores `context` on a plain key, so a consumer's pinned context
opens the store's DEKs through the `+` of its policy path.

**Telling a `403` or a `400` apart by Vault's text.** Works on this release
and breaks silently on the next (ADR-0023).

**Creating the key on first wrap, with `derived` from the context.** Keeps
the old contract and lets a producer resurrect a shredded tenant's key;
custody is the tenant-management context's.

**An in-memory adapter of the KEK port for the stage's tests.** The
PostgreSQL adapter is the simple one already, and the tests run on it with
nothing but the network replaced; a third implementation of the binding
would be a third place for it to drift.
