# ADR-0023: A tenant's keys are one per use: the KEK, a signing key per algorithm, a MAC key per purpose

## Status

Accepted (2026-10-15). Follows the decision taken first in the Rust
reference implementation (its ADR-0016, 2026-10-13), and was ported with
the key management library. Held by `test/kms/test_domain.ml` (the signing
and MAC keys of the model), `test/kms/test_in_memory_keys.ml` (the
in-memory keys behave as the adapters do where a consumer's tests would
notice), `test/kms/test_pg_signing.ml` (the ports on the PostgreSQL
adapter, against a live database), `test/kms/vault/test_vault_transit.ml`
(the adapter's reading of Transit's answers, against a scripted transport)
and `test/kms/vault/test_vault_signing.ml` (the ports on a Vault dev
server).

Amended 2026-10-23 by ADR-0035: point 5 (`sign` makes the key,
`sign_with_version` never does), point 6 (a refusal of `verify` is read on
a `403` and a `500` as on a `400`, and what the read does not explain is
the request's own fault) and the consequence "a public key is not served
by the port", which `Public_key_port.S` supersedes. Amended the same day by
ADR-0036: point 1, `rotate_signing_key` and `delete_signing_key` are on
`Signing_key_custody.S` and `delete_mac_key` on `Mac_key_custody.S`, the
use ports holding use alone; point 5, `sign` and `mac` make the key only
without managed keys.

## Context

`ascetic_ddd.kms` managed one kind of key: the key-encryption key of a
tenant, wrapping the data-encryption keys of `ascetic_ddd.dek`. A consumer
of the reference needed two more operations under a tenant's keys: a
digital signature over artefacts that other parties verify, and a MAC that
tags a draft so that a stale edit is refused, with the requirement that the
keys live in the KMS and never enter the service's process, a tenant-bound
HSM posture, and a verification that a regulated tenant may demand run in a
validated module rather than in a library the service links. This port
serves the same consumers through the same contract, so what the reference
decided about the keys is decided here too; what is OCaml's is how the
ports and the primitives are spelled.

The reference measured on Vault 1.21 OSS Transit, and the reasoning rests
on it: a key name is `[A-Za-z0-9._-]`, a `/` is "unsupported path" even
percent-encoded; `POST keys/{name}` with a type is idempotent; `hmac` is a
key type of its own; an ECDSA signature is ASN.1 DER unless
`marshaling_algorithm: jws`, which gives `r || s`; a signature by a key the
tenant has not got, by a version newer than the key's latest, and by a
version retired through `min_decryption_version` are all HTTP 400,
differing in text only; after a rotation the earlier versions still verify.

## Decision

1. **Two more ports beside `Kms_port.S`, not inside it.** `Signing_port.S`,
   `sign`, `verify`, `rotate_signing_key`, `delete_signing_key`, and
   `Mac_port.S`, `mac`, `delete_mac_key`. The KEK port has consumers that
   need no signing: `Pg_dek_store.Make` and `Envelope_stage.Make` are
   functors over `Kms_port.S` and nothing more, and must not grow a bound
   they do not use. A signature and a MAC are apart too: they share no
   value type and no consumer, and a MAC is checked by recomputation, so
   its port only makes them. Both adapters serve all three ports: Vault
   with the keys in Transit, PostgreSQL with the keys under its master key
   and the cryptography in process, the same byte forms on either; the
   adapter's module `include`s the four signatures over one `t`.
2. **One key per use, three keys per tenant, in one mount, told apart by
   name.** The KEK stays the tenant's id; a signing key is
   `{tenant}.sign.{algorithm}`, a MAC key `{tenant}.mac.{purpose}`. Nothing
   is derived from the KEK: the three keys have three lifecycles, and one
   key would couple them. Rotating the KEK rewraps the DEKs and the data
   stays readable; rotating a signing key makes new artefacts verify under
   the new version and old ones under theirs; rotating a MAC key makes
   every open draft stale at its next write. Deleting the KEK is
   crypto-shredding of the tenant's data; deleting a signing key is the end
   of verifiability of everything it signed.
3. **The dot is the separator, and the tenant's id is checked, not
   encoded.** `Key_label.is_part` is the alphabet, letters, digits, `_` and
   `-`; a label is one to sixty-four of them, `Key_label.make`; for the
   dotted names the tenant's id must be a part too, else `Malformed`,
   before any call is made. The KEK's percent-encoding is not copied.
4. **The algorithm is an input of signing and a property of the
   signature.** `sign` takes a `Signature_algorithm.t`; a `Signature.t`
   carries it, the `Key_version.t` of the key that made it, and the bytes
   in the one form fixed per algorithm, Ed25519's sixty-four, ECDSA's
   `r || s` through JWS marshaling, never DER, so that two encodings of one
   algorithm are never side by side. A tenant moves to another algorithm by
   signing with it: a new key, not a rotation.
5. **Signing makes the key, verifying never does.** `sign` and `mac` ensure
   the key before the operation. `verify` on a missing key is
   `Signing_key_not_found`: creating on verify would make a forged
   artefact's key appear, and a key deleted and re-created starts at
   version 1, so an old signature would then fail as invalid instead of as
   unknown.
6. **A `400` is explained by one read of the key, not by Vault's text.** On
   a `400` from `verify` the adapter reads `GET keys/{name}` once: a `404`
   is the key missing, a version outside
   `[min_decryption_version, latest_version]` is `No_key_of_version`,
   anything else stays `Vault`. The distinction is what lets a consumer
   tell "retry later" from "this artefact names a key the tenant has not
   got".
7. **Verification goes through the KMS.** The Vault adapter links no
   signature library; a tenant that must verify in a validated module gets
   that only there. The PostgreSQL adapter verifies with the model's
   `Public_key`, the keys being its own.
8. **The keys are the model's; the in-memory keys live in the library.**
   `Signing_key` and `Mac_key` stand beside `Kek`, one version of a keyring
   each, taking Ed25519 and ECDSA P-256 from `mirage-crypto-ec` and
   HMAC-SHA-256 from `digestif` as the cipher takes AES-GCM from
   `mirage-crypto`; `Public_key` is the half a verifier holds.
   `In_memory_keys.Make (Session)` serves the two ports from memory for
   the tests of a consumer, with a key registered from a seed so that a
   test's signatures are the same from run to run, so what the tests
   exercise is the mapping that runs with nothing but the network behind
   the port replaced.

## Objections considered

1. *`Cached` does not serve the signing and MAC ports, where the
   reference's `Cached<K>` does, so one value no longer stands for every
   port.* In Rust the consumer is generic over `K` and holds one `Cached<K>`
   for everything `K` serves; in OCaml the consumer names its modules, and
   an adapter that serves four ports is called directly for the three the
   cache has nothing to add to. What has content passes through:
   `Cached.With_custody` forgets a tenant's keys on a retirement as on a
   deletion (ADR-0024). The sealing of the bus composes `Cached.Make` over
   the KEK port alone, which is what it needs.
2. *`Signing_key.sign` and `Mac_key.mac` are total, where the reference's
   return a `Result`: a failure is hidden.* Nothing is hidden: the reference
   re-parses the private half at every signing, which can fail, and this
   port parses it once, when the key is made or loaded, and keeps the
   parsed half in the value, so a `Signing_key.t` is a key of its algorithm
   by construction. The domain test holds a P-256 scalar of zero and a seed
   of thirty-one bytes to `Malformed` at `of_seed`, where they can still be
   refused.
3. *`mirage-crypto-ec`'s ECDSA takes a digest, not a message, and its
   RFC 6979 nonce comes with a warning about power analysis of the signer.*
   The digest is SHA-256 of the message, computed where the key signs and
   where it verifies, which is what Transit's `hash_algorithm: sha2-256`
   does; the reference's `p256` crate derives its nonce by RFC 6979 as
   well, and a deterministic signature is what lets two first signings at
   once, under one key, be checked equal. The attack needs the signer's
   power trace; the posture that forbids it keeps the keys in Vault, where
   this process signs nothing.
4. *Three `CREATE TABLE` statements in `setup` where the reference sends
   one batch.* Caqti's PostgreSQL driver runs one statement per request;
   the three are sent in one transaction under the three advisory locks
   taken in one order, which is the property, and the batch was a
   convenience of the other driver.

## Consequences

- A tenant has three keys and three deletes: `delete_kek` does not touch
  the signing or MAC keys. Off-boarding a tenant is three calls, one per
  port, or a mount per tenant that is unmounted whole.
- `Kms_error.t` gains `Signing_key_not_found`, `Mac_key_not_found` and
  `Invalid_signature`; an exhaustive match outside the library stops
  compiling, which is how a consumer learns of them. The envelope stage's
  verdict parks them with the rest it parks, though no KEK operation
  produces them.
- One `Vault_transit.Make (Transport)` serves all three ports over one
  mount and one token; one `Pg_kms` serves them over three tables and one
  master key, the signing key's private half and the MAC key wrapped under
  the key's name as associated data, so a key wrapped for one tenant,
  algorithm or purpose does not open as another's, and a key wrapped under
  one master key does not open under another: `Decrypt`. A keyring's first
  key and its rotations are under the same advisory lock a tenant's KEK is,
  in a scope of the adapter's own (ADR-0014).
- The signature's byte form is part of the contract: a consumer that stores
  or transmits a `Signature.t` stores the algorithm and the version beside
  the bytes, and reads them back into one. `Mac.equal` compares in constant
  time.
- `mirage-crypto-ec` and `digestif` become dependencies of the package;
  `Key.random` is where the keys the libraries derive from a seed draw
  their bytes.
- Not done, as in the reference: a public key is not served by the port, a
  MAC is not verified by the port, an operator retires a version outside
  it, and ML-DSA waits for a Vault build and a library that have it. A
  signature Vault made verifies in this process under the public key
  Transit publishes, read as the model's `Public_key`, for both
  algorithms (`test_vault_signing.ml`): the two adapters' byte forms are
  one. The other direction, a key this process made verified by Vault,
  needs the key imported into Transit, which is BYOK, owed with the first
  tenant that brings a key.

## Alternatives rejected

**Signing by the consumer's own Vault client.** A second client for one
gap; the library exists so that the gap is filled once.

**Envelope: a signing seed wrapped by the KEK, signing in process.** Needs
no new port, and puts the signing key in every replica's memory and in a
general-purpose table, the posture the KMS is there for, given up.

**`sign` and `mac` on `Kms_port.S`.** The DEK store, the envelope stage and
`Cached` would each have to serve or refuse what they cannot.

**One port for signing and MACs.** No shared value type, no shared
consumer; a MAC is recomputed, a signature is verified.

**The MAC from the signing key's HMAC component, or from the KEK's.**
Transit allows it and it saves a key; it ties the open drafts to the
signing key's or the data's rotation.

**Telling the `400`s apart by Vault's text.** Works on this release, and
breaks silently on the next.

**A `Cached` functor over all four ports.** Every service behind a cache
would have to serve signing and MACs, including the one the sealing builds
per channel, which signs nothing.
