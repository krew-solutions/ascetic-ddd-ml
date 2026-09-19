# ADR-0013: Envelope encryption stands on mirage-crypto, behind signatures

## Status

Accepted (2026-09-19).

## Context

ADR-0001 places encryption before the outbox and after the inbox. The
reference implementation has the two halves of it: a key management service
(a master key over a tenant's versioned key-encryption keys, those over
data-encryption keys, in a PostgreSQL table or in HashiCorp Vault Transit),
and data-encryption keys per resource with a sealing stage for the bus. They
are ported here as `ascetic_ddd.kms` and `ascetic_ddd.dek`.

What another port wrote must open here, and the other way round: the sealed
form is `nonce(12) || ciphertext || tag(16)` under AES-256-GCM, a wrapped
key is a version in four big-endian bytes and then that form, the tenant's
id is the associated data of every wrapping, and the associated data of a
resource's ciphers is a canonical JSON text. Those bytes are fixed by the
sources. What is open is how the model stands in OCaml, where the reference
leans on what Rust gives it: `u32`, a trait object for a cipher, traits with
generics for the services, module-private constructors, memory wiped on
drop, `getrandom`.

## Decision

1. **The cipher is `mirage-crypto`'s `AES.GCM`, behind one adapter,
   `Aes256gcm`**, which refuses what the library would let through: a key of
   sixteen or twenty-four bytes, which it would take and quietly be AES-128
   or AES-192, and a nonce of any length but twelve. The half that takes the
   nonce is `Aes256gcm.Known_answer.seal`, named for its one use.
2. **Randomness is read from the operating system at each call**,
   `Mirage_crypto_rng_unix.getrandom`, for keys and nonces alike. The
   library's seeded generator is not used.
3. **A cipher is a value**, `Cipher.t`, a record of three functions, so a
   KEK's cipher, a versioned cipher and a keyring are all ciphers to a codec.
   **A service is a signature**, `Kms_port.S` and `Dek_store.S`, as the
   outbox's and the inbox's ports are, and what is generic over a service is
   a functor: `Cached.Make`, `Pg_dek_store.Make`, `Envelope_stage.Make`,
   `Vault_transit.Make` over its transport.
4. **A version is a type of its own**, `Key_version.t`: a number that fits
   the four bytes it travels in, checked once where it comes in.
5. **A KEK is made and wrapped at once, or unwrapped from the form it
   carries**, `Kek.generate` and `Kek.load`, each given the wrapping as a
   function by `Master_key`. No constructor takes a key and a wrapped form
   side by side.
6. **The id of a resource has no float, by its type**: `Resource.id` is a
   subtype of `Yojson.Safe.t` without it. `Canonical` writes the text itself
   rather than through the JSON library, to the reference's rules.
7. **The Vault adapter is over the REST session**, ported for it as
   `ascetic_ddd.session.rest`, and over a transport of one function;
   `cohttp-eio` behind it is an optional sub-library, as the Kafka client
   is.
8. **`ascetic_ddd.encryption` stays as it is**, named in the README as what
   these two succeed. Nothing in the repository uses it.

## Objections considered

1. *Whether `mirage-crypto` writes the bytes the other ports write is an
   assumption until shown.* It was shown: the adapter seals the Python
   port's known answer byte for byte under the same key, nonce and
   associated data, opens it, loads the two KEKs the Python port wrapped and
   unwraps its DEKs with them; and a row the Python port wrote is read from
   the table. The tag is appended to the ciphertext by the library, which is
   the layout.
2. *A seeded generator is the library's recommended way; bypassing it looks
   like a shortcut.* The recommended way is a global: `use_default ()` must
   have run before the first key is drawn, and if it has not, the failure is
   an exception at the first use, in whatever fiber meets it. Reading the
   operating system at each call has no such precondition, fails as a value,
   `Entropy`, and is what the reference does. The cost is a system call per
   nonce, which is small beside the database or network round trip every
   operation here makes anyway.
3. *A key should be wiped when dropped, as the reference wipes it; an
   abstract type is not that.* It is not, and cannot be: a string is
   immutable, the collector may have copied it, and the library's key
   schedule holds a copy of its own. A `Bytes.fill` on one copy would claim
   what it does not deliver. What can be done is done: the type is abstract,
   prints its length only, is compared in time that depends on its length
   alone, and leaves through one function. The README says so under
   deviations. A deployment that needs more keeps its KEKs in Vault, where
   they never enter this process.
4. *Functors make the composition root heavier than generics do.* Four
   lines: the KMS, the cache over it, the store over that, the stage over a
   pool and that. `Composite_session.Make` is already applied the same way.
   A record of functions, as the bus's `Adapter.t` is, was the alternative;
   it would have parted from the outbox's and the inbox's ports, and lost
   the adapters' own operations, `setup`, `inner`, `length`, behind the
   record.
5. *Writing JSON by hand, beside a JSON library, invites a divergence.* The
   library's writer is the divergence: it escapes DEL where the reference's
   does not, and a canonical text must not move with a dependency's version.
   The rules are five lines, and a test holds them to a string the
   reference's JSON library actually wrote, taken from a run of it: every
   byte below U+0020, a quote, a backslash, DEL, a slash and text that is
   not ASCII.
6. *Leaving floats out of an id narrows what the reference accepts.* It
   does. A number with more than one spelling, `1`, `1.0`, `1e0`, has no one
   text, the two languages print floats differently, and the mismatch would
   be silent: a payload that does not open. An identifier that is a float is
   a defect in the model that has one; the type says so at compile time
   instead of a ciphertext saying so in production.

## Consequences

- New dependencies: `base64`, for the wrapped key in a header and for
  Vault's fields, which nothing here required before; `eqaf` and `mtime`,
  named now and already in the closure of `mirage-crypto` and of `eio`;
  `cohttp-eio` as an optional one, installed for the tests.
- `Session_observer.scope_kind` gains `Logical`, for the scopes of a session
  with no transaction behind it.
- `docker-compose.yml` and the workflow have a Vault dev server; the Vault
  tests enable the `transit` engine themselves.
- The tests of the reference are ported by name, with these beside them:
  the edges the OCaml adapter adds (a nonce of the wrong length, a version
  beyond four bytes), the cache's time to live on a mock clock, the Vault
  adapter against a scripted transport, a composite id found however it was
  built, and the first contacts of ADR-0014.
- A payload sealed by the Python port under a resource does not open here,
  nor in the reference: the sources bind with `str(stream_id)`, the
  reference and this port with the canonical text. Its wrapped DEKs do
  load.
