# KMS

Key management for envelope encryption. A tenant has a key-encryption key
(KEK), versioned and rotated, that wraps the data-encryption keys (DEKs) its
data is sealed with; deleting the tenant's KEKs is crypto-shredding. One
port and two adapters: keys in a PostgreSQL table under a master key, or in
HashiCorp Vault Transit. A port of the reference implementation in Rust,
itself a port of the Python and Go sources; the encryption stage ADR-0001
places before the outbox.

Four libraries: `ascetic_ddd.kms` is the model, the port and the cache, with
no service behind it; `ascetic_ddd.kms.pg` is the PostgreSQL adapter;
`ascetic_ddd.kms.vault` is the Vault Transit adapter over any HTTP client;
`ascetic_ddd.kms.vault.cohttp`, built only where `cohttp-eio` is installed,
is that client.

```ocaml
open Ascetic_kms

let ( let* ) = Result.bind

let example () =
  let* master_key = Algorithm.generate_key Algorithm.Aes_256_gcm in  (* from configuration, in practice *)
  let* master = Master_key.make master_key Algorithm.Aes_256_gcm in
  let* kek = Master_key.generate_kek master ~tenant_id:"tenant-1" in  (* version 1 *)
  let* dek, wrapped = Kek.generate_dek kek in           (* wrapped names the KEK's version *)
  let* rotated = Master_key.rotate_kek master kek in    (* version 2 *)
  let* rewrapped = Kek.rewrap rotated ~from:kek wrapped in
  let* again = Kek.unwrap rotated rewrapped in
  assert (Key.equal dek again);
  Ok ()
```

The service, inside the caller's transaction:

```ocaml
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool

(* How a failure of the session itself is carried in the scope's error. *)
let lift error = Kms_error.Session error

let example sessions master_key =
  let kms = Pg_kms.create master_key in
  Pool.session sessions ~lift (fun session ->
      let* () = Pg_kms.setup kms session in
      Session.atomic session ~lift (fun tx ->
          (* the KEK is made on first contact *)
          let* dek, wrapped = Pg_kms.generate_dek kms tx ~tenant_id:"tenant-1" in
          let* version = Pg_kms.rotate_kek kms tx ~tenant_id:"tenant-1" in  (* 2 *)
          (* what version 1 wrapped still unwraps, and moves under version 2 *)
          let* rewrapped = Pg_kms.rewrap_dek kms tx ~tenant_id:"tenant-1" wrapped in
          let* again = Pg_kms.decrypt_dek kms tx ~tenant_id:"tenant-1" rewrapped in
          assert (Key_version.to_int version = 2 && Key.equal dek again);
          Ok ()))
```

## The model

Three kinds of key and one operation between them. The `Master_key`, the one
key of the system, from configuration, wraps a tenant's key-encryption keys:
it makes the first `Kek` of a tenant, loads one back from its wrapped form,
and rotates one to the next version. A KEK wraps data-encryption keys:
`wrap`, `unwrap`, `rewrap` after a rotation, `generate_dek`. A `Wrapped_key`
names the version of the key that wrapped it, four big-endian bytes in front
on the wire, so the right version is reached for when it is unwrapped, and a
key asked to unwrap another version's work says so before it tries. The
tenant's id is the associated data of every wrapping, so a key wrapped for
one tenant does not unwrap under another tenant's key, even one of the same
bytes. A key made by a key is of its maker's kind: a KEK has the master
key's algorithm, a DEK the KEK's.

Under the hierarchy sits the primitive. A `Cipher.t` seals bytes under a key
and associated data, opens them again, and makes fresh keys for ciphers of
its own kind; it is a record of three functions, so a KEK's cipher, a
versioned cipher and a keyring are all ciphers to a codec that takes one.
`Algorithm.t` names the ciphers there are, AES-256-GCM, and makes one from a
key; what an algorithm knows, the sizes of its key, nonce and tag and how to
draw a key, lives with its adapter, `Aes256gcm`, over `mirage-crypto`. The
sealed form is `nonce || ciphertext || tag`. Sealing draws a fresh nonce
from the operating system; the half that takes the nonce is
`Aes256gcm.Known_answer.seal`, named for the one use it has, and is checked
against what the Python port sealed.

A `Key.t` is abstract and prints its length, never its bytes. A
`Key_version.t` is a number that fits the four bytes it travels in, checked
once where it comes in.

## The port and the adapters

`Kms_port.S` is the surface of Vault Transit: `encrypt_dek`, `decrypt_dek`,
`generate_dek`, `rotate_kek`, `rewrap_dek`, `delete_kek`, each in the
caller's session, whose type the adapter pins. A wrapped DEK is bytes opaque
to the caller, in the adapter's own form. What is generic over a service is
a functor over the port: `Cached.Make`, the DEK store, the envelope stage.

`Pg_kms` keeps KEKs in `kms_keys`, the table the Python, Go and Rust ports
write, so a key made by one port is read by another; a test unwraps what the
Python port wrapped. Each is wrapped by the master key the service was built
with. `setup` creates the table in one transaction under an advisory lock on
its name. Making a tenant's first key and rotating take an advisory lock on
the tenant in a scope of their own (ADR-0014): two callers meeting a new
tenant at once make one key, and two rotations at once make versions two and
three, whether or not the caller has a transaction open; reads take nothing.
READ COMMITTED is assumed.

The PostgreSQL adapter keeps KEKs in a general-purpose database, wrapped by
a master key the process holds; it is the simple adapter, not key management
hardware. The master key is thirty-two bytes and comes from a secret manager
or the environment, never from source or configuration under version
control; the session may belong to a database other than the data's. Where
the requirements are higher, Vault Transit or a cloud KMS behind the same
port is the answer.

`Cached.Make (Kms)` keeps what any service unwraps, a thousand keys for five
minutes unless told otherwise, so a consumer opening a thousand messages
sealed under one DEK, or a store loading every version of a resource's keys,
asks the KMS once: what makes Vault, a network away, bearable on a hot path.
Deleting a tenant's KEK forgets the tenant's keys held here; elsewhere the
time to live is the bound, so a shredded tenant's keys open for that long at
most, which is the price, with keys held in memory for that long. A refusal
is not kept. The clock is the caller's, an `Eio.Time.Mono`.

`Vault_transit.Make (Transport)` leaves keys and cryptography to Vault; the
wrapped DEK is Vault's ciphertext text, `vault:v1:...`, as bytes. The HTTP
client is the session's, a `Rest_session` of `ascetic_ddd.session.rest`, and
every call goes through the session so that its observer times it; the
client speaks to Vault through a `Vault_transport.S`, one function, `send`.
`Cohttp_transport` implements it over `cohttp-eio`; whether the client
speaks TLS is the application's choice, the `https` argument the client is
made with. A tenant's id becomes the Transit key's name, percent-encoded.
`404` is `Kek_not_found`; any other refusal is `Vault`, with the status and
what Vault said.

## Errors

`Kms_error.t` is a value: a key that is not there, a ciphertext that does
not open, a version that is not the key's or not at hand, an algorithm this
library has not, bytes that are not what they were taken for, no randomness,
and the failures of the session, the database and Vault, the first two as
the session library carries them, without a driver type.

## Deviations from the reference

* A key is not wiped when dropped. The runtime gives no way to: a string is
  immutable and the collector may have copied it. `Key.t` is abstract, is
  never printed, and is compared in time that depends on its length alone.
* A version is a type of its own, `Key_version.t`, where the reference has
  `u32`: an OCaml `int` is wider than the four bytes a version travels in.
* A `Kek` is made and wrapped at once, or unwrapped from the form it
  carries, `Kek.generate` and `Kek.load`; there is no constructor that takes
  a key and a wrapped form side by side, which the reference keeps private
  to its module and OCaml could not. `Master_key` is who calls them.
* `Aes256gcm` refuses a key of sixteen or twenty-four bytes, which
  `mirage-crypto` would take and quietly be AES-128 or AES-192, and a nonce
  of any length but twelve.
* Randomness is read from the operating system at each call,
  `Mirage_crypto_rng_unix.getrandom`, as the reference reads `getrandom`;
  the library's seeded generator is not used, so there is none to
  initialize.
* Making a tenant's first key and rotating run in a scope of their own; the
  reference takes the lock in the caller's transaction and has none of its
  own (ADR-0014).
* The port is a signature and what is generic over it a functor, where the
  reference has a trait and generics. The cache takes its clock from the
  caller.
* The Vault adapter has tests against a scripted transport beside those
  against a dev server, so that what it asks of Vault is checked where there
  is no Vault.

## Testing

```sh
dune test test/kms                       # keys, ciphers, the cache, the Vault adapter scripted
docker compose up -d postgres vault
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
export TEST_VAULT_ADDR=http://localhost:58200 TEST_VAULT_TOKEN=test-root-token
dune test test/kms                       # and PostgreSQL, and a Vault dev server
```

The Vault tests enable the `transit` engine themselves. Without the
variables the integration tests are skipped; without `cohttp-eio` the ones
against Vault are not built.
