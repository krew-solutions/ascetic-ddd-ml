# KMS

Key management for a tenant's keys. For envelope encryption: a tenant has a
key-encryption key (KEK), versioned and rotated, that wraps the
data-encryption keys (DEKs) its data is sealed with; deleting the tenant's
KEKs is crypto-shredding. For what a tenant signs and authenticates:
digital signatures under the tenant's key of an algorithm, at the latest
version or at one named, the public halves of those keys for verifiers
outside the KMS, and MACs under the tenant's key for a purpose. And custody
of each kind of key, apart from its use: a port per kind, beside the use
port. Seven ports, two adapters serving all seven: keys in PostgreSQL
tables under a master key, the signing and the MACs done in this process;
or in HashiCorp Vault Transit, where the keys stay and the cryptography is
Vault's. In-memory keys serve the signing and MAC ports and their custody
for the tests of what consumes them. A port of the reference
implementation in Rust, itself a port of the Python and Go sources, which
have the KEK port only; the encryption stage ADR-0001 places before the
outbox.

Four libraries: `ascetic_ddd.kms` is the model, the ports, the cache and
the in-memory keys, with no service behind it; `ascetic_ddd.kms.pg` is the
PostgreSQL adapter; `ascetic_ddd.kms.vault` is the Vault Transit adapter
over any HTTP client; `ascetic_ddd.kms.vault.cohttp`, built only where
`cohttp-eio` is installed, is that client.

```ocaml
open Ascetic_kms

let ( let* ) = Result.bind

let example () =
  let* master_key = Algorithm.generate_key Algorithm.Aes_256_gcm in  (* from configuration, in practice *)
  let* master = Master_key.make master_key Algorithm.Aes_256_gcm in
  let* kek = Master_key.generate_kek master ~tenant_id:"tenant-1" in  (* version 1; no derivation context *)
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
          (* the KEK is made on first contact; custody would make it, `Pg_kms.create_kek` *)
          let* dek, wrapped = Pg_kms.generate_dek kms tx ~tenant_id:"tenant-1" in
          let* version = Pg_kms.rotate_kek kms tx ~tenant_id:"tenant-1" in  (* 2: custody's, on the adapter *)
          (* what version 1 wrapped still unwraps, and moves under version 2 *)
          let* rewrapped = Pg_kms.rewrap_dek kms tx ~tenant_id:"tenant-1" wrapped in
          let* again = Pg_kms.decrypt_dek kms tx ~tenant_id:"tenant-1" rewrapped in
          assert (Key_version.to_int version = 2 && Key.equal dek again);
          Ok ()))
```

## The model

Three kinds of key and one operation between them, and beside them a
tenant's other keys. The `Master_key`, the one key of the system, from
configuration, wraps a tenant's key-encryption keys: it makes the first
`Kek` of a tenant, loads one back from its wrapped form, and rotates one to
the next version. A KEK wraps data-encryption keys: `wrap`, `unwrap`,
`rewrap` after a rotation, `generate_dek`. A `Wrapped_key` names the
version of the key that wrapped it, four big-endian bytes in front on the
wire, so the right version is reached for when it is unwrapped, and a key
asked to unwrap another version's work says so before it tries. The
tenant's id is the associated data of every wrapping, so a key wrapped for
one tenant does not unwrap under another tenant's key, even one of the same
bytes; a KEK serving a derivation context binds its wraps to the context
too, the tenant's id length-prefixed and then the context, `Kek.binding`
(ADR-0024). A key made by a key is of its maker's kind: a KEK has the
master key's algorithm, a DEK the KEK's.

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

A tenant's other keys are one version of a keyring each, as the `Kek` is: a
`Signing_key` of a `Signature_algorithm`, its private half thirty-two
bytes, its `Public_key` derived from it, makes a `Signature` of its
algorithm and version, and the public half verifies one; a
`Public_keyring` is the public halves of the versions that verify, by
version, never empty, of one algorithm, naming no version as the one that
signs (ADR-0035); a `Mac_key` for a purpose, a `Key_label`, makes a `Mac`. They take the primitives from the
libraries as the cipher does, Ed25519 and ECDSA P-256 with SHA-256 from
`mirage-crypto-ec`, HMAC-SHA-256 from `digestif`, in the forms the
algorithm fixes, and each knows its name, `{tenant}.sign.{algorithm}` or
`{tenant}.mac.{purpose}`, which is Transit's key name on one adapter and
the associated data of the wrapping on the other (ADR-0023).

A `Key.t` is abstract and prints its length, never its bytes. A
`Key_version.t` is a number that fits the four bytes it travels in, checked
once where it comes in; `Key_version.from_one` is the check a version a
caller names must pass, since version zero is on the wire and no key's.

## The ports and the adapters

The use ports hold use alone; the custody of each kind of key is a port
beside its use port, held by the tenant-management context, or an
operator's procedure, and by nobody who merely uses the key (ADR-0024,
ADR-0036).

`Kms_port.S` is the surface of Vault Transit: `encrypt_dek`, `decrypt_dek`,
`generate_dek`, `rewrap_dek`, each in the caller's session, whose type the
adapter pins. A wrapped DEK is bytes opaque to the caller, in the adapter's
own form. What is generic over a service is a functor over the port:
`Cached.Make`, the DEK store, the envelope stage. `Key_custody.S` is
custody of the KEK: `create_kek` with `Kek_options.t`, `derived` and
`auto_rotate_period`, `rotate_kek`, `retire_kek_versions_before`,
`delete_kek`.

`Signing_port.S` is signatures under the tenant's key of one algorithm,
one key per tenant and algorithm, `Signature_algorithm.t`: Ed25519, ECDSA
P-256 with SHA-256. `sign` takes the latest version and makes the key on
first signing unless the adapter's keys are managed elsewhere;
`sign_with_version` takes the version named and makes nothing: for a
message that names its key's version inside what is signed, a JWS's `kid`,
and for a verifier that holds the public keys itself and refuses a version
until it is given it; it is also the one signing that runs under an
identity that may not make keys (ADR-0035). `verify` verifies under the
version the signature names and never makes a key; every version verifies
until an operator retires it. The algorithm is an input of signing and a
property of the `Signature.t`, which carries it, the version of the key
that made it, and the bytes in the one form fixed per algorithm, Ed25519's
sixty-four, ECDSA's `r || s`, never DER, so a tenant moves to another
algorithm by signing with it, a new key rather than a rotation, while what
the old key signed keeps verifying. Verifying answers `Ok ()` or
`Invalid_signature`; a key the tenant has not got is
`Signing_key_not_found`, a version the key has not got or no longer serves
`No_key_of_version`. `Signing_key_custody.S` is `create_signing_key` with
`Key_options.t`, `rotate_signing_key`, `retire_signing_versions_before`,
`delete_signing_key`; deleting a signing key ends the verifiability of
everything it signed: the tenant's off-boarding, never a rotation.

`Public_key_port.S` is `public_keys`, the `Public_keyring.t` of the
tenant's key of one algorithm: the public halves of the versions that
verify, for a JWKS, a gateway's configuration, a device's key directory. A
query beside `Signing_port.S`, so that an adapter that only publishes keys
is bound by reading them. On PostgreSQL each public half is derived from
its wrapped private half and checked against the column, so that what is
published is what the master key's holder made; the keyring does not
verify, and names no newest (ADR-0035).

`Mac_port.S` is HMAC-SHA-256 under the tenant's key for one purpose, a
`Key_label.t` such as `draft-etag`, `mac`, one key per tenant and purpose,
made on first use unless the keys are managed elsewhere; a `Mac.t` carries
the version of the key that made it, and is checked by recomputation, so
the port only makes them. `Mac_key_custody.S` is `create_mac_key`,
`rotate_mac_key`, `retire_mac_versions_before`, `delete_mac_key`. Keeping
MAC keys apart from signing keys by name keeps their lifecycles apart: a
signing rotation does not change what a MAC key makes. Why the keys are
one per use, and what that costs at off-boarding, is ADR-0023.

The rules of every custody port are one body (ADR-0036): making is
idempotent and typed, answering the latest version, a key at the name of
another type or another `derived` being `Conflict`, a rotation period the
PostgreSQL adapter and the in-memory keys refuse as `Unsupported`, having
no scheduler; a rotation never makes the first version, a tenant without
the key being `*_not_found`, and is not idempotent; a retirement's bound is
the oldest version kept, the latest never going, a bound above it
`No_key_of_version` and nothing retired, a bound at or below the floor
nothing done, so a stale or a retried retirement is harmless; a deletion of
nothing is not an error. What a retirement does to the material differs:
PostgreSQL deletes the rows, and nothing brings them back; Vault raises a
floor the adapter never lowers and keeps the material until the key is
trimmed, which no port does. A custody operation refused under the
identity is `Forbidden`, naming it.

`Pg_kms` serves every port over three tables and one master key. KEKs in
`kms_keys`, the table the Python, Go and Rust ports write, so a key made by
one port is read by another; a test unwraps what the Python port wrapped.
Signing keys in `kms_signing_keys` by tenant, algorithm and version, the
private half wrapped by the master key under the name
`{tenant}.sign.{algorithm}` as its associated data, the public half beside
it, never trusted on its own: verifying and publishing derive the public
half from the private one and check the column against it, a mismatch
being `Malformed`, a tamper refused rather than verified under (ADR-0035);
MAC keys in `kms_mac_keys` by tenant, purpose and version, wrapped under
`{tenant}.mac.{purpose}`. The signing, verifying and MACs are the model's,
in this process, in the byte forms the ports fix, the same the Vault
adapter asks Transit for. `setup` creates the three tables in one
transaction under an advisory lock on each name, in one order, since two
services may share one of the tables and not the others. Making a
keyring's first key, rotating and retiring take an advisory lock on the
tenant, or on the key's name, in a scope of their own (ADR-0014): two
callers meeting a new keyring at once make one key, and two rotations at
once make the next two versions, whether or not the caller has a
transaction open; reads take nothing. READ COMMITTED is assumed. A key
wrapped under one master key does not open under another: `Decrypt`, on a
signature, a verification and the keyring alike.

The PostgreSQL adapter keeps keys in a general-purpose database, wrapped by
a master key the process holds; it is the simple adapter, not key
management hardware. The master key is thirty-two bytes and comes from a
secret manager or the environment, never from source or configuration
under version control; the session may belong to a database other than the
data's. Where the requirements are higher, Vault Transit or a cloud KMS
behind the same ports is the answer.

Two settings of either adapter serve the bus, and a holder of keys
(ADR-0024, ADR-0036). Built with a `derivation_context`, the service
serves a derived key: on Vault every KEK operation carries the context,
base64, and a KEK the service makes is `derived`; on PostgreSQL the context
is bound into the wrap's associated data beside the tenant. Either way a
DEK wrapped for one channel does not unwrap for another, and the
composition root builds one instance, one cache and one stage per channel,
from the channel's name in the contract, `Sealing` in
`ascetic_ddd.dek.envelope`. Built with `managed_keys`, the service makes
no key of any kind on first use: a tenant without one is `Kek_not_found`
on every wrap, `Signing_key_not_found` on `sign`, `Mac_key_not_found` on
`mac`, and the key is made by whoever has custody; on Vault the holder's
token then holds `read` on the keys and `update` on the operation's path
alone, and every custody operation under it is `Forbidden`. On Vault a
refusal is read once, never by Vault's text: of a KEK operation, a `400`
is the key missing, `Kek_not_found`, a version retired,
`No_key_of_version`, or, the key there and the version served, the
request's own fault, `Decrypt` on `decrypt` and `rewrap`, a wrong context
or bytes changed, `Malformed` elsewhere; of signing, verifying or a MAC,
on a `400`, a `403` or a `500`, the key missing is its absence, a version
the key does not sign with, or, verifying, does not serve, is
`No_key_of_version`, an unexplained `400` `Malformed`; a `403` with the key
there is `Forbidden`, which no retry under the same identity and context
mends.

`Cached.Make (Kms)` keeps what any service unwraps, a thousand keys for five
minutes unless told otherwise, so a consumer opening a thousand messages
sealed under one DEK, or a store loading every version of a resource's keys,
asks the KMS once: what makes Vault, a network away, bearable on a hot path.
Deleting a tenant's KEK forgets the tenant's keys held here; elsewhere the
time to live is the bound, so a shredded tenant's keys open for that long at
most, which is the price, with keys held in memory for that long. A refusal
is not kept. The clock is the caller's, an `Eio.Time.Mono`.
`Cached.With_custody (Kms)` is the same cache over a service with custody
of the KEK: custody passes through, and a retirement or a deletion forgets
the tenant's keys held here. The signing, MAC and public key ports are not
served by the cache: a signature, a verification and a keyring are not
functions of its key, and the adapter is called directly for them.

`Vault_transit.Make (Transport)` leaves keys and cryptography to Vault; the
wrapped DEK is Vault's ciphertext text, `vault:v1:...`, as bytes. The HTTP
client is the session's, a `Rest_session` of `ascetic_ddd.session.rest`, and
every call goes through the session so that its observer times it; the
client speaks to Vault through a `Vault_transport.S`, one function, `send`.
`Cohttp_transport` implements it over `cohttp-eio`; whether the client
speaks TLS is the application's choice, the `https` argument the client is
made with. The same functor serves every port over one mount, telling a tenant's keys
apart by name: the KEK is the tenant's id, percent-encoded, what the Python
and Go ports name; a signing key `{tenant}.sign.{algorithm}`, Transit
types `ed25519` and `ecdsa-p256`; a MAC key `{tenant}.mac.{purpose}`, type
`hmac`. The dot is the separator Transit allows that the parts cannot
contain, so for the dotted names the tenant's id is checked to be letters,
digits, `_` and `-` rather than encoded: a `/` is "unsupported path" to
Transit even encoded. Signatures are asked for and verified in JWS
marshaling, which is what makes ECDSA's bytes `r || s`; a pinned signature
carries its `key_version`, and the version Transit answers is checked
against it. The public keys are `GET keys/{name}`'s
`data.keys.{n}.public_key` of the versions served, base64 of the raw bytes
for Ed25519, a PEM SubjectPublicKeyInfo for P-256, read by the fixed DER
prefix of an uncompressed point rather than by a parser. Custody is `POST
keys/{name}`, read back and held to what was asked, `POST
keys/{name}/rotate`, which Transit refuses for a key it has not got, and
`config` with a `min_decryption_version` written only when higher than the
one read, a signing floor of the key's own raised with it.

`In_memory_keys.Make (Session)` serves the signing, public key and MAC
ports and the custody of the signing and MAC keys from memory, with the
model's keys, a key registered from a seed so that a test's signatures are
the same from run to run, Ed25519, or made on first signing as Vault would,
MAC keys made on first use, or, built with `managed_keys`, made through
custody alone, so that the adapter a consumer writes over the ports is
exercised as it is in production, with the network behind the port and
nothing else replaced. A ring keeps its versions by number and the last
version made, so a retired version is never numbered again. It does not
serve the KEK port: the stage's and the custody's tests run on the
PostgreSQL adapter, the simple one already. The custody rules are one test
body, `test/kms/custody/custody_rules.ml`, a functor over the ports, run on
the in-memory keys, PostgreSQL and Vault alike.

## Errors

`Kms_error.t` is a value: a key that is not there, of either kind; a
ciphertext that does not open; a version that is not the key's or not at
hand; a signature that is not the key's; an algorithm this library has not;
bytes that are not what they were taken for; a key that exists otherwise, an
operation the KMS forbids, an option the adapter cannot execute; no
randomness; and the failures of the session, the database and Vault, the
first two as the session library carries them, without a driver type.

## What is not here

A MAC is not verified by the port; a keyring does not verify, and names no
version as the one that signs, which is the signer's configuration; the
import of a tenant's own key material waits for the first tenant that
brings one; a trim of the material Vault keeps below the floor, and a floor
on PostgreSQL, wait for the first custodian's procedure that needs a
retirement undone; ML-DSA waits for a Vault build and a library that have
it. Each comes with its first user. The data-encryption keys themselves,
and the stage that seals with them, live in `ascetic_ddd.dek`.

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
* The ports are signatures and what is generic over them a functor, where
  the reference has traits and generics. The cache takes its clock from the
  caller, and serves the KEK port, with its custody through
  `With_custody`, not the signing, public key and MAC ports, which the
  reference's cache passes through because one generic value stands there
  for every port.
* A version a caller names for a pinned signature is checked to be from
  one by `Key_version.from_one` at each adapter's door, where the
  reference has a non-zero integer type; OCaml has none, and
  `Key_version.t` admits the zero the wire carries.
* Transit's P-256 public key, a PEM SubjectPublicKeyInfo, is read by its
  fixed DER prefix, where the reference links a DER parser: the OCaml
  parser, `x509`, would bring `zarith` into every build for twenty-six
  constant bytes, and stays a test dependency, where the test decodes the
  same PEM independently of the code under test (ADR-0035).
* `Key_options.auto_rotate_period` is seconds, as `Kek_options`'s is.
* `Signing_key.sign` and `Mac_key.mac` are total: the private half is parsed
  once, when the key is made or loaded, and kept in the value, where the
  reference parses it again at every signing and returns a `Result`.
* `Kek_options.auto_rotate_period` is seconds, as every duration here is;
  the reference has a `Duration`.
* The Vault adapter has tests against a scripted transport beside those
  against a dev server, so that what it asks of Vault is checked where there
  is no Vault.

## Testing

```sh
dune test test/kms                       # keys, ciphers, the cache, the in-memory keys, the Vault adapter scripted
docker compose up -d postgres vault
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
export TEST_VAULT_ADDR=http://localhost:58200 TEST_VAULT_TOKEN=test-root-token
dune test test/kms                       # and PostgreSQL, and a Vault dev server: KEKs, custody, signing, MACs
```

The Vault tests enable the `transit` engine themselves. Without the
variables the integration tests are skipped; without `cohttp-eio` the ones
against Vault are not built.
