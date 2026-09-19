# DEK

Data-encryption keys, one per resource and versioned, wrapped by the
tenant's key-encryption key through `ascetic_ddd.kms`: the store that keeps
them, and the ciphers a codec seals with. The lower half of envelope
encryption, the KMS being the upper, for whatever a repository keeps under a
key of its own: an aggregate's stream in an event store, a document. A port
of the reference implementation in Rust, itself a port of the `DekStore` of
the Python and Go sources, with the resource named instead of the stream.

Three libraries: `ascetic_ddd.dek` is the model and the port;
`ascetic_ddd.dek.pg` is the store over PostgreSQL;
`ascetic_ddd.dek.envelope` is the sealing stage of the bus.

```ocaml
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Deks = Ascetic_dek_pg.Pg_dek_store.Make (Pg_kms)
open Ascetic_dek

(* How the session's and the KMS's failures are carried in the store's error. *)
let lift error = Dek_error.Session error
let of_kms result = Result.map_error (fun error -> Dek_error.Kms error) result

let example sessions master_key =
  let deks = Deks.create (Pg_kms.create master_key) in
  let order = Resource.make ~tenant_id:"tenant-1" ~kind:"Order" (`String "order-7") in
  Pool.session sessions ~lift (fun session ->
      let* () = of_kms (Pg_kms.setup (Deks.kms deks) session) in
      let* () = Deks.setup deks session in
      Session.atomic session ~lift (fun tx ->
          let* cipher = Deks.get_or_create deks tx order in      (* the DEK is made on first contact *)
          let* sealed = of_kms (Versioned_cipher.encrypt cipher "the order's events") in
          let* keyring = Deks.get_all deks tx order in           (* every version, for reading back *)
          let* opened = of_kms (Keyring.decrypt keyring sealed) in
          assert (opened = "the order's events");
          Ok ()))
```

## The model

A DEK belongs to a `Resource.t`: one thing of one tenant, named by its kind
and its id, the id as JSON so a composite id fits. The resource's canonical
text, the three as a JSON array with object keys in order, is the associated
data of its ciphers: a ciphertext of one resource does not open under
another resource's key, even one of the same bytes, and the text must never
change for a resource that has data. `Canonical` writes that text, and
writes it as the reference port does, byte for byte, so that what one port
sealed another opens; a test holds it to what the reference's JSON library
wrote.

A codec sees a resource's DEKs as ciphers over bytes that carry the key
version along, four big-endian bytes in front, the layout of the sources and
of a wrapped key in the KMS. A `Versioned_cipher.t` is one version: it seals
under that version and refuses another version's work before the key is
tried, and is what the write path gets. A `Keyring.t` is every version the
resource has: it seals under the newest and opens whatever version a
ciphertext names, and is what the read path gets. Either is a
`Ascetic_kms.Cipher.t` to a codec, through `cipher`. The version is read
here, in memory, to pick a key; the KMS reads it to fetch one from its
store.

## The port and the adapter

`Dek_store.S` in the caller's session: `get_or_create` for the write path,
making version 1 on first contact; `get_all` for the read path; `get` for a
version already known; `rewrap` after the tenant's KEK rotated, wrapping the
tenant's DEKs again and saying how many; `delete` to forget one resource for
good. Deleting the tenant's KEKs in the KMS forgets every resource of the
tenant at once.

`Pg_dek_store.Make (Kms)` keeps DEKs in `deks`, each wrapped by the tenant's
current KEK through the KMS it is built with, and labelled with the DEK's
algorithm, AES-256-GCM unless told otherwise. The id is kept as JSONB and
compared as JSONB, so a composite id is found however it was built. `setup`
creates the table in one transaction under an advisory lock on its name;
making a resource's first DEK takes an advisory lock on the resource in a
scope of its own (ADR-0014), so two callers meeting a new resource at once
make one key, whether or not they have a transaction open. Reads take
nothing. READ COMMITTED is assumed.

## The envelope stage of the bus

`Envelope_stage.Make (Sessions) (Kms)` is the sealing stage ADR-0001 places
before the outbox and after the inbox: a fresh DEK per message, drawn for
the message's tenant through the KMS, seals the payload; the DEK travels in
the `dek` header, wrapped by the tenant's KEK, base64, with the cipher's
name in `dek_algorithm`. The receiving side needs no table of keys, only a
KMS that holds the tenant's KEK, which is what lets a message cross to
another bounded context. The stage reaches the KMS through a session pool of
its own, since the KMS's session is not the data's. The payload is bound to
the message's identity: its associated data is the canonical text of the
`tenant_id` and `message_id` headers, unless `bound_to` names others, so a
payload moved under another message's headers does not open while the same
message delivered again does; the names travel in `dek_bound_to`, and the
opening side reads them from there. A message missing a header it is bound
to, or one whose key or payload does not open, or whose tenant's KEK is
gone, is refused for good, `Failure.Permanent`, and the inbox parks it; a
KMS out of reach is a failure of the moment, and the message is tried again.

A fresh DEK per message is one KMS call per message on each side. Where the
KMS is a network away, `reusing stage ~clock reuse` keeps a tenant's DEK at
the sealing side for so many messages or so long, a thousand or a minute by
`Reuse.default`, far under what NIST allows a key with random nonces, and
`Ascetic_kms.Cached` in front of the opening side's KMS unwraps each key
once. Both are off unless asked for.

```ocaml
module Sealing = Ascetic_dek_envelope.Envelope_stage.Make (Pool) (Pg_kms)

let sealing = Sealing.stage (Sealing.create kms_sessions kms)
let placed = Transactional.Producer.through (Outbox_channel.producer outbox ~destination ~encode) sealing
let orders = Transactional.Consumer.through (Inbox_channel.consumer ~sw ~clock inbox ~decode) sealing
```

## What is not here

Nothing is cached: every call reads the resource's rows and unwraps them
through the KMS. A second DEK version for a resource is never made here:
versions are for an algorithm migration, and the row that starts one is the
migration's to write. The codecs that compose a cipher with serialization
belong to the repository that chains them.

## Deviations from the reference

* An id has no float, by its type: `Resource.id` is a subtype of
  `Yojson.Safe.t` without it. A number with more than one spelling has no
  one text, and equality of floats is not identity. An integer is an OCaml
  `int`; an id beyond its range is text.
* A field of an id given twice counts once, the last one: an association
  list can say what a map cannot.
* Making a resource's first DEK runs in a scope of its own; the reference
  takes the lock in the caller's transaction and has none of its own
  (ADR-0014).
* The port is a signature and the store and the stage are functors over the
  KMS, where the reference has traits and generics. The stage takes its
  clock from the caller, and only when it reuses keys.

## Testing

```sh
dune test test/dek                       # the model, no database
docker compose up -d postgres
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
dune test test/dek                       # and the store and the stage on PostgreSQL
```
