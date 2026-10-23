# ADR-0035: A signature whose version something outside the KMS already holds is made at that version and makes no key; the public keys of the versions that verify are read through a port of their own, derived where the adapter holds the private halves

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0019, 2026-10-18, amended 2026-10-23
twice: a refusal of `verify` read as a pinned signature's, and the
alternatives to deriving the public half weighed), whose measurements on
Vault 1.21.4 are the facts below; this port's live tests ran on the Vault
1.17.6 the `docker-compose.yml` of the day named and found the same
answers, and run on 1.21.4 since the same day, the image moved to the
`1.21` line with this decision. Held by
`test/kms/vault/test_vault_transit.ml` (the adapter's reading of Transit's
answers against a scripted transport: a pinned signature asks its version
and no `POST keys`, a signature by another version is refused, every
measured refusal is explained by one read, a refusal of `verify` read of
the versions that verify, the public keys read from PEM and base64 against
signatures Transit made and openssl verified), `test/kms/vault/test_vault_signing.ml`
(the same on a Vault dev server: the earlier version signs when it is
named, a pinned signature makes no key, a version above the latest, retired
or below the key's signing floor is named, the keyring is what Transit
publishes for both algorithms, a holder's token signs at the version
named and is refused `verify`), `test/kms/test_pg_signing.ml` (the
PostgreSQL adapter: the earlier version signs when named, a pinned
signature makes nothing, the keyring is the versions there are, a keyring
under another master key is `Decrypt`, a public half replaced in the table
is refused), `test/kms/test_in_memory_keys.ml` (the in-memory keys pin,
retire and publish as the adapters do, a retired number is never used
again) and `test/kms/test_domain.ml` (a P-256 public key is held
uncompressed).

Amends ADR-0023: point 5 (`sign` makes the key, `sign_with_version` never
does), point 6 (a refusal of `verify` is read on a `403` and a `500` as on a
`400`, and what the read does not explain is the request's own fault, not
left to Vault), and the consequence "a public key is not served by the
port", which this decision supersedes.

Amended the same day by ADR-0036: the custody of signing keys is a port
beside `Signing_port.S`, `managed_keys` covers `sign` and `mac`, and the
in-memory keys retire through that port.

## Context

`Signing_port.S` signed with the latest version of a tenant's key of an
algorithm and made the key on first signing (ADR-0023 point 5), and the
public half of a version was left to "a verifier elsewhere" reading
Transit or the table itself (ADR-0023's consequences: "not served by the
port"). The reference's first users needed both, and found four more
things, each of which holds of this port as it stood:

- **A message that names its key's version inside what is signed cannot
  be signed with "the latest".** A JWS's header, `alg`, `kid`, `typ`, is
  part of the signing input (RFC 7515 § 7.1), and its `kid` names the
  version. The version must be known before the signature is made; a
  rotation between a read of "the latest" and the signature makes a token
  whose `kid` names one version and whose signature is another's.
- **A verifier that holds the public keys refuses a version until it is
  given it.** A gateway verifies against a JWKS pushed with its
  configuration and never refetches on an unknown `kid`; its rotation is
  four steps: rotate in the KMS, push both public versions, raise the
  version the issuer signs with, retire the old one after the token's
  maximum lifetime. With the latest signing, step one makes every new token
  one the gateways refuse until step two lands everywhere.
- **The latest-signing could not run under the identity the signer is
  given.** A minting token holds `update` on the key's `sign` path and
  `read` on the key, nothing else. `sign`'s first call was `POST
  keys/{name}`, the ensure of ADR-0023 point 5, unconditional, outside
  `managed_keys`, and that is a `403` under this token, while the signature
  itself is allowed.
- **On PostgreSQL the public half beside the wrapped private half is not
  authenticated.** The private half is wrapped under the master key with
  the key's name as associated data; the `public_key` column is plain
  bytes. `verify` read the column, so a writer of the table without the
  master key could make the adapter verify under a key of their own; a
  published public key read from the column would make database write into
  token forgery at every verifier that trusts the publication.
- **A P-256 public key was held in whatever SEC1 form it came in**, while
  its documentation promised the sixty-five-byte uncompressed point a JWK's
  coordinates are cut from.

Measured on Vault 1.21.4 OSS, Transit, `ecdsa-p256` and `ed25519`, and
found the same on 1.17.6 and on 1.21.4 by this port's live tests:

- `POST sign/{name}` with `key_version` on a key that does not exist: `400`
  `signing key not found`; `GET keys/{name}`: `404`.
- `key_version` above the latest: `500`; below `min_decryption_version` (the
  version retired): `500` `no such key version`; below
  `min_encryption_version`: `500`; `key_version: 0`: the latest.
- Raising `min_decryption_version` leaves `min_encryption_version` at `0`,
  which is no floor of its own; `GET keys/{name}` lists the versions from
  `min_decryption_version` to the latest.
- `data.keys.{n}.public_key`: base64 of the raw thirty-two bytes for
  Ed25519, a PEM SubjectPublicKeyInfo for P-256.
- Under a token holding the key's `sign` (`update`) and `read` only: the
  pinned signature `200`, a signature without `key_version` `200`, `POST
  keys/{name}` `403`, `verify` and `rotate` `403`.

## Decision

1. **A second way to sign, at the version named.**
   `sign_with_version t session ~tenant_id algorithm key_version message`
   signs with exactly that version; the signature names it whatever the
   latest is. OCaml has no non-zero integer type and `Key_version.t` admits
   zero, which the wire can carry: the function checks the version with
   `Key_version.from_one` before anything is asked, since Transit reads
   zero as the latest, which is what the function exists not to do; zero
   is `Malformed`, and no adapter sees it. On Vault the version Transit
   reports is checked against the one asked, `Malformed` otherwise. `sign`
   stays as it was, right where the signed bytes do not name the version
   and the verifier is the KMS, and the port's documentation states both
   criteria.
2. **It never makes a key.** A version is named because something outside
   the KMS already holds it; a key made here would be a version nobody
   holds, or one numbered as a deleted key's was. It is the one signing that
   runs under an identity that may not make keys.
3. **On Vault a refusal of it is explained by one read of the key, on
   `400`, `403` and `500`, never by Vault's text.** The key missing:
   `Signing_key_not_found`; a `403` with the key there: `Forbidden` naming
   `sign`; a version outside `[max(min_decryption_version,
   min_encryption_version, 1), latest]`: `No_key_of_version`; a `400` the
   read does not explain: `Malformed` (the rule ADR-0024 point 5 set for the
   KEK); a `500` it does not explain stays Vault's, of the moment; the read
   refused: the refusal as it came. Any other status is left as it came,
   unread: a read would add to the load a rate limit protects, or meet the
   same sealed or standby node. `verify` is read the same way, of the
   versions that verify rather than sign: the key missing
   `Signing_key_not_found` on a `403` as on a `400`, a `403` with the key
   there `Forbidden` naming `verify`, which a holder whose policy holds
   `sign` and `read` and no `verify` meets, a retired version
   `No_key_of_version`, an unexplained `400` `Malformed`; a version below
   `min_encryption_version` serves a verification, so a `400` by it is the
   request's fault and not the version's. `Vault_transit.explain_use` is
   the one reading, with what was asked of the version, `Signs` or
   `Serves`, as its parameter.
4. **The public keys are a port of their own**, `Public_key_port.S` with
   `public_keys t session ~tenant_id algorithm`, answering a
   `Public_keyring.t`: the public halves of the versions that verify, never
   empty, one algorithm, a version that verifies and no longer signs among
   them. A query, beside `Signing_port.S` rather than on it, interface
   segregation: an adapter that only publishes keys is bound by reading
   them, and a holder of public keys that cannot sign can serve it. The
   keyring names no version as the one that signs and answers no newest,
   so that pinning a signature to whatever the KMS made last is not the
   easy path; `Public_keyring.versions` and `to_list` answer oldest first,
   `get` one version. It does not verify: a key the KMS holds is verified
   in the KMS (ADR-0023 point 7 stands on that), a verifier elsewhere
   verifies with its own library, a test with `Public_key.verify`.
5. **Where an adapter holds the private halves, a public half is derived
   from its private half and the stored one is checked against it.** On
   PostgreSQL `public_keys` and `verify` unwrap each version and compare: a
   mismatch is `Malformed`, a tamper detected, never a forged key published
   or verified under. `Pg_kms.checked_signing_key` is the one place.
6. **A public key is one form per algorithm**: `Public_key.make` holds a
   P-256 key uncompressed, `0x04 || x || y`, whatever SEC1 form it was
   given, as a signature is one form per algorithm.
7. **Neither new function is cached.** A keyring held across a rotation
   would be a JWKS without the version about to sign. The port's cache,
   `Cached.Make`, serves the KEK port alone in this port and always has
   (ADR-0023's deviation: the reference's one generic value stands for
   every port, a functor over `Kms_port.S` does not), so there is nothing
   to pass through: the adapter is called directly for signing, as it was.
8. **The in-memory keys hold a version once used as used.** A ring keeps
   its versions in a map and the last version made beside it, so a version
   retired is never numbered again, and `sign_with_version`, `verify` and
   `public_keys` answer from the map.
9. **Transit's P-256 PEM is read by its fixed DER prefix, not by a parser.**
   The SubjectPublicKeyInfo of an uncompressed P-256 point as Go's x509
   marshals it, which is what Transit publishes, is twenty-six bytes fixed
   by the algorithm and the curve and then the sixty-five of the point;
   the adapter checks the prefix and takes the point. The reference links a
   DER parser for this; the OCaml one, `x509`, pulls `mirage-crypto-pk` and
   `zarith`, a C dependency, into every build of the library for a prefix
   comparison. `x509` stays a test dependency, where it decodes the same
   PEM independently of the code under test (`test_vault_signing.ml`): the
   two readings meet, which is a stronger check than one parser used twice.

## Objections considered

1. *A fixed prefix is a parser that stops working the day Transit emits
   another encoding.* Three encodings it could emit, each refused loudly as
   `Malformed` by the live test at the next Vault upgrade: a compressed
   point (another length and a different last prefix bytes), explicit curve
   parameters instead of the named curve (another prefix), another curve
   (another prefix). None of the three is a form the port promised to read,
   and a parser that accepted the first would have to normalize it, which
   `Public_key.make` does anyway for a point handed to it. Go's x509 has
   emitted this one form since its first release.
2. *`Key_version.from_one` is a check at the adapter's door, where the
   reference has it in the type; a fourth adapter may forget it.* The check
   is one line, named for its purpose, and the scripted test and the
   in-memory test hold it; a type `Key_version.from_one` as a distinct
   abstract type would split every signature's version into two types, the
   one a signature carries and the one a caller may ask, for one function.
   The reference's `NonZeroU32` is the same check in the one place its
   language puts it.
3. *A keyring without the version that signs sends a publisher to the
   signer's configuration for it; two sources of truth.* One source: the
   signer's configuration names the version, the KMS answers which versions
   verify, and the procedure of the consumer's rotation is the only place
   the two meet, which is where the reference put it too. The keyring
   answering a newest would make "sign with whatever the KMS has" one
   function call, which is the hazard decision 1 exists against.
4. *On PostgreSQL `verify` now unwraps the private half to verify a
   signature, which is more than a verification needs and holds the private
   half in memory on a path that signs nothing.* It does, and the ADR's
   consequences say so; the alternatives below, the stored half sealed
   under the master key, or both forms, cover the pair or the writer and
   not both, and the posture of the adapter, the master key in the process,
   is below this difference. The reference weighed the same three and chose
   the same.

## Consequences

- One required function more on `Signing_port.S` and one port more; the
  implementors are this library's own, and `ascetic_ddd.dek` is bound by
  `Kms_port.S` alone and needs no change.
- A pinned signature's success path costs one call fewer than `sign`'s (no
  ensure); its failure path, one read.
- On PostgreSQL the public keys and `verify` unwrap, one AES-GCM open and
  one scalar multiplication per version, and fail with `Decrypt` under a
  wrong master key, as `sign` does: integrity over availability for what a
  verifier trusts. A publisher of public keys there holds the master key,
  as a signer does, and so could sign: the separation `Public_key_port.S`
  is made for, a holder of public keys that cannot sign, is Vault's alone,
  where the publisher holds `read` on the key and nothing else.
- The version that signs lives in the consumer's configuration; it can
  name a version the KMS has not made or no longer signs with, which the
  library turns into `No_key_of_version` at the first signature. A consumer
  turns that into a refused readiness with a canary, a pinned signature at
  boot checked under the keyring, which this decision recommends and does
  not hold.
- `Public_keyring.make` refuses an empty keyring, a version zero, a version
  named twice and a key of another algorithm, so an adapter's reading of
  the KMS's answer is checked once, where it comes in.

## What can go wrong, and what holds it

- **A Vault release answers another status for a version refusal**: it
  stays Vault's and is retried. Held by the live tests, which pin every
  measured status and run at each Vault upgrade.
- **A consumer pins whatever the keyring lists last**: the hazard by hand.
  The keyring answers no newest and its documentation says why; nothing
  stronger is possible from the library.
- **A database writer replaces a stored public half**: `Malformed` on the
  read and on `verify`, held by the PostgreSQL test that overwrites the
  column.

## Alternatives rejected

**One `sign` with an optional version.** Every caller decides at each call
whether `None` means the latest; the type would not say which signature a
JWS may be made with, and `Some 0` is the latest again on Transit.

**Every signature pinned.** Breaks the bundle signer, whose key id is
outside the signed bytes and whose verifier is the KMS, the case "the
latest" is right for, for no gain.

**The floor as the pointer** (`min_encryption_version` as "the version that
signs", held in the KMS). It is a floor, not a pointer: every other client
still signs with the latest above it, its default `0` is the hazard itself,
and reading it costs what a signature costs.

**The public-key read on `Signing_port.S`.** A publisher would implement or
stub four functions it never runs; interface segregation decides, not who
holds the signature.

**A keyring that verifies, or answers its newest.** The first reopens the
local verification ADR-0023 rejected for keys the KMS holds; the second
makes the hazard of point 1 the obvious line.

**The stored public half on PostgreSQL.** Cheaper by an unwrap per version,
and a trust anchor anyone who can write the table controls.

**The stored public half sealed under the master key.** Tamper-evident as
the derivation is, and the private half stays wrapped while a verifier or
a publisher reads. It authenticates the writer and not the pair: a public
half of another key, sealed by the master key's holder, a fault of a
rotation or an import, passes, where the derivation refuses it. The
difference is the private half in memory on the paths that verify and
publish, which is below the adapter's posture: the master key is in the
process, and the private half in memory at every signing.

**Both, sealed for reading and derived at signing.** Covers both
properties at the price of a column, a migration of every row and a fourth
run of the custody tests; deferred until a publisher of public keys runs
on the PostgreSQL adapter, which none does.

**`x509` as a dependency of the Vault adapter.** A real parser for one
fixed prefix, at the cost of `zarith` in every build; decision 9.

**A JWK type in this library.** JOSE is the consumer's; the library fixes
the bytes a JWK's `x` and `y` are cut from and says where.

**Telling the refusals apart by Vault's text.** Works on this release and
breaks silently on the next (ADR-0023 point 6).
