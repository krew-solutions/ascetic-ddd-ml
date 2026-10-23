# ADR-0036: Custody of a tenant's keys is a port per kind, beside its use; under managed keys nothing that uses a key makes one; a rotation never makes the first version, and a retirement keeps the latest and never lowers the floor

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0020, 2026-10-18, amended 2026-10-23:
what a retirement does to the material differs by adapter, and the port's
documentation says so). Held by one body of tests run on every adapter
that serves the signing and MAC custody, `test/kms/custody/custody_rules.ml`,
a functor over the ports, on the in-memory keys
(`test/kms/test_in_memory_keys.ml`), on PostgreSQL
(`test/kms/test_pg_signing.ml`) and on a Vault dev server
(`test/kms/vault/test_vault_signing.ml`): nothing signs or MACs before
custody makes the key and nothing is made by trying, making is idempotent,
a rotation makes no first version, a retirement retires below its bound,
never lowers the floor, refuses a bound above the latest and may retire a
version that still signs, a deletion of nothing is nothing. Beside it:
`test/kms/vault/test_vault_transit.ml` (the adapter's calls against a
scripted transport: managed `sign` and `mac` ask nothing but the
operation, a rotation only rotates and reads its version from the answer,
a retirement reads once and writes only a higher floor, raising a signing
floor of the key's own with it, a making reads Transit's own description
and refuses another type, a custody operation refused under the identity
is `Forbidden` naming it), `test/kms/vault/test_vault_signing.ml` (a key
of another type at the name is a conflict, a rotation period asked is the
key's, a holder's token signs and MACs and every custody operation under
it is `Forbidden`), `test/kms/vault/test_vault.ml` and `test/kms/test_pg.ml`
(the KEK's rotation of nothing makes none, its retirement never lowers the
floor nor passes the latest; on PostgreSQL a bound above the latest
deleted every row before this), `test/kms/test_cache.ml` (a retirement and
a deletion forget what was unwrapped, through `With_custody`).

Amends ADR-0023 point 1 (the ports' functions) and point 5 (signing and
MACs make the key only without managed keys), ADR-0024 points 3
(`managed_keys` covers every kind) and 4 (`Key_custody.S` takes
`rotate_kek` and `delete_kek`; a retirement reads the latest first),
ADR-0035 point 8 (the in-memory keys retire through the port).

## Context

ADR-0024 put the KEK's custody beside its use, `Key_custody.S` with
`create_kek` and `retire_kek_versions_before`, and left `rotate_kek` and
`delete_kek` on `Kms_port.S`. The signing and MAC keys of ADR-0023 had no
custody of their own:

- **Their keys were made only as a side effect.** `sign` and `mac` posted
  `keys/{name}` before every operation, unconditionally; `managed_keys`
  switched off the KEK's ensure alone.
- **A rotation made the first version.** `rotate_signing_key` made version
  1 when the key was missing, the Vault adapter's `rotate_named` created
  the key, PostgreSQL's answered `Key_version.first` for no row, the
  in-memory `push` pushed onto an empty ring, as `rotate_kek` did. An
  onboarding step retried at least once cannot make a key through it, and a
  scheduled rotation that meets an off-boarded tenant brings its key back,
  the hazard ADR-0024 removed from `encrypt`.
- **Signing versions could not be retired through any port**, and MAC
  keys could not be rotated through any port.
- **What signs saw custody in its type**: `rotate_signing_key` and
  `delete_signing_key` on `Signing_port.S`, `delete_mac_key` on
  `Mac_port.S`, `rotate_kek` and `delete_kek` on `Kms_port.S`.

And the KEK's retirement as built had two defects:

- **On Vault it wrote the floor without reading it.** Transit lowers
  `min_decryption_version` when asked, so a stale or out-of-order
  retirement un-retired versions.
- **On PostgreSQL a bound above the latest deleted every row**, a
  crypto-shred nobody asked for, where Vault refuses the same request with
  a `400`.

Measured by the reference on Vault 1.21.4 OSS, Transit, and found the same
on 1.17.6 and on 1.21.4 by this port's live tests:

- `POST keys/{name}` on a new key answers with the key's description
  (`data.type`, `data.latest_version`); on a key that exists with another
  `type`, the key unchanged and no word of the conflict.
- `POST keys/{name}/rotate` on a missing key: `400 key not found`; Transit
  itself never makes a key on rotate. On a key: the new
  `data.latest_version` in the answer.
- `sign` without `key_version` and `hmac` on a missing key: `400`; neither
  makes the key. `encrypt` on a missing key makes it under a token that
  may: the one upsert, which is why the live test of a rotation of nothing
  checks the key's absence with a decryption.
- `config` with `min_decryption_version` lowered: `200`, the retired
  versions served again. Above the latest: `400`. With a non-zero
  `min_encryption_version` below the new decryption floor: `400`; both
  raised in one call: `200`.
- `auto_rotate_period` at creation: accepted on `ed25519` and `hmac`;
  below an hour, `400`.
- Under a token holding `read` on `keys/+` and `update` on `sign/+` and
  `hmac/+`: `sign` and `hmac` at the latest `200`, a pinned signature
  `200`; `POST keys/{name}`, `rotate`, `config`, `DELETE` and `verify`
  `403`.

## Decision

1. **A custody port per kind of key, beside that kind's use port.**
   `Key_custody.S`, the KEK's, the name kept: `create_kek`, `rotate_kek`,
   `retire_kek_versions_before`, `delete_kek`; `Signing_key_custody.S`:
   `create_signing_key`, `rotate_signing_key`,
   `retire_signing_versions_before`, `delete_signing_key`;
   `Mac_key_custody.S`: `create_mac_key`, `rotate_mac_key`,
   `retire_mac_versions_before`, `delete_mac_key`. The use ports hold use
   only: `encrypt_dek`, `decrypt_dek`, `generate_dek`, `rewrap_dek`;
   `sign`, `sign_with_version`, `verify`; `mac`; and `public_keys`. A
   holder bound by a use port cannot name a custody operation; an
   implementor `include`s exactly the signatures it can serve: the
   in-memory keys hold no KEK (ADR-0024 point 6) and serve the signing and
   MAC custody alone, the two adapters serve all seven. `Cached.Make` is a
   functor over `Kms_port.S` and loses `rotate_kek` and `delete_kek` with
   it; `Cached.With_custody` passes the whole of `Key_custody.S` through.
2. **Making is idempotent and typed.** It answers the latest version; a key
   that exists is left as it is. A key at the name of another Transit type,
   or derived where a plain one is asked (or the reverse, for the KEK), is
   `Conflict`: Transit says nothing of it, so the adapter holds Transit's
   own description, the answer to `POST keys/{name}` or one read where the
   answer has no description, to what was asked. A rotation period asked
   and not the key's is set by `config`; `None` leaves the key's as it is;
   an adapter without a scheduler refuses one as `Unsupported`.
   `Key_options.t`, `auto_rotate_period` in seconds, for the signing and
   MAC keys, as `Kek_options.t` for the KEK.
3. **A rotation never makes the first version**, as Transit's never does:
   a key missing is `Kek_not_found`, `Signing_key_not_found`,
   `Mac_key_not_found`. It is not idempotent, a retried rotation makes one
   version more, which serves as well for every kind: a KEK's data is
   rewrapped to the latest, a signing key's verifiers are given the
   keyring and a pinned signer moves only when told, a MAC key makes one
   more refusal of each open etag.
4. **A retirement's bound is the oldest version kept.** The latest is never
   retired: a bound above it is `No_key_of_version` and nothing is
   retired. A bound at or below the floor there is does nothing, the floor
   is never lowered, so a retried or a stale retirement is harmless; on
   Vault the adapter reads the key once and writes only a higher
   `min_decryption_version`, and on PostgreSQL the adapter reads the latest
   under the keyring's lock and the deletion below the bound is by its
   nature monotonic. A version that still signs may be retired: that is
   how a compromise is answered; on Vault a signing floor of the key's own
   below the bound is raised with it in the same call, which Transit
   requires and which changes nothing observable, a retired version
   neither signing nor verifying. A MAC key's retirement changes nothing a
   `mac` makes, it is made with the latest, and on PostgreSQL bounds the
   key material kept at rest.
5. **A deletion of nothing is not an error.**
6. **`managed_keys` covers every kind.** Built so, no adapter makes a key
   on first use: a tenant without one is `Kek_not_found` on a wrap,
   `Signing_key_not_found` on `sign`, `Mac_key_not_found` on `mac`, on
   Vault named by one read of the key on a `400`, `403` or `500`, as
   ADR-0035 reads a pinned signature's refusal. The default is unchanged: a
   consumer and its tests that rely on keys made on first use keep working.
   The executor of "a holder makes no key" is the holder's KMS identity, on
   Vault `read` alone on `keys/+`; the switch is what makes the right
   configuration work, and a configuration that forgets it fails at its
   first `sign` with a `403`, loudly.
7. **A custody operation refused under the identity is `Forbidden`**,
   naming the operation, `create`, `rotate`, `retire` or `delete`, read by
   one read of the key where the key's absence would explain it otherwise;
   a making refused is `Forbidden` with no read, the policy's whether the
   key is there or not.
8. **The in-memory keys** serve the signing and MAC custody ports with
   these rules and are built `~managed_keys:true` as the adapters are; a
   ring is a map of the versions that serve and the last version made.
   `Cached.With_custody` passes every KEK custody operation through and
   forgets what was unwrapped under a tenant's KEK on its retirement and
   deletion.
9. **The custody rules are one test body, a functor over the ports.**
   `Custody_rules.Make (Keys)` in `test/kms/custody`, a library the three
   test executables share, takes a record with one polymorphic field, the
   scope a call runs in, and runs the same assertions on the in-memory
   keys, PostgreSQL and Vault, so the in-memory keys a consumer's tests run
   on are held to the adapters' behaviour by the same text.

## Objections considered

1. *A record with a polymorphic field for the scope is a device; the
   reference's `Scopes` trait reads more plainly.* An OCaml functor
   argument with a polymorphic `val atomic : (session -> t -> ('a, e)
   result) -> ('a, e) result` would do as well; the record is one value a
   test file builds in one line from its fixture, and is what a
   first-class polymorphic function is spelled as here. The device is the
   language's, not the test's.
2. *The PostgreSQL retirement takes a lock and a read where it took one
   statement; the KEK's retirement is on the bus's path.* It is on the
   custodian's path, once per retention period, never on a producer's or a
   consumer's; two statements more there buy the refusal of a bound above
   the latest, which deleted every row before this.
3. *`managed_keys` on `sign` and `mac` changes what a consumer's tests of
   the in-memory keys see if they built them so.* No consumer built them
   so: the argument is new here, and its default keeps the keys made on
   first use. A consumer that opts in gets the adapters' behaviour, which
   is the point of the in-memory keys.
4. *The reference measured on 1.21.4 and this port's compose file ran
   1.17.6; the answers may differ.* They were found the same by the live
   tests on both, which pin them; the compose file names the `1.21` line
   since this decision, so the live tests run where the reference
   measured. Where a later Vault answers otherwise, the tests say so at the
   upgrade, and the reading by one read of the key is what makes the
   adapter indifferent to the status within `400`, `403` and `500`.

## Consequences

- Breaking for every caller of the moved functions: `rotate_kek` and
  `delete_kek` are on `Key_custody.S`, which `Pg_kms` and `Vault_transit`
  still `include`, so a caller holding an adapter's module compiles as it
  was; a caller holding `Cached.Make`'s value loses them and takes
  `With_custody`. In this repository: the library's tests, and
  `ascetic_ddd.dek`'s tests, whose source is bound by `Kms_port.S` alone
  and calls none of them.
- Calls on the success path: a managed `sign` or `mac` one, where two were
  (no ensure); a rotation one, where three were; a retirement two (a read
  first), where one was; a making one, with a read only where Vault
  answers without the key's description.
- A tenant without keys reaches a managed holder's use path as
  `*_not_found`: a consumer maps it to a refusal that names the gap, not to
  an unavailable dependency.
- The custodian holds `create`, `update` and `read` on `keys/+`, `update` on
  `keys/+/rotate` and `keys/+/config`, `delete` on `keys/+`; a holder `read`
  on `keys/+` and `update` on the operation's path alone.
- A retirement is final on PostgreSQL and final within the port on Vault.
  The port promises that a retired version never serves again, and both
  keep it; but PostgreSQL keeps it by deleting the rows, the material with
  them, as a trim would, where Vault raises a floor the adapter never
  lowers and keeps the material until the key is trimmed, which the port
  does not do, so an operator with Vault's own API may lower the floor and
  bring a version back, and a mistaken bound is mended there and not on
  PostgreSQL. The guard against a mistaken bound is the custodian's, where
  this ADR put the serialisation of custody: a retirement is reviewed as a
  deletion is.

## What can go wrong, and what holds it

- **A composition root forgets `managed_keys`**: under a holder's policy
  its first `sign` is a `403`. Held by the holder test on Vault.
- **Two custodians retire one key at once**, each reading the floor before
  the other writes: Transit has no compare-and-set on `config`, so the
  lower bound may land last. The read narrows the hazard from every stale
  call to two writers at once; serialising custody per key is the
  custodian's. On PostgreSQL the keyring's advisory lock serialises them.
- **A pinned signer's version is retired** before it moves: its next
  signature is `No_key_of_version`, loudly; the order of the consumer's
  procedure and ADR-0035's boot canary hold it.
- **A Vault release answers otherwise**: the live tests pin every measured
  answer and run at each upgrade.

## Owed, named

- The import of a tenant's own key material (BYOK), on the custody ports,
  with the first tenant that brings one.
- Whether `managed_keys` becomes the default, a breaking step of its own,
  once consumers make their keys through custody.
- The two adapters name a KEK version retired otherwise: `No_key_of_version`
  on Vault, `Kek_not_found { key_version = Some _ }` on PostgreSQL.
- A floor on PostgreSQL, reads refusing below it, the material kept,
  `delete_*` the one deletion, would give Vault's reversibility for every
  kind of key and name a retired version `No_key_of_version` as Vault
  does, which closes the item above as well. A step of its own, taken with
  the first custodian's procedure that needs a retirement undone, or with
  a `trim` on the custody ports, which bounds the material on Vault.

## Alternatives rejected

**One custody port with the kind of key as a value** (a variant `Kek |
Signing of algorithm | Mac of purpose`). A custodian works from data, a
manifest of keys, and a new purpose would need no code. But the in-memory
keys would answer `Unsupported` for the KEK at run time where the
signatures say it, the options differ by kind (`derived` is the KEK's
alone), and the custodian's loop over a manifest is a three-arm match
either way.

**The custody functions left on the use ports, the KMS policy the only
guard.** The smallest change, and the policy does guard. But a holder that
cannot perform them carries them in its bound, and the in-memory keys
would keep giving a consumer's tests operations its production identity is
refused, the reason ADR-0024 put the KEK's custody beside its use.

**A conditional rotation, `rotate ~after:version`.** Cannot be atomic on
Transit, whose rotate has no compare-and-set; needs a read of the latest
the KEK and MAC ports do not otherwise have; and with a stale `after` it
would skip a compromise rotation silently.

**Refusing to retire a version that still signs.** Adds a step to the
emergency it exists for.

**The retirement written without a read.** Brings retired versions back on
any stale call.

**Managed keys as the default now.** Changes run-time behaviour under every
consumer with no compile error.

**Renaming `Key_custody` to `Kek_custody`.** Symmetry bought with an edit
at every consumer's `include` and in the ADRs; the name already pairs it
with `Kms_port`.
