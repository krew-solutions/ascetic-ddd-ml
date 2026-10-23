(** The PostgreSQL key management service: a tenant's keys in tables, each wrapped by the
    master key. One adapter serves every port: the KEKs, the signing keys, their public
    halves and the MAC keys, and the custody of each kind.

    {2 The tables}

    [kms_keys (tenant_id, key_version, encrypted_key, master_algorithm, key_algorithm,
     created_at)], primary key [(tenant_id, key_version)]: the table the Python, Go and
    Rust ports write, so a key made by one port is read by another; a test unwraps what
    the Python port wrapped. [encrypted_key] is a {!Ascetic_kms.Wrapped_key.t} under
    master key version 1; [master_algorithm] is the master key's algorithm when the row
    was written, and is what the row is read back with.

    [kms_signing_keys (tenant_id, algorithm, key_version, encrypted_key, public_key,
     master_algorithm, created_at)]: the private half wrapped by the master key under the
    key's name, [{tenant}.sign.{algorithm}], as its associated data, so that a key wrapped
    for one tenant or algorithm does not open as another's; the public half beside it,
    which is never trusted on its own: verifying and publishing unwrap the private half,
    derive the public half from it and check the column against it, since a writer of the
    table without the master key can change the column and never the wrapped private
    half, so a mismatch is a tamper, [Malformed], refused rather than verified under or
    published (ADR-0035). [kms_mac_keys (tenant_id, purpose, key_version, encrypted_key,
     master_algorithm, created_at)]: the key wrapped under [{tenant}.mac.{purpose}]. The
    signing, verifying and MACs are the model's, in this process, in the byte forms the
    ports fix, the same the Vault adapter asks Transit for (ADR-0023).

    {2 A derivation context, and keys made elsewhere}

    Built with a [derivation_context], the service wraps and unwraps under associated data
    that binds the wrap to the context beside the tenant: what was wrapped for one channel
    of the bus does not unwrap for another, as under a derived key of Vault's. The rows
    are the same; the binding is the wrap's. Built with [managed_keys], the service makes
    no key of any kind on first use: a tenant without one is [Kek_not_found] on every
    wrap, [Signing_key_not_found] on [sign], [Mac_key_not_found] on [mac], and the key is
    made by whoever has custody, under the same lock (ADR-0024, ADR-0036).

    {2 Custody}

    The custody of each kind of key is served with the rules of ADR-0036: making is
    idempotent, a rotation never makes the first version, a retirement reads the latest
    version first, refuses a bound above it and deletes the rows below the bound, which
    is final here, the latest never going, and a deletion of nothing is nothing. A
    rotation period is refused as [Unsupported]: there is no scheduler in a table.

    {2 Locks}

    Making a tenant's first key, rotating and retiring take
    [pg_advisory_xact_lock(hashtext(table), hashtext(name))], the tenant's id for a KEK
    and the key's name for a signing or MAC key, in a scope of their own: a savepoint
    inside the caller's transaction, where the lock stays with the transaction to its end,
    and a transaction of its own when the caller has none, where the lock lasts as long as
    the making does. Either way two callers meeting a new keyring at once make one key,
    the second reading what the first committed, instead of both making version 1 and
    one failing on the primary key; two rotations at once make the next two versions, not
    one and an error. Reads take nothing. READ COMMITTED is assumed: the read after the
    lock is in a snapshot of its own, so it sees what the lock's previous holder
    committed.

    {2 What this adapter is}

    Keys in a general-purpose database, wrapped by a master key the process holds: the
    simple adapter, not key management hardware. The master key is thirty-two bytes and
    comes from a secret manager or the environment, never from source or configuration
    under version control; the session may belong to a database other than the data's.
    Where the requirements are higher, Vault Transit or a cloud KMS behind the same ports
    is the answer. *)

module Session = Ascetic_session_caqti.Caqti_session
module Identifier = Ascetic_session_caqti.Identifier

type t

include Ascetic_kms.Kms_port.S with type t := t and type session = Session.t
include Ascetic_kms.Key_custody.S with type t := t and type session := session
include Ascetic_kms.Signing_port.S with type t := t and type session := session
include Ascetic_kms.Signing_key_custody.S with type t := t and type session := session
include Ascetic_kms.Public_key_port.S with type t := t and type session := session
include Ascetic_kms.Mac_port.S with type t := t and type session := session
include Ascetic_kms.Mac_key_custody.S with type t := t and type session := session

val create :
  ?master_algorithm:Ascetic_kms.Algorithm.t ->
  ?table:Identifier.t ->
  ?signing_table:Identifier.t ->
  ?mac_table:Identifier.t ->
  ?derivation_context:string ->
  ?managed_keys:bool ->
  Ascetic_kms.Key.t ->
  t
(** A service wrapping keys with the master key, under AES-256-GCM and in the tables
    [kms_keys], [kms_signing_keys] and [kms_mac_keys] unless told otherwise. Every KEK
    made from then on is of the master algorithm too; rows already written keep the
    algorithms they name. A table's name goes into SQL as text, so it is an
    {!Identifier.t}: parsed once, safe after. A master key of the wrong length for its
    algorithm is refused, with [Malformed], by the first operation that needs it.

    [derivation_context] binds every KEK operation to a channel of the bus: one instance
    per channel, built in the composition root from the channel's name. [managed_keys]
    makes no key on first use: for a producer or a consumer of the bus, or a holder that
    signs and MACs, whose keys are made by whoever has custody. *)

val table : t -> Identifier.t
(** The table the KEKs are in. *)

val signing_table : t -> Identifier.t
(** The table of the signing keys. *)

val mac_table : t -> Identifier.t
(** The table of the MAC keys. *)

val derivation_context : t -> string option
(** The channel this instance serves, if one. *)

val setup : t -> Session.t -> (unit, Ascetic_kms.Kms_error.t) result
(** Creates the tables that are not there: the KEKs', the signing keys' and the MAC keys'.
    One transaction under an advisory lock on each table's name, taken in one order, so
    that two processes setting up at once do not race in [CREATE TABLE IF NOT EXISTS], on
    any of the three, since two services may share one of the tables and not the others.
*)
