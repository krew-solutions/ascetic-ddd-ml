(** The PostgreSQL key management service: a tenant's KEKs in a table, each wrapped by the
    master key.

    {2 The table}

    [kms_keys (tenant_id, key_version, encrypted_key, master_algorithm, key_algorithm,
     created_at)], primary key [(tenant_id, key_version)]: the table the Python, Go and
    Rust ports write, so a key made by one port is read by another; a test unwraps what
    the Python port wrapped. [encrypted_key] is a {!Ascetic_kms.Wrapped_key.t} under
    master key version 1; [master_algorithm] is the master key's algorithm when the row
    was written, and is what the row is read back with.

    {2 Locks}

    Making a tenant's first key, and rotating, take
    [pg_advisory_xact_lock(hashtext(table), hashtext(tenant_id))] in a scope of their own:
    a savepoint inside the caller's transaction, where the lock stays with the transaction
    to its end, and a transaction of its own when the caller has none, where the lock
    lasts as long as the making does. Either way two callers meeting a new tenant at once
    make one key, the second reading what the first committed, instead of both making
    version 1 and one failing on the primary key; two rotations at once make versions two
    and three, not one and an error. Reads take nothing. READ COMMITTED is assumed: the
    read after the lock is in a snapshot of its own, so it sees what the lock's previous
    holder committed.

    {2 What this adapter is}

    Keys in a general-purpose database, wrapped by a master key the process holds: the
    simple adapter, not key management hardware. The master key is thirty-two bytes and
    comes from a secret manager or the environment, never from source or configuration
    under version control; the session may belong to a database other than the data's.
    Where the requirements are higher, Vault Transit or a cloud KMS behind the same port
    is the answer. *)

module Session = Ascetic_session_caqti.Caqti_session
module Identifier = Ascetic_session_caqti.Identifier

type t

include Ascetic_kms.Kms_port.S with type t := t and type session = Session.t

val create :
  ?master_algorithm:Ascetic_kms.Algorithm.t ->
  ?table:Identifier.t ->
  Ascetic_kms.Key.t ->
  t
(** A service wrapping KEKs with the master key, under AES-256-GCM and in table [kms_keys]
    unless told otherwise. Every KEK made from then on is of the master algorithm too;
    rows already written keep the algorithms they name. The table's name goes into SQL as
    text, so it is an {!Identifier.t}: parsed once, safe after. A master key of the wrong
    length for its algorithm is refused, with [Malformed], by the first operation that
    needs it. *)

val table : t -> Identifier.t
(** The table the KEKs are in. *)

val setup : t -> Session.t -> (unit, Ascetic_kms.Kms_error.t) result
(** Creates the table if it is not there. One transaction under an advisory lock on the
    table's name, so that two processes setting up at once do not race in
    [CREATE TABLE IF NOT EXISTS]. *)
