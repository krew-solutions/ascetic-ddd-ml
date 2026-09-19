(** The PostgreSQL DEK store: a resource's DEKs in a table, each wrapped by the tenant's
    KEK through the KMS port.

    {2 The table}

    [deks (tenant_id, kind, resource_id, version, encrypted_dek, algorithm, created_at)],
    primary key [(tenant_id, kind, resource_id, version)]: the [stream_deks] of the Python
    and Go sources, with the columns named after {!Ascetic_dek.Resource}. [resource_id] is
    JSONB, the id's canonical text; [encrypted_dek] is what the KMS wrapped; [algorithm]
    is the DEK's own, what its cipher is built with.

    {2 Locks}

    Making a resource's first DEK takes
    [pg_advisory_xact_lock(hashtext(table), hashtext(resource))] in a scope of its own, a
    savepoint inside the caller's transaction, where the lock stays with the transaction
    to its end, and a transaction of its own when the caller has none. Either way two
    callers meeting a new resource at once make one key, the second reading what the first
    committed, instead of both making version 1 and one failing on the primary key. Reads
    take nothing. READ COMMITTED is assumed, as in the KMS.

    {2 What is not here}

    Nothing is cached: every call reads the resource's rows and unwraps them through the
    KMS; {!Ascetic_kms.Cached} in front of the KMS keeps what it unwraps. A second DEK
    version for a resource is never made here: versions are for an algorithm migration,
    and the row that starts one is the migration's to write. *)

module Session = Ascetic_session_caqti.Caqti_session
module Identifier = Ascetic_session_caqti.Identifier

module Make (Kms : Ascetic_kms.Kms_port.S with type session = Session.t) : sig
  type t

  include Ascetic_dek.Dek_store.S with type t := t and type session = Session.t

  val create : ?algorithm:Ascetic_kms.Algorithm.t -> ?table:Identifier.t -> Kms.t -> t
  (** A store wrapping DEKs through the KMS, making them for AES-256-GCM and in table
      [deks] unless told otherwise. Rows already written keep the algorithm they name. The
      table's name goes into SQL as text, so it is an {!Identifier.t}. *)

  val table : t -> Identifier.t
  (** The table the DEKs are in. *)

  val kms : t -> Kms.t
  (** The KMS the DEKs are wrapped through. *)

  val setup : t -> Session.t -> (unit, Ascetic_dek.Dek_error.t) result
  (** Creates the table if it is not there, in one transaction under an advisory lock on
      its name. *)
end
