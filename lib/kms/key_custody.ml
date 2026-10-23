(** Custody of a tenant's KEK: what makes it, with its options, rotates it, retires its
    versions and deletes it. The tenant-management context's, held by nobody who merely
    wraps and unwraps (ADR-0024, ADR-0036). Beside {!Kms_port.S}, not on it: a producer
    or a consumer of the bus is bound by that port and must not hold this one.

    The rules of every custody port (ADR-0036): making is idempotent and typed; a rotation
    never makes the first version and is not idempotent, a retried one makes one version
    more, which serves as well; a retirement keeps the latest version and never lowers
    the floor, so a retried or a stale one does nothing; a deletion of nothing is not an
    error. What a retirement does to the material of the versions retired is the
    adapter's: on PostgreSQL their rows are deleted, and nothing brings them back; on
    Vault the material stays until the key is trimmed, which no port does, and an
    operator may lower the floor outside the port. The port promises that a retired
    version never serves again, and a custodian reviews a retirement as it reviews a
    deletion. *)

module type S = sig
  type t
  type session

  val create_kek :
    t ->
    session ->
    tenant_id:string ->
    Kek_options.t ->
    (Key_version.t, Kms_error.t) result
  (** Makes the tenant's KEK with the options, version 1, and answers its latest version;
      a key that exists with the same options is left as it is and its latest version
      answered, one that exists with other options is [Conflict]. *)

  val rotate_kek : t -> session -> tenant_id:string -> (Key_version.t, Kms_error.t) result
  (** The next version of the tenant's KEK; answers it. Earlier versions stay, so what
      they wrapped still unwraps. [Kek_not_found] when the tenant has none: a rotation
      never makes the first version, so a scheduled one that meets an off-boarded tenant
      does not bring its key back. *)

  val retire_kek_versions_before :
    t -> session -> tenant_id:string -> Key_version.t -> (unit, Kms_error.t) result
  (** Retires every version of the tenant's KEK below the version: what they wrapped never
      unwraps again, [No_key_of_version] on Vault and [Kek_not_found] of that version on
      PostgreSQL from then on. The bound of a message's life on the bus, applied once the
      channel's retention has passed (ADR-0024). A bound at or below the oldest version
      kept does nothing: the floor is never lowered. [Kek_not_found] when the tenant has
      none; [No_key_of_version] for a bound above the latest version, which would retire
      the key whole, and nothing is retired. *)

  val delete_kek : t -> session -> tenant_id:string -> (unit, Kms_error.t) result
  (** Deletes every version of the tenant's KEK: nothing wrapped under them unwraps again,
      crypto-shredding. Nothing to delete is not an error. *)
end
