(** Custody of a tenant's KEK: what makes it, with its options, and what retires its
    versions. The tenant-management context's, held by nobody who merely wraps and unwraps
    (ADR-0024). Beside {!Kms_port.S}, not on it: a producer or a consumer of the bus is
    bound by that port and must not hold this one. *)

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

  val retire_kek_versions_before :
    t -> session -> tenant_id:string -> Key_version.t -> (unit, Kms_error.t) result
  (** Retires every version of the tenant's KEK below the version: what they wrapped never
      unwraps again, [No_key_of_version] or [Kek_not_found] of that version from then on.
      The bound of a message's life on the bus, applied once the channel's retention has
      passed. *)
end
