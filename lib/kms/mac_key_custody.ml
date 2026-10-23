(** Custody of a tenant's MAC keys: what makes, rotates, retires and deletes them, never
    what makes MACs (ADR-0036). The rules are {!Key_custody.S}'s. Since a MAC is made with
    the latest version alone, a retirement changes nothing a holder of {!Mac_port.S}
    sees: it bounds the versions that serve, and on PostgreSQL the key material kept at
    rest. *)

module type S = sig
  type t
  type session

  val create_mac_key :
    t ->
    session ->
    tenant_id:string ->
    Key_label.t ->
    Key_options.t ->
    (Key_version.t, Kms_error.t) result
  (** Makes the tenant's key for the purpose with the options, version 1, and answers its
      latest version; a key that exists is left as it is and its latest version answered,
      its rotation period set when the options ask for another. [Conflict] when the name
      holds a key of another type, or a derived one; [Unsupported] for a rotation period
      an adapter without a scheduler cannot keep. *)

  val rotate_mac_key :
    t -> session -> tenant_id:string -> Key_label.t -> (Key_version.t, Kms_error.t) result
  (** Makes the next version of the tenant's key for the purpose; answers it. What the
      earlier version made no longer matches what the key makes: a consumer that checks
      by recomputation sees it as stale. [Mac_key_not_found] when the tenant has none. *)

  val retire_mac_versions_before :
    t ->
    session ->
    tenant_id:string ->
    Key_label.t ->
    Key_version.t ->
    (unit, Kms_error.t) result
  (** Retires every version of the tenant's key for the purpose below the version; the
      latest never is. A bound at or below the oldest version kept does nothing.
      [Mac_key_not_found] when the tenant has none; [No_key_of_version] for a bound above
      the latest version, and nothing is retired. *)

  val delete_mac_key :
    t -> session -> tenant_id:string -> Key_label.t -> (unit, Kms_error.t) result
  (** Deletes every version of the tenant's key for the purpose; nothing to delete is not
      an error. *)
end
