(** Message authentication under a tenant's keys: one key per tenant and purpose, made on
    first use, versioned by rotation outside this port. A MAC is checked by recomputation,
    so the port only makes them (ADR-0023). *)

module type S = sig
  type t
  type session

  val mac :
    t ->
    session ->
    tenant_id:string ->
    Key_label.t ->
    string ->
    (Mac.t, Kms_error.t) result
  (** HMAC-SHA-256 of the message under the latest version of the tenant's key for the
      purpose, making the key if the tenant has none. *)

  val delete_mac_key :
    t -> session -> tenant_id:string -> Key_label.t -> (unit, Kms_error.t) result
  (** Deletes every version of the tenant's key for the purpose; nothing to delete is not
      an error. *)
end
