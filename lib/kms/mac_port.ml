(** Message authentication under a tenant's keys: one key per tenant and purpose,
    versioned by rotation. A MAC is checked by recomputation, so the port only makes them
    (ADR-0023). The use of the key only; its custody is {!Mac_key_custody.S}
    (ADR-0036). *)

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
      purpose, making the key if the tenant has none, unless the adapter's keys are
      managed elsewhere, when none is [Mac_key_not_found]. *)
end
