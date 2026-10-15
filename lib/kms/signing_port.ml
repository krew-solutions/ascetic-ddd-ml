(** Digital signatures under a tenant's keys: one key per tenant and algorithm, made on
    first signing, versioned by rotation. The latest version signs; every version
    verifies, until an operator retires it.

    The algorithm is an input of signing and a property of the signature, so a tenant
    moves to another algorithm by signing with it, a new key rather than a rotation, while
    what the old key signed keeps verifying. Deleting a key ends the verifiability of
    everything it ever signed: the tenant's off-boarding, never a rotation. A port beside
    {!Kms_port.S}, not on it: a consumer bound by the KEK port alone must not grow a
    bound it does not use (ADR-0023). *)

module type S = sig
  type t
  type session

  val sign :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    string ->
    (Signature.t, Kms_error.t) result
  (** Signs the message with the latest version of the tenant's key of the algorithm,
      making the key if the tenant has none. *)

  val verify :
    t ->
    session ->
    tenant_id:string ->
    string ->
    Signature.t ->
    (unit, Kms_error.t) result
  (** Verifies the signature over the message under the key version it names: [Ok ()] when
      it verifies, [Invalid_signature] when it does not. Never makes a key: a signature by
      a key the tenant has not got is [Signing_key_not_found], by a version the key has
      not got, or no longer serves, [No_key_of_version]. *)

  val rotate_signing_key :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    (Key_version.t, Kms_error.t) result
  (** Makes the next version of the tenant's key of the algorithm, or its first; answers
      the version. Earlier versions keep verifying. *)

  val delete_signing_key :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    (unit, Kms_error.t) result
  (** Deletes every version of the tenant's key of the algorithm; nothing to delete is not
      an error. *)
end
