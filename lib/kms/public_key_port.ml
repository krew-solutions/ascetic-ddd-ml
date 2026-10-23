(** The public halves of a tenant's signing keys, for verifiers outside the KMS: a JWKS, a
    gateway's configuration, a device's key directory. A query, beside {!Signing_port.S}
    rather than on it, so that an adapter that only publishes keys is bound by reading
    them, and a holder of public keys that cannot sign can serve it (ADR-0035). *)

module type S = sig
  type t
  type session

  val public_keys :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    (Public_keyring.t, Kms_error.t) result
  (** The public halves of the versions of the tenant's key of the algorithm that verify:
      from the oldest an operator has not retired to the latest, a version that verifies
      and no longer signs among them. Never makes a key. Where the adapter holds the
      private halves, each public half is derived from its private half, never taken from
      beside it. [Signing_key_not_found] when the tenant has no key of the algorithm;
      [Malformed] when the KMS describes the key as another algorithm's, or a stored
      public half is not its private half's. *)
end
