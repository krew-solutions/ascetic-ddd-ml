(** Custody of a tenant's signing keys: what makes, rotates, retires and deletes them. The
    tenant-management context's, or an operator's procedure for a key of the platform's
    own, and never what signs (ADR-0036). Beside {!Signing_port.S}, not on it, as
    {!Key_custody.S} is beside {!Kms_port.S}; the rules are {!Key_custody.S}'s. *)

module type S = sig
  type t
  type session

  val create_signing_key :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    Key_options.t ->
    (Key_version.t, Kms_error.t) result
  (** Makes the tenant's key of the algorithm with the options, version 1, and answers its
      latest version; a key that exists is left as it is and its latest version answered,
      its rotation period set when the options ask for another. [Conflict] when the name
      holds a key of another type, or a derived one; [Unsupported] for a rotation period
      an adapter without a scheduler cannot keep. *)

  val rotate_signing_key :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    (Key_version.t, Kms_error.t) result
  (** Makes the next version of the tenant's key of the algorithm; answers it. Earlier
      versions keep verifying until they are retired. [Signing_key_not_found] when the
      tenant has none: a rotation never makes the first version. *)

  val retire_signing_versions_before :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    Key_version.t ->
    (unit, Kms_error.t) result
  (** Retires every version of the tenant's key of the algorithm below the version: they
      neither sign, verify nor are published again, [No_key_of_version] from then on. A
      version that still signs may be retired: that is how a compromised one is answered;
      the latest never is. A bound at or below the oldest version kept does nothing.
      [Signing_key_not_found] when the tenant has none; [No_key_of_version] for a bound
      above the latest version, and nothing is retired. *)

  val delete_signing_key :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    (unit, Kms_error.t) result
  (** Deletes every version of the tenant's key of the algorithm: what it signed never
      verifies again. Nothing to delete is not an error. *)
end
