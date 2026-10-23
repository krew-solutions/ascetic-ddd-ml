(** Digital signatures under a tenant's keys: one key per tenant and algorithm, versioned
    by rotation; every version verifies until it is retired. The use of the key only; its
    custody is {!Signing_key_custody.S} (ADR-0036).

    Which version signs is the caller's, by the function it calls. {!S.sign} takes the
    latest and makes the key on first signing, unless the adapter's keys are managed
    elsewhere; {!S.sign_with_version} takes the version named and makes nothing. The
    latest is right only where the signed bytes do not name the version and every
    verifier learns of a version when the KMS makes it: verification through this port. A
    message that names its key's version inside what is signed, a JWS's [kid], must have
    the version before it is signed; a verifier that holds the public keys itself, a
    gateway's JWKS, a device's key directory, refuses a version until it is given it.
    Both sign with the version named (ADR-0035).

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
      making the key if the tenant has none, unless the adapter's keys are managed
      elsewhere, when none is [Signing_key_not_found]. A rotation makes the new version
      sign at once. *)

  val sign_with_version :
    t ->
    session ->
    tenant_id:string ->
    Signature_algorithm.t ->
    Key_version.t ->
    string ->
    (Signature.t, Kms_error.t) result
  (** Signs the message with that version of the tenant's key of the algorithm, and with
      no other: the signature names the version whatever the latest is. Never makes a
      key: a version is named because something outside the KMS already holds it, and a
      key made here would be a version nobody holds, or one numbered as a deleted key's
      was. It also signs under an identity that may not make keys.

      The version is from one: version 0 is [Malformed] before anything is asked, since
      Transit reads zero as the latest, which is what this function exists not to do.
      [Signing_key_not_found] when the tenant has no key of the algorithm;
      [No_key_of_version] for a version the key has not got yet, has retired, or no
      longer signs with, one that still verifies included; [Forbidden] when the KMS
      refuses to sign under this identity and the key is there. *)

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
end
