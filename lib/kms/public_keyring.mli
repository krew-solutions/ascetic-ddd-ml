(** The public halves of the versions of a tenant's key of one algorithm that verify, by
    version: what a JWKS publishes and a device's key directory holds. Never empty, of one
    algorithm, its versions from one.

    It names no version as the one that signs, the signer's configuration does
    (ADR-0035), and answers no newest, so that pinning a signature to whatever the KMS
    made last is not the easy path. It does not verify: a key the KMS holds is verified in
    the KMS (ADR-0023); a verifier elsewhere verifies with its own library, and a test
    with {!Public_key.verify}. *)

type t

val make :
  Signature_algorithm.t -> (Key_version.t * Public_key.t) list -> (t, Kms_error.t) result
(** The keyring of the algorithm from its versions and their public halves, in any order.
    [Malformed] when there is no version, a version is zero or named twice, or a key is of
    another algorithm. *)

val algorithm : t -> Signature_algorithm.t

val get : t -> Key_version.t -> Public_key.t option
(** The public half of the version, if that version verifies. *)

val to_list : t -> (Key_version.t * Public_key.t) list
(** The versions and their public halves, oldest first. *)

val versions : t -> Key_version.t list
(** The versions alone, oldest first. *)

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
(** The algorithm and the versions. *)
