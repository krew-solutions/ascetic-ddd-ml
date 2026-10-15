(** The signing key: one version of a tenant's key of an algorithm, the private half that
    signs and the public half that verifies.

    The private half is thirty-two bytes either way, an Ed25519 seed or a P-256 scalar;
    the public half is derived from it, never trusted to storage. Made fresh, from a
    seed, or loaded from the private half storage kept. The key knows its name,
    [{tenant}.sign.{algorithm}], which is Transit's key name on one adapter and the
    associated data of the wrapping on the other. *)

type t

val generate :
  tenant_id:string ->
  algorithm:Signature_algorithm.t ->
  version:Key_version.t ->
  (t, Kms_error.t) result
(** A fresh key of the algorithm for the tenant, as that version, from the operating
    system's randomness; [Entropy] when it gives none. A draw that is not a key of the
    algorithm, a P-256 scalar outside the field, with negligible probability, is drawn
    again; an Ed25519 seed always is one. *)

val of_seed :
  tenant_id:string ->
  algorithm:Signature_algorithm.t ->
  version:Key_version.t ->
  string ->
  (t, Kms_error.t) result
(** The key a seed makes, the same seed the same key, so that a test's signatures are the
    same from run to run. Any thirty-two bytes are an Ed25519 key; a P-256 seed must be a
    scalar of the field, else [Malformed]. *)

val load :
  tenant_id:string ->
  algorithm:Signature_algorithm.t ->
  version:Key_version.t ->
  Key.t ->
  (t, Kms_error.t) result
(** The key from its private half, as storage kept it; [Malformed] when the bytes are not
    a private key of the algorithm. *)

val tenant_id : t -> string
val algorithm : t -> Signature_algorithm.t

val version : t -> Key_version.t
(** From 1. *)

val private_key : t -> Key.t
(** The private half: what storage wraps and keeps. *)

val public : t -> Public_key.t

val name_of : tenant_id:string -> Signature_algorithm.t -> string
(** The name of a tenant's key of an algorithm, the same on every adapter:
    [{tenant}.sign.{algorithm}]. *)

val name : t -> string
(** This key's name. *)

val sign : t -> string -> Signature.t
(** Signs the message: a signature of this algorithm and version, in the algorithm's form,
    Ed25519's sixty-four bytes, ECDSA's [r || s] with SHA-256 of the message, never DER.
    ECDSA's nonce is RFC 6979's, so the same key signs the same message the same way. *)

val pp : Format.formatter -> t -> unit
(** The tenant, the algorithm and the version; never the key. *)
