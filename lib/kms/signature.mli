(** A signature: the algorithm pinned when it was made, the version of the tenant's key of
    that algorithm that made it, and the bytes in the algorithm's form.

    The byte form is part of the contract: a consumer that stores or transmits a signature
    stores the algorithm and the version beside the bytes, and reads them back into one. *)

type t

val make : algorithm:Signature_algorithm.t -> key_version:Key_version.t -> string -> t
(** A signature as a verifier receives it. *)

val algorithm : t -> Signature_algorithm.t
val key_version : t -> Key_version.t

val to_string : t -> string
(** The bytes: Ed25519's sixty-four, ECDSA's [r || s]. *)

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
(** The algorithm, the version and the length. *)
