(** The public half of a signing key, in the algorithm's form: Ed25519's thirty-two bytes,
    P-256's SEC1 uncompressed point of sixty-five.

    What a verifier holds, and what storage keeps beside the wrapped private half so that
    verifying unwraps nothing. *)

type t

val make : Signature_algorithm.t -> string -> (t, Kms_error.t) result
(** A public key from its bytes, checked to be one of the algorithm's; [Malformed] when
    they are not. *)

val algorithm : t -> Signature_algorithm.t

val to_string : t -> string
(** The bytes, in the algorithm's form. *)

val verify : t -> string -> Signature.t -> (unit, Kms_error.t) result
(** Whether the signature is this key's over the message: [Ok ()] when it is,
    [Invalid_signature] when it is not: another message, another key, another algorithm,
    bytes that are no signature. *)

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
(** The algorithm and the length. *)
