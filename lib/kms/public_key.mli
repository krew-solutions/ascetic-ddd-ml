(** The public half of a signing key, in the algorithm's form: Ed25519's thirty-two bytes,
    P-256's SEC1 uncompressed point of sixty-five.

    What a verifier outside the KMS holds, in a {!Public_keyring}; what storage keeps
    beside the wrapped private half, and checks against the one derived from it
    (ADR-0035). *)

type t

val make : Signature_algorithm.t -> string -> (t, Kms_error.t) result
(** A public key from its bytes, checked to be one of the algorithm's, and held in the
    algorithm's one form: Ed25519's thirty-two bytes; P-256's uncompressed SEC1 point,
    [0x04 || x || y], sixty-five bytes, whichever SEC1 form it was given in, so that a
    key's bytes are one form per algorithm, as a signature's are. [Malformed] when the
    bytes are not a key of the algorithm. *)

val algorithm : t -> Signature_algorithm.t

val to_string : t -> string
(** The bytes, in the algorithm's form: Ed25519's thirty-two; P-256's sixty-five, [0x04]
    then the coordinates [x] and [y], big-endian, thirty-two bytes each, what a JWK's [x]
    and [y] are cut from. *)

val verify : t -> string -> Signature.t -> (unit, Kms_error.t) result
(** Whether the signature is this key's over the message: [Ok ()] when it is,
    [Invalid_signature] when it is not: another message, another key, another algorithm,
    bytes that are no signature. *)

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
(** The algorithm and the length. *)
