(** The algorithms a key may be for. The name is what storage records. *)

type t =
  | Aes_256_gcm
      (** AES with a 256-bit key in Galois/Counter Mode: a 96-bit nonce and a 128-bit tag.
      *)

val to_string : t -> string
(** The name, as storage records it: [AES-256-GCM]. *)

val of_string : string -> (t, Kms_error.t) result
(** The algorithm of a stored name; [Unsupported_algorithm] for a name this library does
    not implement. *)

val generate_key : t -> (Key.t, Kms_error.t) result
(** A fresh key for a cipher of this algorithm, for when there is no cipher yet to ask: a
    master key for a configuration, a key for a test. *)

val cipher : t -> Key.t -> aad:string -> (Cipher.t, Kms_error.t) result
(** The cipher of this algorithm over the key, binding what it seals to [aad]. One arm per
    algorithm: the adapter is chosen here and nowhere else. Refuses a key of the wrong
    length. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
