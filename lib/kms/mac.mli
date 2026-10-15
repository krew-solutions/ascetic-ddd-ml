(** A MAC: HMAC-SHA-256, thirty-two bytes, and the version of the key that made it.

    Checked by recomputation, so it carries no algorithm: whoever holds the same key makes
    the tag again and compares, in time that depends on the length alone. *)

type t

val make : key_version:Key_version.t -> string -> t
(** A MAC as the key's holder made it. *)

val key_version : t -> Key_version.t

val to_string : t -> string
(** The bytes. *)

val equal : t -> t -> bool
(** The same version and the same bytes, compared in constant time. *)

val pp : Format.formatter -> t -> unit
(** The version and the length. *)
