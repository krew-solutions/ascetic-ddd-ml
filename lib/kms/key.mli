(** A symmetric key in the clear, or the private half of an asymmetric one: bytes that are
    never printed.

    The type is abstract so that a key does not end up in a log or an error by accident:
    {!pp} shows its length and nothing else. The runtime gives no way to wipe a value when
    it is dropped, a string is immutable and the collector may have copied it, so a key
    lives in memory until the collector reuses the space; what can be done is not to
    spread it, and the bytes leave this type only through {!to_string}. *)

type t

val of_string : string -> t
(** A key of the given bytes. *)

val to_string : t -> string
(** The bytes. *)

val random : int -> (t, Kms_error.t) result
(** So many bytes from the operating system, [getrandom(2)] on Linux; [Entropy] when it
    gives none. For the keys the libraries draw from a seed: a signing key, a MAC key. *)

val length : t -> int
(** How many bytes. *)

val equal : t -> t -> bool
(** Whether two keys are the same bytes, in time that depends on their length alone. *)

val pp : Format.formatter -> t -> unit
(** [Key(32 bytes)]: the length, never the bytes. *)
