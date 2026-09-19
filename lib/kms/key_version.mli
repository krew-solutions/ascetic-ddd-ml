(** The version of a key: a count that fits the four bytes it travels in.

    A wrapped key, and what a versioned cipher seals, name the version of the key that
    sealed them, as four big-endian bytes in front. A version is therefore a number from 0
    to 2{^ 32}-1, and it is one by being a {!t}: checked once, where it comes in. Versions
    of a tenant's keys start at {!first}. *)

type t

val size : int
(** The length of a version on the wire, in bytes: 4. *)

val max : int
(** The greatest version, 2{^ 32}-1. *)

val first : t
(** Version 1. *)

val of_int : int -> (t, Kms_error.t) result
(** Accepts [0..max]; anything else is [Malformed]. *)

val of_int_exn : int -> t
(** {!of_int}, raising [Invalid_argument] on a number refused: for literals. *)

val to_int : t -> int

val next : t -> (t, Kms_error.t) result
(** The version after this one; [Malformed] after {!max}. *)

val stamp : t -> string -> string
(** [version || bytes]: the version as four big-endian bytes, then the bytes. *)

val read : what:string -> string -> (t * string, Kms_error.t) result
(** The version in front and the rest. Bytes too short to name a version are [Malformed],
    and the message says they cannot be [what]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
