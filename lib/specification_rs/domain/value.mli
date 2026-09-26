(** The scalar values every part of the library speaks: what a JSONPath template's
    literals and parameters are, what the typed DSL builds with, and what goes to
    PostgreSQL as a query parameter.

    The operators follow PostgreSQL, because a specification has two readers that must
    agree, the evaluator here and the database reading the compiled SQL: integer division
    truncates, a division by zero and an integer overflow are errors rather than wrapped
    results, an integer meeting a float is promoted, a shift counts modulo 64, [NaN]
    equals itself and is greater than any other number. Text is the exception the library
    cannot remove: here it is ordered by code point, in the database by the column's
    collation.

    An integer is a 64-bit one, PostgreSQL's [bigint], and not OCaml's 63-bit [int]: the
    two readers must overflow at the same value. *)

(** A point in time: microseconds since the Unix epoch, PostgreSQL's resolution. A type of
    the library's own, so that the core depends on no calendar library; the application
    converts at its edge. *)
module Timestamp : sig
  type t [@@deriving show, eq, ord]

  val of_micros : int64 -> t
  (** The point [micros] microseconds after the Unix epoch. *)

  val to_micros : t -> int64
  (** Microseconds since the Unix epoch. *)
end

(** A span of time, in microseconds: what two {!Timestamp.t}s differ by. *)
module Interval : sig
  type t [@@deriving show, eq, ord]

  val of_micros : int64 -> t
  (** The span of [micros] microseconds. *)

  val to_micros : t -> int64
  (** The span in microseconds. *)
end

(** A scalar value.

    {!equal} is identity of representation, which is what comparing trees in a test wants:
    [Int 1L] is not [Float 1.0]. Equality of the values meant is {!equals}. *)
type t =
  | Null  (** The null: a value that is not known. *)
  | Bool of bool  (** A truth value. *)
  | Int of int64  (** An integer: PostgreSQL's [bigint]. *)
  | Float of float  (** A float: PostgreSQL's [double precision]. *)
  | Text of string  (** A text. *)
  | Timestamp of Timestamp.t  (** A point in time. *)
  | Interval of Interval.t  (** A span of time. *)
[@@deriving show, eq]

include Operand.S with type t := t

val of_int : int -> t
(** [Int] of the integer. *)

val of_bool : bool -> t
val of_float : float -> t
val of_string : string -> t

val of_option : ('a -> t) -> 'a option -> t
(** [None] is the null. *)
