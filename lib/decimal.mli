(** Fixed-point decimal arithmetic.

    Internally represented as an integer scaled by 10000, providing
    4 decimal places of precision without floating-point errors.
    Suitable for financial/karma calculations where exactness matters.

    {2 Gospel Contracts}
    - [zero] is the additive identity: [add x zero = x]
    - [neg (neg x) = x]
    - [abs x >= zero] for all [x]
    - [clamp_non_negative x = max zero x]
    - [is_zero x <=> to_float x = 0.0]
    - [of_raw (to_raw x) = x] (roundtrip) *)

type t [@@deriving show, eq, ord]

(** {1 Construction} *)

val zero : t
val one : t
val of_int : int -> t
val of_float : float -> t
val of_string : string -> t option
val of_string_exn : string -> t

(** {1 Arithmetic} *)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val neg : t -> t
val abs : t -> t
val min : t -> t -> t
val max : t -> t -> t
val clamp_non_negative : t -> t
val scale : t -> int -> t

(** {1 Predicates} *)

val is_zero : t -> bool
val is_positive : t -> bool
val is_negative : t -> bool
val is_non_negative : t -> bool

(** {1 Conversion} *)

val to_float : t -> float
val to_string : t -> string
val to_int_truncated : t -> int

(** {1 Internal — for persistence only} *)

val to_raw : t -> int
val of_raw : int -> t
