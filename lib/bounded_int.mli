(** Bounded integer functor.

    Creates integer types constrained to a specific range,
    enforced by smart constructors. Useful for value objects
    like FraudScore (0-100), Percentile (0-100), etc.

    {2 Gospel Contracts}
    - [of_int n = Some t] iff [min_value <= n <= max_value]
    - [to_int t] always satisfies [min_value <= to_int t <= max_value]
    - [of_int_clamped n] always produces a valid value
    - [of_int_exn n] requires [min_value <= n <= max_value] *)

module type BOUNDS = sig
  val min_value : int
  val max_value : int
  val name : string
end

module type S = sig
  type t [@@deriving show, eq, ord]

  val min_value : int
  val max_value : int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val of_int_clamped : int -> t
  val to_int : t -> int
  val zero : t option
end

module Make (B : BOUNDS) : S
