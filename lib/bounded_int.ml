(** Bounded integer functor. *)

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

module Make (B : BOUNDS) : S = struct
  type t = int [@@deriving eq, ord]

  let min_value = B.min_value
  let max_value = B.max_value

  let pp fmt t = Format.fprintf fmt "%s(%d)" B.name t
  let show t = Format.asprintf "%a" pp t

  let of_int n =
    if n >= B.min_value && n <= B.max_value then Some n else None

  let of_int_exn n =
    match of_int n with
    | Some v -> v
    | None ->
        invalid_arg
          (Printf.sprintf "%s.of_int_exn: %d not in [%d, %d]" B.name n
             B.min_value B.max_value)

  let of_int_clamped n =
    Int.max B.min_value (Int.min B.max_value n)

  let to_int t = t

  let zero = of_int 0
end
