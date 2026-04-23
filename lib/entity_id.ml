(** Typed entity ID wrapper functor. *)

module type NAME = sig
  val name : string
end

module type S = sig
  type t [@@deriving show, eq, ord]

  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
end

module Make (N : NAME) : S = struct
  type t = int64 [@@deriving eq, ord]

  let pp fmt t = Format.fprintf fmt "%s(%Ld)" N.name t
  let show t = Format.asprintf "%a" pp t

  let of_int64 n = n
  let to_int64 t = t
  let of_int n = Int64.of_int n
  let to_int t = Int64.to_int t
end
