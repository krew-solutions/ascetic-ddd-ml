(* What stands where a value will, in a template that is not yet bound. *)

(* How a placeholder names its parameter. *)
module Param_key = struct
  type t =
    | Position of int
        (** By its place among the template's placeholders, from zero: [%d]. *)
    | Name of string  (** By name: [%(age)d]. *)
  [@@deriving show { with_path = false }, eq, ord]

  let to_string = function
    | Position position -> Printf.sprintf "#%d" position
    | Name name -> Printf.sprintf "'%s'" name
end

(* What a placeholder's letter asks of its parameter. A null fits any. *)
module Param_kind = struct
  type t =
    | Any  (** [%s]: a value of any kind, as Python's [%s] takes any object. *)
    | Integer  (** [%d]: an integer. *)
    | Number  (** [%f]: a number, integer or float. *)
  [@@deriving show { with_path = false }, eq, ord]

  let to_string = function
    | Any -> "a value"
    | Integer -> "an integer"
    | Number -> "a number"

  let admits kind (value : Value.t) =
    match (kind, value) with
    | Any, _ | _, Null -> true
    | Integer, Int _ -> true
    | Number, (Int _ | Float _) -> true
    | (Integer | Number), _ -> false
end

(* A placeholder. *)
module Param = struct
  type t = { key : Param_key.t; kind : Param_kind.t }
  [@@deriving show { with_path = false }, eq, ord]
end

(* What stands where a value will: the value, if the template spells it out, or a
   placeholder for it. *)
type t = Literal of Value.t | Param of Param.t
[@@deriving show { with_path = false }, eq]
