type error =
  | Unsupported of { operator : string; left : string; right : string option }
  | Division_by_zero
  | Out_of_range
[@@deriving show { with_path = false }, eq]

let unsupported operator left right = Unsupported { operator; left; right = Some right }

let unsupported_unary operator operand =
  Unsupported { operator; left = operand; right = None }

let error_to_string = function
  | Unsupported { operator; left; right = Some right } ->
      Printf.sprintf "operator \"%s\" is not supported for %s and %s" operator left right
  | Unsupported { operator; left; right = None } ->
      Printf.sprintf "operator \"%s\" is not supported for %s" operator left
  | Division_by_zero -> "division by zero"
  | Out_of_range -> "the result is out of range"

module type S = sig
  type t

  val null : t
  val is_null : t -> bool
  val of_bool : bool -> t
  val to_bool : t -> bool option
  val kind : t -> string
  val equals : t -> t -> (bool, error) result
  val compare : t -> t -> (int, error) result
  val negate : t -> (t, error) result
  val compute : Operator.arithmetic -> t -> t -> (t, error) result
end
