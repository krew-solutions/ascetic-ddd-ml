module type NULLABLE = sig
  type t

  val is_null : t -> bool
end

module Make (N : NULLABLE) = struct
  let is_null = function Ast.Value value -> N.is_null value | _ -> false

  (* [test] of the operand that stands beside the null constant, or [compare] of the two
     if neither is one. *)
  let test_or_compare left right ~test ~compare =
    if is_null right then test left
    else if is_null left then test right
    else compare left right

  let equal left right = test_or_compare left right ~test:Ast.is_null ~compare:Ast.eq

  let not_equal left right =
    test_or_compare left right ~test:Ast.is_not_null ~compare:Ast.ne

  let rec throughout = function
    | Ast.Infix (left, Comparison Eq, right) -> equal (throughout left) (throughout right)
    | Infix (left, Comparison Ne, right) -> not_equal (throughout left) (throughout right)
    | Infix (left, op, right) -> Infix (throughout left, op, throughout right)
    | Prefix (op, operand) -> Prefix (op, throughout operand)
    | Postfix (operand, op) -> Postfix (throughout operand, op)
    | Any (source, predicate) -> Any (source, throughout predicate)
    | (Value _ | Field _) as leaf -> leaf
end
