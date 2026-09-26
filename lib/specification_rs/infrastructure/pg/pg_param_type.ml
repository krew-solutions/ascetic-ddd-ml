(* The type PostgreSQL is told a constant has, where nothing else tells it.

   A constant is a numbered parameter, and the server finds its type from what stands
   beside it: ["age" >= $1] makes [$1] whatever [age] is, and the driver writes the value
   as that. Where every operand of an operator is a constant there is nothing beside it:
   [$1 + $2] is "operator is not unique: unknown + unknown", [-$1] likewise, and of
   [$1 IS NULL] the server "could not determine data type". So it is to a driver that
   asks the server; Python's sends a type with each value, an integer's by its size, and
   the server computed [1 << 63] in sixteen bits: 0.

   So there, and only there, the text says the type: [$1::bigint + $2::bigint]. Not
   everywhere. A value adapts to the column it meets - an integer is written as [int2],
   [int4], [int8] or a float, a point in time as a timestamp with or without zone - and a
   type said beside a column takes that away: ["at" = $1::timestamptz] of a column
   without zone is compared in the session's time zone, and selects other rows than
   ["at" = $1] does.

   The one column that is cast is the count of a shift, ["a" << "b"::integer]:
   PostgreSQL shifts by an [integer] and by nothing else, and a column there, a [bigint]
   more often than not, is "operator does not exist: bigint << bigint". A cast of the
   count takes nothing away - the operator has it an integer already - and turns a column
   of any integer type into the one the operator has. *)

(* What PostgreSQL calls the type of a value, by its kind. *)
module type S = sig
  type t

  val param_type : t -> string option
  (** The name of the type, or none for a value of no kind: the null. *)

  val nul_in_text : t -> bool
  (** Whether the value is a text with a NUL in it: no text PostgreSQL has, [text] holds
      none. The compiler refuses such a value where it meets the server, rather than let
      the driver or the server fail the query. *)
end

module Of_value : S with type t = Value.t = struct
  type t = Value.t

  let nul_in_text = function Value.Text text -> String.contains text '\000' | _ -> false

  let param_type = function
    | Value.Null -> None
    | Bool _ -> Some "boolean"
    | Int _ -> Some "bigint"
    | Float _ -> Some "double precision"
    | Text _ -> Some "text"
    | Timestamp _ -> Some "timestamptz"
    | Interval _ -> Some "interval"
end

let is_shift : Operator.infix -> bool = function
  | Arithmetic (Shl | Shr) -> true
  | _ -> false

(* The type to say of the operand of a prefix operator, if it is a constant: it has
   nothing beside it to take a type from. A null has no kind, and takes what the operator
   is of: a number under [-]; under [NOT] the server finds [boolean] by itself. *)
let under_prefix ~param_type (op : Operator.prefix) operand =
  match operand with
  | Ast.Value value -> (
      match param_type value with
      | Some said -> Some said
      | None -> ( match op with Neg -> Some "bigint" | Not -> None))
  | _ -> None

(* Whether [right] is the count of a shift that must be said an integer. A constant there
   is inferred, or was said an integer already where nothing stands beside it; a column or
   an expression has a type of its own, which the server will not convert, so it is
   cast. *)
let is_a_count_to_cast op right =
  is_shift op && match right with Ast.Value _ -> false | _ -> true

(* The type to say of the operand of a null test, if it is a constant. Of what type a null
   is tested does not matter, and the server must be told one: "could not determine data
   type of parameter". *)
let under_postfix ~param_type operand =
  match operand with
  | Ast.Value value -> (
      match param_type value with Some said -> Some said | None -> Some "text")
  | _ -> None

(* The types to say of the operands of [op], if both are constants: neither has anything
   beside it to take a type from. PostgreSQL shifts a [bigint] by an [integer], so the
   count of a shift is that. Two nulls take what the operator is of: numbers under
   arithmetic; compared, the server takes them for texts by itself. *)
let of_both ~param_type left (op : Operator.infix) right =
  match (left, right) with
  | Ast.Value left, Ast.Value right -> (
      let count = function
        | Some "bigint" when is_shift op -> Some "integer"
        | other -> other
      in
      match (param_type left, param_type right, op) with
      | None, None, Arithmetic _ -> (Some "bigint", count (Some "bigint"))
      | left, right, _ -> (left, count right))
  | _ -> (None, None)
