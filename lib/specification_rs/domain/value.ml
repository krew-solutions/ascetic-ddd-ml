module Micros = struct
  type t = int64 [@@deriving show { with_path = false }, eq, ord]

  let of_micros micros = micros
  let to_micros micros = micros
end

module Timestamp = Micros
module Interval = Micros

module Date = struct
  type t = int [@@deriving show { with_path = false }, eq, ord]

  let of_days days = days
  let to_days days = days

  let of_civil year month day =
    Option.map
      (fun midnight -> fst (Ptime.Span.to_d_ps (Ptime.to_span midnight)))
      (Ptime.of_date (year, month, day))

  (* The civil date of a day count from 1970-01-01: Howard Hinnant's algorithm, for a
     calendar that the day may lie outside a calendar library's range of. *)
  let to_civil days =
    let days = days + 719_468 in
    let era = (if days >= 0 then days else days - 146_096) / 146_097 in
    let doe = days - (era * 146_097) in
    let yoe = (doe - (doe / 1460) + (doe / 36_524) - (doe / 146_096)) / 365 in
    let year = yoe + (era * 400) in
    let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
    let mp = ((5 * doy) + 2) / 153 in
    let day = doy - (((153 * mp) + 2) / 5) + 1 in
    let month = if mp < 10 then mp + 3 else mp - 9 in
    let year = if month <= 2 then year + 1 else year in
    (year, month, day)
end

type t =
  | Null
  | Bool of bool
  | Int of int64
  | Float of float
  | Text of string
  | Timestamp of Timestamp.t
  | Date of Date.t
  | Interval of Interval.t
  | Uuid of Uuidm.t
[@@deriving show { with_path = false }, eq]

let null = Null
let is_null = function Null -> true | _ -> false
let of_bool value = Bool value
let to_bool = function Bool value -> Some value | _ -> None
let of_int value = Int (Int64.of_int value)
let of_float value = Float value
let of_string value = Text value
let of_option f = function None -> Null | Some value -> f value

let kind = function
  | Null -> "null"
  | Bool _ -> "boolean"
  | Int _ -> "integer"
  | Float _ -> "float"
  | Text _ -> "text"
  | Timestamp _ -> "timestamp"
  | Date _ -> "date"
  | Interval _ -> "interval"
  | Uuid _ -> "uuid"

(* PostgreSQL's order of floats: [NaN] equals itself and is greater than everything
   else; [-0] equals [0]. *)
let float_order left right =
  match (Float.is_nan left, Float.is_nan right) with
  | true, true -> 0
  | true, false -> 1
  | false, true -> -1
  | false, false -> Float.compare left right

let compare left right =
  match (left, right) with
  (* False before true: PostgreSQL's order, OCaml's and Python's. The Go source compares
     truth values for equality only. *)
  | Bool left, Bool right -> Ok (Bool.compare left right)
  | Int left, Int right -> Ok (Int64.compare left right)
  | Float left, Float right -> Ok (float_order left right)
  | Int left, Float right -> Ok (float_order (Int64.to_float left) right)
  | Float left, Int right -> Ok (float_order left (Int64.to_float right))
  | Text left, Text right -> Ok (String.compare left right)
  | Timestamp left, Timestamp right -> Ok (Int64.compare left right)
  | Date left, Date right -> Ok (Int.compare left right)
  | Interval left, Interval right -> Ok (Int64.compare left right)
  (* By its bytes, as PostgreSQL orders a uuid. *)
  | Uuid left, Uuid right -> Ok (Uuidm.compare left right)
  | _ ->
      Error
        (Operand.unsupported (Operator.comparison_to_string Lt) (kind left) (kind right))

let equals left right = Result.map (fun order -> order = 0) (compare left right)

(* ------------------------------------------------------------------------ *)
(* Integers: 64-bit, checked                                                 *)

let checked result = Option.to_result ~none:Operand.Out_of_range result

let checked_add left right =
  let sum = Int64.add left right in
  (* The sum overflowed if the operands share a sign and the sum has the other. *)
  if Int64.compare (Int64.logand (Int64.logxor left sum) (Int64.logxor right sum)) 0L < 0
  then None
  else Some sum

let checked_sub left right =
  let difference = Int64.sub left right in
  if
    Int64.compare
      (Int64.logand (Int64.logxor left right) (Int64.logxor left difference))
      0L
    < 0
  then None
  else Some difference

let checked_mul left right =
  if left = 0L || right = 0L then Some 0L
  else if (left = -1L && right = Int64.min_int) || (right = -1L && left = Int64.min_int)
  then None
  else
    let product = Int64.mul left right in
    if Int64.div product right = left then Some product else None

let checked_neg value = if value = Int64.min_int then None else Some (Int64.neg value)

(* Every operator is defined for two integers. *)
let integer (op : Operator.arithmetic) left right =
  match op with
  | Add -> checked (checked_add left right)
  | Sub -> checked (checked_sub left right)
  | Mul -> checked (checked_mul left right)
  | (Div | Mod) when right = 0L -> Error Operand.Division_by_zero
  | Div when left = Int64.min_int && right = -1L -> Error Operand.Out_of_range
  | Div -> Ok (Int64.div left right)
  (* [min_int % -1] overflows in C and is 0 in PostgreSQL. *)
  | Mod -> Ok (if right = -1L then 0L else Int64.rem left right)
  (* PostgreSQL takes the count modulo 64, a negative count included. *)
  | Shl -> Ok (Int64.shift_left left (Int64.to_int right land 63))
  | Shr -> Ok (Int64.shift_right left (Int64.to_int right land 63))

(* ------------------------------------------------------------------------ *)
(* Floats                                                                    *)

(* [None] if the operator is not one of floats.

   A result PostgreSQL has no float for is out of range, at either end: an infinity of
   operands that are finite, and a zero of operands that are not - an underflow, which
   IEEE arithmetic rounds to zero in silence. One divided by infinity is a zero, and
   right. A NaN divided by zero is a NaN, as it is divided by anything; any other number
   divided by zero is the error. *)
let float (op : Operator.arithmetic) left right =
  let finite value = Float.is_finite value in
  match op with
  | Mod | Shl | Shr -> None
  | Div when right = 0.0 && not (Float.is_nan left) ->
      Some (Error Operand.Division_by_zero)
  | Add | Sub | Mul | Div ->
      let result =
        match op with
        | Add -> left +. right
        | Sub -> left -. right
        | Mul -> left *. right
        | Div -> left /. right
        | Mod | Shl | Shr -> assert false
      in
      let overflow = Float.abs result = Float.infinity && finite left && finite right in
      let underflow =
        result = 0.0
        &&
        match op with
        | Mul -> left <> 0.0 && right <> 0.0
        | Div -> left <> 0.0 && finite right
        | _ -> false
      in
      Some
        (if overflow || underflow then Error Operand.Out_of_range else Ok (Float result))

(* ------------------------------------------------------------------------ *)
(* Time                                                                      *)

(* [None] if the operator is not one of these two. A point less a point is a span; a point
   and a span make a point; spans add up. *)
let temporal (op : Operator.arithmetic) left right =
  let point micros = Timestamp micros and span micros = Interval micros in
  let of_checked wrap result = Some (Result.map wrap (checked result)) in
  match (left, op, right) with
  | Timestamp l, Sub, Timestamp r -> of_checked span (checked_sub l r)
  | Timestamp l, Add, Interval r | Interval r, Add, Timestamp l ->
      of_checked point (checked_add l r)
  | Timestamp l, Sub, Interval r -> of_checked point (checked_sub l r)
  | Interval l, Add, Interval r -> of_checked span (checked_add l r)
  | Interval l, Sub, Interval r -> of_checked span (checked_sub l r)
  | _ -> None

let negate = function
  | Int value -> Result.map (fun value -> Int value) (checked (checked_neg value))
  | Float value -> Ok (Float (-.value))
  | Interval value ->
      Result.map (fun value -> Interval value) (checked (checked_neg value))
  | value ->
      Error (Operand.unsupported_unary (Operator.prefix_to_string Neg) (kind value))

let compute op left right =
  let defined =
    match (left, right) with
    | Int l, Int r -> Some (Result.map (fun value -> Int value) (integer op l r))
    | Float l, Float r -> float op l r
    | Int l, Float r -> float op (Int64.to_float l) r
    | Float l, Int r -> float op l (Int64.to_float r)
    | _ -> temporal op left right
  in
  match defined with
  | Some result -> result
  | None ->
      Error
        (Operand.unsupported (Operator.arithmetic_to_string op) (kind left) (kind right))

(* ------------------------------------------------------------------------ *)
(* Reading                                                                   *)

let read_beside this other =
  match this with
  | Text text -> (
      let unreadable form =
        Error (Operand.Unreadable { text; kind = kind other; form })
      in
      let read parse wrap form =
        match parse text with
        | Some read -> Ok (Some (wrap read))
        | None -> unreadable form
      in
      match other with
      | Timestamp _ ->
          read Reading.point_in_time
            (fun micros -> Timestamp (Timestamp.of_micros micros))
            Reading.point_in_time_form
      | Date _ ->
          read Reading.calendar_date
            (fun days -> Date (Date.of_days days))
            Reading.point_in_time_form
      | Uuid _ -> read Reading.uuid (fun id -> Uuid id) Reading.uuid_form
      | _ -> Ok None)
  | _ -> Ok None
