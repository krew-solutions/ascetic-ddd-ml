type boolean = [ `Boolean ]
type number = [ `Number ]
type text = [ `Text ]
type datetime = [ `Datetime ]
type timespan = [ `Timespan ]
type date = [ `Date ]
type uuid = [ `Uuid ]
type comparable = [ number | text | datetime | date | timespan | uuid ]
type negatable = [ number | timespan ]
type not_null
type nullable
type ('s, 'n) t = Value.t Ast.t
type ('s, 'n) term = ('s, 'n) t

module Null_test = Null_test.Make (Value)

let field_at path = Ast.field_at path
let field names = Ast.field names
let of_expr expr = expr
let expr term = term
let is_null term = Ast.is_null term
let is_not_null term = Ast.is_not_null term
let not_ term = Ast.not_ term
let and_ left right = Ast.and_ left right
let or_ left right = Ast.or_ left right
let is left right = Ast.is left right
let eq left right = Null_test.equal left right
let ne left right = Null_test.not_equal left right
let gt left right = Ast.gt left right
let lt left right = Ast.lt left right
let ge left right = Ast.ge left right
let le left right = Ast.le left right
let add left right = Ast.add left right
let sub left right = Ast.sub left right
let mul left right = Ast.mul left right
let div left right = Ast.div left right
let modulo left right = Ast.modulo left right
let shl left right = Ast.left_shift left right
let shr left right = Ast.right_shift left right
let neg term = Ast.neg term

module Infix = struct
  let ( && ) = and_
  let ( || ) = or_
  let ( = ) = eq
  let ( <> ) = ne
  let ( > ) = gt
  let ( < ) = lt
  let ( >= ) = ge
  let ( <= ) = le
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = modulo
  let ( ~- ) = neg
end

module Sort (L : sig
  type literal

  val to_value : literal -> Value.t
end) =
struct
  let field = field
  let field_at = field_at
  let value literal = Ast.value (L.to_value literal)
  let nullable literal = Ast.value (Value.of_option L.to_value literal)
end

module Boolean = struct
  type nonrec t = (boolean, not_null) t

  include Sort (struct
    type literal = bool

    let to_value = Value.of_bool
  end)
end

module Null_boolean = struct
  type nonrec t = (boolean, nullable) t

  let field = field
  let field_at = field_at
  let value = Boolean.nullable
end

module Number = struct
  type nonrec t = (number, not_null) t

  let field = field
  let field_at = field_at
  let of_int value = Ast.value (Value.of_int value)
  let of_int64 value = Ast.value (Value.Int value)
  let of_float value = Ast.value (Value.Float value)
end

module Null_number = struct
  type nonrec t = (number, nullable) t

  let field = field
  let field_at = field_at
  let of_int value = Ast.value (Value.of_option Value.of_int value)
  let of_int64 value = Ast.value (Value.of_option (fun v -> Value.Int v) value)
  let of_float value = Ast.value (Value.of_option (fun v -> Value.Float v) value)
end

module Text = struct
  type nonrec t = (text, not_null) t

  include Sort (struct
    type literal = string

    let to_value = Value.of_string
  end)
end

module Null_text = struct
  type nonrec t = (text, nullable) t

  let field = field
  let field_at = field_at
  let value = Text.nullable
end

module Datetime = struct
  type nonrec t = (datetime, not_null) t

  include Sort (struct
    type literal = Value.Timestamp.t

    let to_value v = Value.Timestamp v
  end)

  let add point span = Ast.add point span
  let sub point span = Ast.sub point span
  let diff later earlier = Ast.sub later earlier
end

module Null_datetime = struct
  type nonrec t = (datetime, nullable) t

  let field = field
  let field_at = field_at
  let value = Datetime.nullable
end

module Timespan = struct
  type nonrec t = (timespan, not_null) t

  include Sort (struct
    type literal = Value.Interval.t

    let to_value v = Value.Interval v
  end)

  let add left right = Ast.add left right
  let sub left right = Ast.sub left right
end

module Null_timespan = struct
  type nonrec t = (timespan, nullable) t

  let field = field
  let field_at = field_at
  let value = Timespan.nullable
end

module Date = struct
  type nonrec t = (date, not_null) t

  include Sort (struct
    type literal = Value.Date.t

    let to_value v = Value.Date v
  end)
end

module Null_date = struct
  type nonrec t = (date, nullable) t

  let field = field
  let field_at = field_at
  let value = Date.nullable
end

module Uuid = struct
  type nonrec t = (uuid, not_null) t

  include Sort (struct
    type literal = Uuidm.t

    let to_value v = Value.Uuid v
  end)
end

module Null_uuid = struct
  type nonrec t = (uuid, nullable) t

  let field = field
  let field_at = field_at
  let value = Uuid.nullable
end
