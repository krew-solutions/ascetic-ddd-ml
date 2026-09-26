type prefix = Not | Neg [@@deriving show { with_path = false }, eq, ord]
type postfix = Is_null | Is_not_null [@@deriving show { with_path = false }, eq, ord]

type comparison = Eq | Ne | Gt | Lt | Ge | Le
[@@deriving show { with_path = false }, eq, ord]

type logical = And | Or [@@deriving show { with_path = false }, eq, ord]

type arithmetic = Add | Sub | Mul | Div | Mod | Shl | Shr
[@@deriving show { with_path = false }, eq, ord]

type infix =
  | Comparison of comparison
  | Is
  | Logical of logical
  | Arithmetic of arithmetic
[@@deriving show { with_path = false }, eq, ord]

let eq = Comparison Eq
let ne = Comparison Ne
let gt = Comparison Gt
let lt = Comparison Lt
let ge = Comparison Ge
let le = Comparison Le
let is = Is
let and_ = Logical And
let or_ = Logical Or
let add = Arithmetic Add
let sub = Arithmetic Sub
let mul = Arithmetic Mul
let div = Arithmetic Div
let modulo = Arithmetic Mod
let shl = Arithmetic Shl
let shr = Arithmetic Shr
let prefix_to_string = function Not -> "NOT" | Neg -> "-"
let postfix_to_string = function Is_null -> "IS NULL" | Is_not_null -> "IS NOT NULL"

let comparison_to_string = function
  | Eq -> "="
  | Ne -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="

let logical_to_string = function And -> "AND" | Or -> "OR"

let arithmetic_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Shl -> "<<"
  | Shr -> ">>"

let infix_to_string = function
  | Comparison op -> comparison_to_string op
  | Is -> "IS"
  | Logical op -> logical_to_string op
  | Arithmetic op -> arithmetic_to_string op
