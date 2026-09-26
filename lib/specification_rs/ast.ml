type 'v t =
  | Value of 'v
  | Field of Path.t
  | Prefix of Operator.prefix * 'v t
  | Infix of 'v t * Operator.infix * 'v t
  | Postfix of 'v t * Operator.postfix
  | Any of Path.t * 'v t
[@@deriving show { with_path = false }, eq]

let ( let* ) = Result.bind

let rec try_map_values f = function
  | Value v -> Result.map (fun w -> Value w) (f v)
  | Field path -> Ok (Field path)
  | Prefix (op, operand) ->
      let* operand = try_map_values f operand in
      Ok (Prefix (op, operand))
  | Infix (left, op, right) ->
      let* left = try_map_values f left in
      let* right = try_map_values f right in
      Ok (Infix (left, op, right))
  | Postfix (operand, op) ->
      let* operand = try_map_values f operand in
      Ok (Postfix (operand, op))
  | Any (source, predicate) ->
      let* predicate = try_map_values f predicate in
      Ok (Any (source, predicate))

let value v = Value v
let field_at path = Field path
let field names = Field (Path.of_string names)
let item name = Field (Path.item name)
let outer up name = Field (Path.outer up name)
let not_ operand = Prefix (Operator.Not, operand)
let neg operand = Prefix (Operator.Neg, operand)
let infix left op right = Infix (left, op, right)
let eq left right = infix left Operator.eq right
let ne left right = infix left Operator.ne right
let gt left right = infix left Operator.gt right
let lt left right = infix left Operator.lt right
let ge left right = infix left Operator.ge right
let le left right = infix left Operator.le right
let is left right = infix left Operator.is right
let and_ left right = infix left Operator.and_ right
let or_ left right = infix left Operator.or_ right
let and_all first rest = List.fold_left and_ first rest
let or_all first rest = List.fold_left or_ first rest
let add left right = infix left Operator.add right
let sub left right = infix left Operator.sub right
let mul left right = infix left Operator.mul right
let div left right = infix left Operator.div right
let modulo left right = infix left Operator.modulo right
let left_shift left right = infix left Operator.shl right
let right_shift left right = infix left Operator.shr right
let is_null operand = Postfix (operand, Operator.Is_null)
let is_not_null operand = Postfix (operand, Operator.Is_not_null)
let any_at source predicate = Any (source, predicate)
let any source predicate = any_at (Path.of_string source) predicate
let all_at source predicate = not_ (any_at source (not_ predicate))
let all source predicate = all_at (Path.of_string source) predicate
