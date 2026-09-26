(* From the tokens of a template to its tree: one function per rule of the grammar in the
   documentation of [Jsonpath], each taking the input left and returning what it read with
   the input left after it.

   The sources carry a mutable context through the parser - whether [@] is an item just
   now, which placeholder comes next - and step over a bracket or a parenthesis where one
   "may be present", so that a template with none, or with one too many, is read as if it
   were well formed. Here what [@] means is an argument, the placeholders were numbered by
   the lexer, and a token the grammar asks for is required. *)

open Jsonpath_lexer
module Slot = Jsonpath_slot

(* How tall a template's tree may be: the levels of the longest way down it. Every reader
   of a tree recurses, so an unbounded tree from a text that is not trusted is a stack
   overflow; no specification a person wrote comes near. *)
let max_height = 128

(* How deep the parser may go to read a template: through groups, [!] and the filters of
   collections, which is where it recurses.

   The two are counted apart, for they are not one number. How deep the parser is comes
   down to a rule from the rule that called it. How tall a tree is comes up from the trees
   below it, and grows wherever a node is made - in the loop of a chain as well, where the
   parser does not recurse at all, and above a left operand, which was read before
   anything knew it would have an operator over it. *)
let max_nesting = 32

(* The tokens not yet read, and where the template ends - the position of an error met
   there. *)
type input = { tokens : token array; at : int; end_ : int }

let kind input =
  if input.at < Array.length input.tokens then Some input.tokens.(input.at).kind else None

let position input =
  if input.at < Array.length input.tokens then input.tokens.(input.at).position
  else input.end_

let advance input = { input with at = input.at + 1 }
let error input message expected = Jsonpath_error.make message (position input) expected

(* The input after [kind], which must come next. *)
let expect input wanted message expected =
  if kind input = Some wanted then Ok (advance input)
  else Error (error input message expected)

let unexpected input expected =
  match kind input with
  | Some kind ->
      error input (Printf.sprintf "Unexpected token '%s'" (spelling kind)) expected
  | None -> error input "Unexpected end of expression" expected

let ( let* ) = Result.bind

(* A tree, and how tall it is: the levels of the longest way down it. *)
type tree = { expr : Slot.t Ast.t; height : int }

let leaf expr = { expr; height = 1 }
let too_deep at = error at "Expression is nested too deep" "a simpler expression"

(* A level above [height], if a tree may be that tall. [at] is where the text asks for
   the node, for the error to point at if it may not be made. *)
let taller at height =
  if height < max_height then Ok (height + 1) else Error (too_deep at)

(* A level below [depth], if the parser may go that deep. *)
let deeper at depth = if depth < max_nesting then Ok (depth + 1) else Error (too_deep at)

(* [make] of [operand], a level taller than it. *)
let over at make operand =
  let* height = taller at operand.height in
  Ok { height; expr = make operand.expr }

(* [make] of [left] and [right], a level taller than the taller of them. *)
let over_both at make left right =
  let* height = taller at (max left.height right.height) in
  Ok { height; expr = make left.expr right.expr }

(* Python's [%] takes a tuple or a mapping, never both; a template that mixes the two
   styles has no parameters it could be bound to. *)
let one_style tokens =
  let find style =
    List.find_opt
      (fun token ->
        match token.kind with
        | Placeholder { Slot.Param.key; _ } -> style key
        | _ -> false)
      (Array.to_list tokens)
  in
  let named = find (function Slot.Param_key.Name _ -> true | _ -> false) in
  let positional = find (function Slot.Param_key.Position _ -> true | _ -> false) in
  match (named, positional) with
  | Some first, Some second ->
      Error
        (Jsonpath_error.make "Positional and named placeholders in one template"
           (max first.position second.position)
           "placeholders of one style")
  | _ -> Ok ()

(* [path = ( "." name )*] *)
let rec names input acc =
  if kind input = Some Dot then
    match kind (advance input) with
    | Some (Name name) -> names (advance (advance input)) (name :: acc)
    | _ -> Error (error (advance input) "Expected field name" "after '.'")
  else Ok (List.rev acc, input)

(* The path of [names] from [root], if there are any. *)
let collection root = function
  | [] -> None
  | first :: rest -> Some (List.fold_left Path.child (Path.make root first) rest)

(* ["[*]"] *)
let wildcard input =
  let expected = "after path" and message = "Expected wildcard '[*]'" in
  let* input = expect input Left_bracket message expected in
  let* input = expect input Star message expected in
  expect input Right_bracket message expected

(* [filter = "[" "?" or "]"]. [scope] is what [@] means inside, [depth] how deep the
   parser is. *)
let rec filter input scope depth =
  let message = "Expected filter expression '[?...]'" in
  let* input = expect input Left_bracket message "'['" in
  let* input = expect input Question message "'?'" in
  let* predicate, input = or_ input scope depth in
  let* input = expect input Right_bracket "Expected ']'" "end of filter expression" in
  Ok (predicate, input)

(* [or = and ( "||" and )*], nested to the left. *)
and or_ input scope depth = chain input Or Ast.or_ (fun input -> and_ input scope depth)

(* [and = unary ( "&&" unary )*], nested to the left. *)
and and_ input scope depth =
  chain input And Ast.and_ (fun input -> unary input scope depth)

(* [operand ( separator operand )*], nested to the left. A loop and not a recursion, as
   the nesting is: the parser gets no deeper, and the tree a level taller with each
   operand. *)
and chain input separator make operand =
  let* left, input = operand input in
  let rec links left input =
    if kind input = Some separator then
      let* right, rest = operand (advance input) in
      let* left = over_both input make left right in
      links left rest
    else Ok (left, input)
  in
  links left input

(* [unary = "!" unary | comparison] *)
and unary input scope depth =
  if kind input = Some Not then
    let* depth = deeper input depth in
    let* operand, rest = unary (advance input) scope depth in
    let* tree = over input Ast.not_ operand in
    Ok (tree, rest)
  else comparison input scope depth

(* [comparison = operand ( ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) operand )?] - at
   most one: a comparison does not associate. *)
and comparison input scope depth =
  let* left, input = operand input scope depth in
  let make =
    match kind input with
    | Some Eq -> Some Ast.eq
    | Some Ne -> Some Ast.ne
    | Some Gt -> Some Ast.gt
    | Some Ge -> Some Ast.ge
    | Some Lt -> Some Ast.lt
    | Some Le -> Some Ast.le
    | _ -> None
  in
  match make with
  | None -> Ok (left, input)
  | Some make ->
      let* right, rest = operand (advance input) scope depth in
      let* tree = over_both input make left right in
      Ok (tree, rest)

(* [operand = "(" or ")" | literal | placeholder | query] *)
and operand input scope depth =
  let literal value = Ok (leaf (Ast.Value (Slot.Literal value)), advance input) in
  let is_word name word = String.lowercase_ascii name = word in
  match kind input with
  | Some Left_paren ->
      let* depth = deeper input depth in
      let* inner, rest = or_ (advance input) scope depth in
      let* rest = expect rest Right_paren "Expected ')'" "closing parenthesis" in
      Ok (inner, rest)
  | Some (Int value) -> literal (Value.Int value)
  | Some (Float value) -> literal (Value.Float value)
  | Some (Text value) -> literal (Value.Text value)
  | Some (Name name) when is_word name "true" -> literal (Value.Bool true)
  | Some (Name name) when is_word name "false" -> literal (Value.Bool false)
  | Some (Name name) when is_word name "null" -> literal Value.Null
  | Some (Placeholder param) -> Ok (leaf (Ast.Value (Slot.Param param)), advance input)
  | Some At -> query (advance input) scope depth
  | Some Dollar -> query (advance input) Path.Global depth
  | _ ->
      Error
        (unexpected input "value (number, string, boolean, null or placeholder) or query")

(* [query = ( "@" | "$" ) path ( "[*]" filter )?], after its first token: the value of a
   member, or a collection with a predicate on its items. *)
and query input root depth =
  let* names, input = names input [] in
  let* path =
    Option.to_result
      ~none:(error input "Expected field name" "after '@' or '$'")
      (collection root names)
  in
  if kind input = Some Left_bracket then
    let* depth = deeper input depth in
    let* after = wildcard input in
    let* predicate, rest = filter after (Path.Item 0) depth in
    let* tree = over input (fun predicate -> Ast.any_at path predicate) predicate in
    Ok (tree, rest)
  else Ok (leaf (Ast.Field path), input)

(* [template = "$" ( filter | path "[*]" filter )], and nothing after it. *)
let template tokens end_ =
  let* () = one_style tokens in
  let input = { tokens; at = 0; end_ } in
  let* input = expect input Dollar "Expected '$'" "a template starts at the root" in
  let* names, after = names input [] in
  let* tree, input =
    match collection Path.Global names with
    | None -> filter after Path.Global 0
    | Some source ->
        let* after_wildcard = wildcard after in
        let* predicate, rest = filter after_wildcard (Path.Item 0) 1 in
        let* any = over after (fun predicate -> Ast.any_at source predicate) predicate in
        Ok (any, rest)
  in
  match kind input with
  | None -> Ok tree.expr
  | Some _ -> Error (unexpected input "end of expression")
