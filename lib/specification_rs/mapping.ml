type 's mapped = Scalar of 's Ast.t | Composite of 's mapped list | Null of 's
[@@deriving show { with_path = false }, eq]

type ('d, 's, 'e) t = {
  field : Path.t -> ('s mapped, 'e) result;
  value : 'd -> ('s mapped, 'e) result;
}

type 'e error =
  | Mapping of 'e
  | Shape_mismatch
  | No_current_item
  | Outside_its_collection
  | Collection_not_a_place
  | Not_composite
  | Empty_composite
  | Unsupported_operator of Operator.infix
  | Unexpected_composite
[@@deriving show { with_path = false }, eq]

let error_to_string of_mapping = function
  | Mapping error -> of_mapping error
  | Shape_mismatch -> "composite expressions have different length"
  | Not_composite -> "not enough composite expressions"
  | Empty_composite -> "a composite expression has no parts"
  | Unsupported_operator op ->
      Printf.sprintf "operator \"%s\" is not supported for composite expressions"
        (Operator.infix_to_string op)
  | Unexpected_composite -> "a composite expression where a single one is needed"
  | No_current_item -> "no current item in context"
  | Outside_its_collection ->
      "the mapping put a member of an item outside its collection: the storage's path of \
       a member of an item starts with the collection's"
  | Collection_not_a_place ->
      "the mapping answered for a collection with what is not a place"

let ( let* ) = Result.bind

(* A collection whose predicate is being transformed: its whole path from the candidate,
   in the domain's names and in the storage's. *)
type collection = { domain : string list; storage : string list }

(* The collections the expression is inside of, the nearest first: the item [up]
   collections out is of [List.nth inside up]. *)
type inside = collection list

(* The path [names] from the candidate. *)
let from_candidate = function
  | [] -> None
  | first :: rest -> Some (List.fold_left Path.child (Path.global first) rest)

(* The whole path of [path] from the candidate: from an item, the path of the item's
   collection and then the names. *)
let whole path (inside : inside) =
  let* collection =
    match Path.root path with
    | Global -> Ok None
    | Item up -> (
        match List.nth_opt inside up with
        | Some collection -> Ok (Some collection)
        | None -> Error No_current_item)
  in
  let prefix =
    match collection with None -> [] | Some collection -> collection.domain
  in
  Option.to_result ~none:No_current_item (from_candidate (prefix @ Path.names path))

let rec strip_prefix prefix names =
  match (prefix, names) with
  | [], rest -> Some rest
  | p :: prefix, n :: names when String.equal p n -> strip_prefix prefix names
  | _ -> None

(* Every path from the candidate in [expr] starts with [prefix], the collection's, and
   is made a path from the item [up] collections out. *)
let rec placed_expr expr prefix up =
  match expr with
  | Ast.Field path when Path.root path = Global -> (
      match strip_prefix prefix (Path.names path) with
      | Some (first :: rest) ->
          Ok (Ast.Field (List.fold_left Path.child (Path.outer up first) rest))
      | Some [] | None -> Error Outside_its_collection)
  | Field _ | Value _ -> Ok expr
  | Prefix (op, operand) ->
      let* operand = placed_expr operand prefix up in
      Ok (Ast.Prefix (op, operand))
  | Postfix (operand, op) ->
      let* operand = placed_expr operand prefix up in
      Ok (Ast.Postfix (operand, op))
  | Infix (left, op, right) ->
      let* left = placed_expr left prefix up in
      let* right = placed_expr right prefix up in
      Ok (Ast.Infix (left, op, right))
  | Any (source, predicate) ->
      let* predicate = placed_expr predicate prefix up in
      Ok (Ast.Any (source, predicate))

let rec traverse f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = traverse f xs in
      Ok (y :: ys)

(* The storage's path of a member, put where the member was: of an item, the mapping's
   answer less the collection's, from the item. *)
let rec placed mapped root (inside : inside) =
  match root with
  | Path.Global -> Ok mapped
  | Item up -> (
      let collection = List.nth inside up in
      match mapped with
      | Scalar expr ->
          Result.map (fun expr -> Scalar expr) (placed_expr expr collection.storage up)
      | Composite parts ->
          let* parts = traverse (fun part -> placed part root inside) parts in
          Ok (Composite parts)
      | Null _ as null -> Ok null)

let scalar = function
  | Scalar expr -> Ok expr
  | Null null -> Ok (Ast.Value null)
  | Composite _ -> Error Unexpected_composite

(* An operand where it is not tested for: one expression, or several. The storage's null
   is the constant it carries, under any operator but the equality that tests for it. *)
type 's written = One of 's Ast.t | Several of 's mapped list

let written = function
  | Scalar expr -> One expr
  | Null null -> One (Ast.Value null)
  | Composite parts -> Several parts

(* [left = right], part by part: [l1 = r1 AND l2 = r2 AND ...]. *)
let rec equal left right =
  if List.length left <> List.length right then Error Shape_mismatch
  else
    let part = function
      (* A part that is the storage's null is tested for, as a whole is. *)
      | tested, Null _ | Null _, tested -> Result.map Ast.is_null (scalar tested)
      | Scalar left, Scalar right -> Ok (Ast.eq left right)
      | Composite left, Composite right -> equal left right
      | Composite _, Scalar _ | Scalar _, Composite _ -> Error Shape_mismatch
    in
    let* parts = traverse part (List.combine left right) in
    match parts with
    | [] -> Error Empty_composite
    | first :: rest -> Ok (List.fold_left Ast.and_ first rest)

let rec lower mapping expr (inside : inside) =
  match expr with
  | Ast.Value value -> Result.map_error (fun error -> Mapping error) (mapping.value value)
  | Field path -> member mapping path inside
  | Prefix (op, operand) ->
      let* operand = lower mapping operand inside in
      let* operand = scalar operand in
      Ok (Scalar (Ast.Prefix (op, operand)))
  | Postfix (operand, op) ->
      let* operand = lower mapping operand inside in
      let* operand = scalar operand in
      Ok (Scalar (Ast.Postfix (operand, op)))
  | Infix (left, op, right) -> (
      let* left = lower mapping left inside in
      let* right = lower mapping right inside in
      (* Equality with what the mapping says is the storage's null is the null test of
         the other operand. *)
      let test =
        match op with
        | Comparison Eq -> Some Ast.is_null
        | Comparison Ne -> Some Ast.is_not_null
        | _ -> None
      in
      match (left, right, test) with
      | tested, Null _, Some test | Null _, tested, Some test ->
          let* tested = scalar tested in
          Ok (Scalar (test tested))
      | left, right, _ -> (
          match (written left, written right) with
          | One left, One right -> Ok (Scalar (Ast.infix left op right))
          | Several left, Several right -> (
              match op with
              | Comparison Eq -> Result.map (fun expr -> Scalar expr) (equal left right)
              (* Unequal is "not equal in every part", which is not "unequal in every
                 part": (1, 2) and (1, 3) differ. *)
              | Comparison Ne ->
                  Result.map (fun expr -> Scalar (Ast.not_ expr)) (equal left right)
              | _ -> Error (Unsupported_operator op))
          | Several _, One _ | One _, Several _ -> Error Not_composite))
  | Any (source, predicate) -> collection mapping source predicate inside

(* The member at [path], as the mapping has it and where the member was. *)
and member mapping path inside =
  let* whole = whole path inside in
  let* mapped = Result.map_error (fun error -> Mapping error) (mapping.field whole) in
  placed mapped (Path.root path) inside

(* [any] over the collection at [source]: a collection is a member, the mapping says
   where it is, by a path from the candidate, and the predicate is transformed inside
   it. *)
and collection mapping source predicate inside =
  let* domain = whole source inside in
  let* mapped = Result.map_error (fun error -> Mapping error) (mapping.field domain) in
  let* storage =
    match mapped with
    | Scalar (Ast.Field path) when Path.root path = Global -> Ok path
    | _ -> Error Collection_not_a_place
  in
  let* source =
    match placed (Scalar (Ast.Field storage)) (Path.root source) inside with
    | Ok (Scalar (Ast.Field source)) -> Ok source
    | Ok _ -> Error Collection_not_a_place
    | Error error -> Error error
  in
  let collection = { domain = Path.names domain; storage = Path.names storage } in
  let* predicate = lower mapping predicate (collection :: inside) in
  let* predicate = scalar predicate in
  Ok (Scalar (Ast.any_at source predicate))

let transform mapping expr =
  let* lowered = lower mapping expr [] in
  scalar lowered
