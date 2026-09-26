module Schema = Pg_schema
module Foreign_key = Pg_schema.Foreign_key

type error = Pg_error.t =
  | No_current_item
  | No_table
  | Ambiguous_key of string
  | Wrong_key of string
  | Invalid_identifier of string
  | Nul_in_text
[@@deriving show { with_path = false }, eq]

let error_to_string = Pg_error.to_string

type 'v query = { sql : string; params : 'v list }
[@@deriving show { with_path = false }, eq]

module type PARAM_TYPE = Pg_param_type.S

let ( let* ) = Result.bind
let traverse = Pg_identifier.traverse
let identifier = Pg_identifier.identifier
let qualified = Pg_identifier.qualified
let quoted = Pg_identifier.quoted

(* How many parameters and aliases have been numbered so far. *)
type next = { param_count : int; alias_count : int }

(* The item under test, inside a collection's predicate: what its row is called in the
   query, what it is a row of to the schema - a table, or the composite at a column of one
   - and the item of the enclosing collection's predicate, if there is one. *)
type item = { alias : string; row : string; outer : item option }

(* The item [up] collections out from the item under test: the aliases of the enclosing
   queries are in scope of the inner one, as SQL has it. *)
let rec item_out item up =
  match (item, up) with
  | None, _ -> Error No_current_item
  | Some item, 0 -> Ok item
  | Some item, up -> item_out item.outer (up - 1)

(* The singular of the last name of [table], in lower case: what an alias is made of. *)
let singular_of table =
  let name =
    match List.rev (String.split_on_char '.' table) with last :: _ -> last | [] -> table
  in
  Result.map
    (fun plain -> Pg_singular.singular (String.lowercase_ascii plain))
    (Pg_identifier.plain name)

let spelling : Operator.infix -> string = function
  | Is -> "IS NOT DISTINCT FROM"
  | (Comparison _ | Logical _ | Arithmetic _) as op -> Operator.infix_to_string op

(* A piece of the condition, and how tightly its outermost operator binds. *)
type 'v fragment = { text : string; values : 'v list; precedence : int }

let of_text text = { text; values = []; precedence = Pg_precedence.atom }

(* [fragment], as an operand of an operator of [precedence]: parenthesised if it binds
   looser, or as tight and [apart] - on the side the operator does not group towards. *)
let within fragment precedence apart =
  if fragment.precedence < precedence || (fragment.precedence = precedence && apart) then
    { fragment with text = "(" ^ fragment.text ^ ")"; precedence = Pg_precedence.atom }
  else fragment

(* [fragment], its type said if there is one to say: a cast binds tighter than any
   operator, so what was an atom is one still. *)
let of_type fragment = function
  | Some param_type -> { fragment with text = fragment.text ^ "::" ^ param_type }
  | None -> fragment

(* The same parameters under other text. *)
let with_text fragment text precedence = { fragment with text; precedence }

module Make (P : PARAM_TYPE) = struct
  type compiler = { schema : Schema.t option }

  (* The key a collection named [name] in a row of [row] is joined by: the one of that
     name, if it references the row; else the one key on the table [name] that does.
     None: the name is an array in the row. *)
  let key_of_collection compiler row name =
    match compiler.schema with
    | None -> Ok None
    | Some schema -> (
        match Schema.key_named schema name with
        | Some key ->
            if not (String.equal (Foreign_key.referenced_table key) row) then
              Error
                (Wrong_key
                   (Printf.sprintf "the key %s references %s, not %s" name
                      (Foreign_key.referenced_table key)
                      row))
            else Ok (Some key)
        | None -> (
            match Schema.keys_referencing schema name row with
            | [] -> Ok None
            | [ key ] -> Ok (Some key)
            | keys ->
                Error
                  (Ambiguous_key
                     (Printf.sprintf "%s has %d keys to %s: %s; name the key" name
                        (List.length keys) row
                        (String.concat ", " (List.map Foreign_key.name keys))))))

  (* The key an object named [name] in a row of [row] is read through: the one of that
     name, if it is on the row; else the one key on the row that [name] is a column of.
     None: the name is a composite in the row. *)
  let key_of_object compiler row name =
    match compiler.schema with
    | None -> Ok None
    | Some schema -> (
        match Schema.key_named schema name with
        | Some key ->
            if not (String.equal (Foreign_key.table key) row) then
              Error
                (Wrong_key
                   (Printf.sprintf "the key %s is on %s, not %s" name
                      (Foreign_key.table key) row))
            else Ok (Some key)
        | None -> (
            match Schema.keys_on schema row name with
            | [] -> Ok None
            | [ key ] -> Ok (Some key)
            | keys ->
                Error
                  (Ambiguous_key
                     (Printf.sprintf "%s is a column of %d keys of %s: %s; name the key"
                        name (List.length keys) row
                        (String.concat ", " (List.map Foreign_key.name keys))))))

  (* Whether [operand] is a column the schema declares a composite. The column is named
     as a key names it: by its table, or by the array it is a row of, [stores.items]; a
     composite inside a composite by the column, [stores.discount]. *)
  let is_composite_column compiler operand item =
    match (operand, compiler.schema) with
    | Ast.Field path, Some schema -> (
        match List.rev (Path.names path) with
        | [] -> Ok false
        | column :: owners_reversed ->
            let owners = List.rev owners_reversed in
            let* of_ =
              match Path.root path with
              | Item up -> Result.map (fun item -> item.row) (item_out item up)
              | Global -> Ok (Schema.table schema)
            in
            Ok (Schema.is_composite schema (String.concat "." (of_ :: owners)) column))
    | _ -> Ok false

  (* The words of a null test: of the value as a whole, for a column the schema declares
     a composite.

     Of a composite [IS NULL] is true when all its members are null and [IS NOT NULL] when
     none is - the standard's null predicate over a row value - so a row with a null
     member is neither. An option of a Value Object is [Some] or [None] whatever its
     members hold, and so is the column: null, or a row. [IS DISTINCT FROM NULL] tests
     that, as the manual advises; a [None] is written as a null column, not as a row of
     nulls. *)
  let null_test compiler (op : Operator.postfix) operand item =
    let* composite = is_composite_column compiler operand item in
    if not composite then Ok (Operator.postfix_to_string op)
    else
      Ok
        (match op with
        | Is_null -> "IS NOT DISTINCT FROM NULL"
        | Is_not_null -> "IS DISTINCT FROM NULL")

  (* The path as a column reference, no object on its way looked up: the place of a
     collection, and a name from the candidate.

     From the candidate the names are written with dots, which PostgreSQL reads as a
     qualified name: ["s"."price"] is the column [price] of [s]. From the item under test
     the first name is a column of the item's row, under its alias, and what follows a
     member of a composite kept there. *)
  let column compiler path item =
    let* names = traverse identifier (Path.names path) in
    match (Path.root path, names) with
    (* Inside a collection's predicate the candidate's column is qualified with its row:
       unqualified, PostgreSQL reads it from the innermost row that has a column of that
       name, and a category with a [limit] of its own hid the shop's. A name of several
       parts the author qualified. *)
    | Global, [ name ] when Option.is_some item ->
        let* schema = Option.to_result ~none:No_table compiler.schema in
        let* row = qualified (Schema.row schema) in
        Ok (row ^ "." ^ name)
    | Global, names -> Ok (String.concat "." names)
    | Item up, names -> (
        let* item = item_out item up in
        let alias = quoted item.alias in
        match names with
        | column :: (_ :: _ as members) ->
            Ok (Printf.sprintf "(%s.%s).%s" alias column (String.concat "." members))
        | _ -> Ok (alias ^ "." ^ String.concat "." names))

  (* The member at [names] of the row written [row], which is a row of [of_] to the
     schema: a table, or the composite at a column of one.

     An object kept in a table of its own is read by a subquery in the column's place: it
     has at most the one row the key names, and is null if there is none, as a member of
     a composite that is null is. An object not mentioned is a composite in its row, and
     the parentheses are what makes it that: with dots alone PostgreSQL reads a schema, a
     table and a column, and there is no such table. *)
  let rec member_of_row compiler row of_ names next =
    match names with
    | [] -> Ok (row, next)
    | [ name ] ->
        let* name = identifier name in
        Ok (row ^ "." ^ name, next)
    | object_ :: rest -> (
        let* key = key_of_object compiler of_ object_ in
        match key with
        | None ->
            let* object_name = identifier object_ in
            member_of_row compiler
              (Printf.sprintf "(%s.%s)" row object_name)
              (of_ ^ "." ^ object_)
              rest next
        | Some key ->
            (* The row read is one of the referenced table, and its alias says so. *)
            let number = next.alias_count + 1 in
            let* singular = singular_of (Foreign_key.referenced_table key) in
            let alias = quoted (Printf.sprintf "%s_%d" singular number) in
            let* keys =
              traverse
                (fun (column, referenced) ->
                  let* referenced = identifier referenced in
                  let* column = identifier column in
                  Ok (Printf.sprintf "%s.%s = %s.%s" alias referenced row column))
                (List.combine (Foreign_key.columns key)
                   (Foreign_key.referenced_columns key))
            in
            let next = { next with alias_count = number } in
            let* member, next =
              member_of_row compiler alias (Foreign_key.referenced_table key) rest next
            in
            let* table = qualified (Foreign_key.referenced_table key) in
            Ok
              ( Printf.sprintf "(SELECT %s FROM %s AS %s WHERE %s)" member table alias
                  (String.concat " AND " keys),
                next ))

  (* The value of the member at [path], as the storage has it.

     An object on the way to the member is looked up in the schema, as a collection is,
     by the names that lead to it. Kept in a table of its own, it is reached through its
     key. Not mentioned, it is what the dots have meant so far: from the item under test
     a composite kept in the item's row - a Value Object; from the candidate a qualifier
     of the name, ["s"."price"]. *)
  let member compiler path item next =
    let names = Path.names path in
    match Path.root path with
    | Item up ->
        let* item = item_out item up in
        member_of_row compiler (quoted item.alias) item.row names next
    (* An object of the candidate kept in a table of its own, or a composite column of
       its row - a Value Object - that the schema says is one: the dots of an undeclared
       name are a qualifier. *)
    | Global -> (
        match (names, compiler.schema) with
        | object_ :: _ :: _, Some schema ->
            let* key = key_of_object compiler (Schema.table schema) object_ in
            if
              Option.is_some key
              || Schema.is_composite schema (Schema.table schema) object_
            then
              let* row = qualified (Schema.row schema) in
              member_of_row compiler row (Schema.table schema) names next
            else
              let* column = column compiler path item in
              Ok (column, next)
        | _ ->
            let* column = column compiler path item in
            Ok (column, next))

  let rec render compiler expr item next =
    match expr with
    | Ast.Value value ->
        (* In memory such a text is a string like any other; here it meets the server,
           which has no such text. *)
        if P.nul_in_text value then Error Nul_in_text
        else
          let param = next.param_count + 1 in
          let fragment =
            {
              text = Printf.sprintf "$%d" param;
              values = [ value ];
              precedence = Pg_precedence.atom;
            }
          in
          Ok (fragment, { next with param_count = param })
    | Field path ->
        let* text, next = member compiler path item next in
        Ok (of_text text, next)
    | Prefix (op, operand) ->
        let precedence = Pg_precedence.prefix op in
        let alone = Pg_param_type.under_prefix ~param_type:P.param_type op operand in
        let* operand, next = render compiler operand item next in
        let operand = of_type operand alone in
        (* [NOT NOT a] reads as it should; [--a] reads as a comment. *)
        let doubled = op = Neg && operand.precedence = precedence in
        let operand = within operand precedence doubled in
        let text =
          match op with Not -> "NOT " ^ operand.text | Neg -> "-" ^ operand.text
        in
        Ok (with_text operand text precedence, next)
    | Postfix (operand, op) ->
        let precedence = Pg_precedence.postfix op in
        let alone = Pg_param_type.under_postfix ~param_type:P.param_type operand in
        let* test = null_test compiler op operand item in
        let* operand, next = render compiler operand item next in
        let operand = within (of_type operand alone) precedence true in
        Ok (with_text operand (operand.text ^ " " ^ test) precedence, next)
    | Infix (left, op, right) ->
        let precedence, associativity = Pg_precedence.infix op in
        let apart side = associativity <> side && not (Pg_precedence.regroups op) in
        let of_left, of_right =
          Pg_param_type.of_both ~param_type:P.param_type left op right
        in
        let count = Pg_param_type.is_a_count_to_cast op right in
        let* left, next = render compiler left item next in
        let* right, next = render compiler right item next in
        let left = of_type left of_left in
        (* A cast binds tighter than any operator: the count of a shift is parenthesised
           against the cast, if it is not an atom, before its type is said:
           [("b" + $1)::integer]. *)
        let right =
          if count then of_type (within right Pg_precedence.cast false) (Some "integer")
          else of_type right of_right
        in
        let left = within left precedence (apart Left) in
        let right = within right precedence (apart Right) in
        let text = Printf.sprintf "%s %s %s" left.text (spelling op) right.text in
        Ok ({ text; values = left.values @ right.values; precedence }, next)
    | Any (source, predicate) -> exists compiler source predicate item next

  (* [EXISTS (SELECT 1 FROM ... AS alias WHERE ...)] over an array of the parent's row,
     or over the rows of a table that point at the parent. *)
  and exists compiler source predicate item next =
    let* enclosing =
      match Path.root source with
      | Global -> Ok None
      | Item up -> Result.map Option.some (item_out item up)
    in
    (* The row the collection is of, to the schema: the enclosing item's, or the root's.
       Without a schema there is no relation to look for. *)
    let of_ =
      match (enclosing, compiler.schema) with
      | Some item, _ -> Some item.row
      | None, Some schema -> Some (Schema.table schema)
      | None, None -> None
    in
    let name = String.concat "." (Path.names source) in
    (* A key is what a schema says, so it comes with its schema. *)
    let* key =
      match of_ with Some of_ -> key_of_collection compiler of_ name | None -> Ok None
    in
    let number = next.alias_count + 1 in
    (* The alias is the singular of the row's table, or of the array's name: the
       compiler's own. *)
    let* alias =
      match key with
      | Some key -> singular_of (Foreign_key.table key)
      | None -> singular_of (Path.name source)
    in
    let inner =
      {
        alias = Printf.sprintf "%s_%d" alias number;
        row =
          (match key with
          | Some key -> Foreign_key.table key
          | None -> Printf.sprintf "%s.%s" (Option.value of_ ~default:"") name);
        outer = item;
      }
    in
    let next = { next with alias_count = number } in
    let* predicate, next = render compiler predicate (Some inner) next in
    let alias = quoted inner.alias in
    match (key, compiler.schema) with
    | Some key, Some schema ->
        let* parent =
          match enclosing with
          | Some item -> Ok (quoted item.alias)
          | None -> qualified (Schema.row schema)
        in
        let* keys =
          traverse
            (fun (column, referenced) ->
              let* column = identifier column in
              let* referenced = identifier referenced in
              Ok (Printf.sprintf "%s.%s = %s.%s" alias column parent referenced))
            (List.combine (Foreign_key.columns key) (Foreign_key.referenced_columns key))
        in
        (* The predicate is an operand of the [AND] after the keys. *)
        let and_, _ = Pg_precedence.infix (Logical And) in
        let predicate = within predicate and_ false in
        let* table = qualified (Foreign_key.table key) in
        let text =
          Printf.sprintf "EXISTS (SELECT 1 FROM %s AS %s WHERE %s AND %s)" table alias
            (String.concat " AND " keys) predicate.text
        in
        Ok (with_text predicate text Pg_precedence.atom, next)
    | _ ->
        let* column = column compiler source item in
        let text =
          Printf.sprintf "EXISTS (SELECT 1 FROM unnest(%s) AS %s WHERE %s)" column alias
            predicate.text
        in
        Ok (with_text predicate text Pg_precedence.atom, next)

  let compile ?schema ?(offset = 0) specification =
    let compiler = { schema } in
    let* fragment, _ =
      render compiler specification None { param_count = offset; alias_count = 0 }
    in
    Ok { sql = fragment.text; params = fragment.values }
end

include Make (Pg_param_type.Of_value)
