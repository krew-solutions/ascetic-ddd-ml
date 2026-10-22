module Schema = Pg_schema
module Foreign_key = Pg_schema.Foreign_key
module Layout = Pg_layout
module Kind = Pg_layout.Kind

type error = Pg_error.t =
  | No_current_item
  | No_table
  | Ambiguous_key of string
  | Wrong_key of string
  | Invalid_identifier of string
  | Nul_in_text
  | Undeclared_member of string
  | Not_a_value of string
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
let key = Pg_identifier.key

(* How many parameters and aliases have been numbered so far. *)
type next = { param_count : int; alias_count : int }

(* What an item of a collection is, to the query. *)
type holds =
  | Table  (** A row of a table: its columns are named, and it is no value. *)
  | Unnested
      (** An element of an array, of a composite type or of a scalar one, that [unnest]
          names by its alias: a member of a composite, or the value. *)
  | Document of Layout.Items.t  (** An object or a value of an array in a document. *)

(* The item under test, inside a collection's predicate: what its row is called in the
   query, what it is a row of to the schema - a table, or the composite at a column of
   one, or the array at a path in a document - what it is, and the item of the enclosing
   collection's predicate, if there is one. *)
type item = { alias : string; row : string; holds : holds; outer : item option }

(* Where an expression stands: where only its truth counts - the condition as a whole,
   [AND] and [OR] there, the predicate of [any] - and a null selects as a false does; or
   where its value does. *)
type stand = Truth | Value

(* What a member is read for: its value, or whether it is null - which an object or a
   collection of a document can be said to be, and is no value. *)
type reading = As_value | As_null_test

(* A document a path goes into: written as a [jsonb], named by its table and column or
   by the array its object is an item of, what it holds, the names of the path in it,
   and the paths in the document, a column, that the schema says have a GIN index - none
   in a document's array. *)
module Entered = struct
  type t = {
    document : string;
    of_ : string;
    layout : Layout.t;
    names : string list;
    gins : string list list;
  }
end

(* A GIN index that serves a path into a document: the document, the path in it the
   index is on, and the path from the document to what is asked. *)
module Gin = struct
  type t = { document : string; at : string list; names : string list }

  let rec is_prefix prefix names =
    match (prefix, names) with
    | [], _ -> true
    | p :: prefix, n :: names -> String.equal p n && is_prefix prefix names
    | _ :: _, [] -> false

  (* The deepest of [gins] on the way to [names] in [document]: none if no index is on
     the way. *)
  let serving gins document names =
    let deepest =
      List.fold_left
        (fun deepest gin ->
          match deepest with
          | _ when not (is_prefix gin names) -> deepest
          | Some found when List.length found >= List.length gin -> deepest
          | _ -> Some gin)
        None gins
    in
    Option.map (fun at -> { document; at; names }) deepest
end

(* An array of a document, as [exists] reads it: written as a [jsonb]; the row its items
   are of, by the document's column and the names in it; what they are; and the GIN
   index on the way to it, if the document is a column that has one. *)
module Document_collection = struct
  type t = { array : string; row : string; items : Layout.Items.t; gin : Gin.t option }
end

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
    match List.rev (String.split_on_char '.' table) with
    | last :: _ -> last
    | [] -> table
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

(* [array] if it is one, and a null if it is not. [jsonb_array_elements] fails on a
   scalar, a JSON null among them, and on an object; and whether a row with one reaches
   it is the plan's to decide, not the text's - the planner orders the operands of [AND]
   by cost and skips what a false one makes moot, so the same query failed under one
   schema and selected under another. Of a null the function gives no rows: what is no
   array has no items, as a member that is not there is null (ADR-0033). *)
let if_array array =
  Printf.sprintf "CASE WHEN jsonb_typeof(%s) = 'array' THEN %s END" array array

(* A member named of an item that is a value of a document. *)
let no_members row names =
  Undeclared_member
    (Printf.sprintf "%s.%s: the items of %s are values, which have no members" row
       (String.concat "." names) row)

(* The object of the document written [document] that the member at [names] is in,
   reached by [->]; the member's name; and what the layout declares it. *)
let in_document document of_ layout names =
  match List.rev names with
  | [] ->
      Error
        (Undeclared_member
           (Printf.sprintf "%s: a path into a document names a member" of_))
  | name :: objects_reversed -> (
      let* object_, layout, at =
        List.fold_left
          (fun acc step ->
            let* object_, layout, at = acc in
            let at = at ^ "." ^ step in
            match Layout.member layout step with
            | Some (Layout.Object inner) ->
                let* key = key step in
                Ok (object_ ^ " -> " ^ key, inner, at)
            | _ ->
                Error
                  (Undeclared_member
                     (Printf.sprintf "%s is not an object of its document's layout" at)))
          (Ok (document, layout, of_))
          (List.rev objects_reversed)
      in
      match Layout.member layout name with
      | Some member -> Ok (object_, name, member)
      | None ->
          Error
            (Undeclared_member
               (Printf.sprintf "%s.%s is not a member of its document's layout" at name)))

(* The member at [names] of the document written [document], which holds what [layout]
   says and is [of_] to a message: [->] through its objects, [->>] of the member, read
   as the member's kind. Of an object or a collection, the text [->>] makes of it is null
   if the member is, and is read for that alone. *)
let document_member document of_ layout names reading =
  let* object_, name, member = in_document document of_ layout names in
  let* key = key name in
  let text = Printf.sprintf "(%s ->> %s)" object_ key in
  match (member, reading) with
  | Layout.Value kind, _ -> Ok (Kind.read kind text)
  | (Object _ | Collection _ | Values _), As_null_test -> Ok text
  | (Object _ | Collection _ | Values _), As_value ->
      Error
        (Undeclared_member
           (Printf.sprintf
              "%s.%s is an object or a collection of its document, not a value" of_
              (String.concat "." names)))

module Make (P : PARAM_TYPE) = struct
  module Containment = Pg_containment.Make (P)

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
     composite inside a composite by the column, [stores.discount]. A member of an object
     of a document is no column. *)
  let is_composite_column compiler operand item =
    match (operand, compiler.schema) with
    | Ast.Field path, Some schema -> (
        match List.rev (Path.names path) with
        | [] -> Ok false
        | column :: owners_reversed -> (
            let owners = List.rev owners_reversed in
            let* of_ =
              match Path.root path with
              | Item up ->
                  let* item = item_out item up in
                  Ok
                    (match item.holds with
                    | Document _ -> None
                    | Table | Unnested -> Some item.row)
              | Global -> Ok (Some (Schema.table schema))
            in
            match of_ with
            | None -> Ok false
            | Some of_ ->
                Ok (Schema.is_composite schema (String.concat "." (of_ :: owners)) column)
            ))
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

  (* The document at the column [names] starts with, of a row of [of_], written [row],
     as the path enters it. None: the column is not declared a document, or the path
     does not go into it. *)
  let document_at compiler of_ row names =
    match (compiler.schema, names) with
    | Some schema, column :: (_ :: _ as rest) -> (
        match Schema.layout schema of_ column with
        | Some layout ->
            let* column_name = identifier column in
            Ok
              (Some
                 Entered.
                   {
                     document = row ^ "." ^ column_name;
                     of_ = of_ ^ "." ^ column;
                     layout;
                     names = rest;
                     gins = Schema.gins schema of_ column;
                   })
        | None -> Ok None)
    | _ -> Ok None

  (* The member at [names] of the row written [row], which is a row of [of_] to the
     schema: a table, or the composite at a column of one.

     An object kept in a table of its own is read by a subquery in the column's place: it
     has at most the one row the key names, and is null if there is none, as a member of
     a composite that is null is. A column declared a document is the document, and the
     rest of the path is in it. An object not mentioned is a composite in its row, and
     the parentheses are what makes it that: with dots alone PostgreSQL reads a schema, a
     table and a column, and there is no such table. *)
  let rec member_of_row compiler row of_ names next reading =
    match names with
    | [] -> Ok (row, next)
    | [ name ] ->
        let* name = identifier name in
        Ok (row ^ "." ^ name, next)
    | object_ :: rest -> (
        match
          Option.bind compiler.schema (fun schema -> Schema.layout schema of_ object_)
        with
        | Some layout ->
            let* object_name = identifier object_ in
            let* member =
              document_member
                (row ^ "." ^ object_name)
                (of_ ^ "." ^ object_)
                layout rest reading
            in
            Ok (member, next)
        | None -> (
            let* key = key_of_object compiler of_ object_ in
            match key with
            | None ->
                let* object_name = identifier object_ in
                member_of_row compiler
                  (Printf.sprintf "(%s.%s)" row object_name)
                  (of_ ^ "." ^ object_)
                  rest next reading
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
                  member_of_row compiler alias
                    (Foreign_key.referenced_table key)
                    rest next reading
                in
                let* table = qualified (Foreign_key.referenced_table key) in
                Ok
                  ( Printf.sprintf "(SELECT %s FROM %s AS %s WHERE %s)" member table alias
                      (String.concat " AND " keys),
                    next )))

  (* The value of the member at [path], as the storage has it.

     An object on the way to the member is looked up in the schema, as a collection is,
     by the names that lead to it. Kept in a table of its own, it is reached through its
     key. A column declared a document, it is the document, and the rest of the path is
     in it. Not mentioned, it is what the dots have meant so far: from the item under
     test a composite kept in the item's row - a Value Object; from the candidate a
     qualifier of the name, ["s"."price"]. *)
  let member compiler path item next reading =
    let names = Path.names path in
    match Path.root path with
    | Item up -> (
        let* item = item_out item up in
        match item.holds with
        (* The item is an object of an array in a document. *)
        | Document (Objects layout) ->
            let* member =
              document_member (quoted item.alias) item.row layout names reading
            in
            Ok (member, next)
        | Document (Values _) -> Error (no_members item.row names)
        | Table | Unnested ->
            member_of_row compiler (quoted item.alias) item.row names next reading)
    (* An object of the candidate kept in a table of its own, a composite column of its
       row - a Value Object - or a document that the schema says is one: the dots of an
       undeclared name are a qualifier. *)
    | Global -> (
        match (names, compiler.schema) with
        | object_ :: _ :: _, Some schema ->
            let* key = key_of_object compiler (Schema.table schema) object_ in
            if
              Option.is_some key
              || Schema.is_composite schema (Schema.table schema) object_
              || Option.is_some (Schema.layout schema (Schema.table schema) object_)
            then
              let* row = qualified (Schema.row schema) in
              member_of_row compiler row (Schema.table schema) names next reading
            else
              let* column = column compiler path item in
              Ok (column, next)
        | _ ->
            let* column = column compiler path item in
            Ok (column, next))

  (* A constant: the next parameter. In memory a text with a NUL in it is a string like
     any other; here it meets the server, which has no such text. *)
  let constant value next =
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

  (* The item [up] collections out itself: the alias [unnest] gives an element, or the
     text of a document's value read as its kind. *)
  let itself up item next =
    let* item = item_out item up in
    match item.holds with
    | Unnested -> Ok (of_text (quoted item.alias), next)
    | Document (Values kind) -> Ok (of_text (Kind.read kind (quoted item.alias)), next)
    | Table ->
        Error
          (Not_a_value
             (Printf.sprintf "an item of %s is a row of a table: name its column" item.row))
    | Document (Objects _) ->
        Error
          (Not_a_value
             (Printf.sprintf "an item of %s is an object of a document: name its member"
                item.row))

  (* [left = right] as what a document with a GIN index contains, and the document: none
     if it is not an equality of a member of such a document, of a kind with one
     spelling, with a constant that is not null. The document is a column, of the
     candidate's row or of the row of an item of a table; an item of a document's array
     is no column, and no index has its rows. *)
  let contained compiler left right item =
    match (left, right) with
    | Ast.Field path, Ast.Value value | Ast.Value value, Ast.Field path ->
        let names = Path.names path in
        let* entered =
          match (Path.root path, compiler.schema) with
          | Global, Some schema ->
              let* row = qualified (Schema.row schema) in
              document_at compiler (Schema.table schema) row names
          | Global, None -> Ok None
          | Item up, _ -> (
              let* item = item_out item up in
              match item.holds with
              | Document _ -> Ok None
              | Table | Unnested ->
                  document_at compiler item.row (quoted item.alias) names)
        in
        Ok
          (Option.bind entered (fun (entered : Entered.t) ->
               Option.bind (Gin.serving entered.gins entered.document entered.names)
                 (fun gin ->
                   Option.map
                     (fun pattern -> (gin, pattern))
                     (Containment.equal entered.layout entered.names value))))
    | _ -> Ok None

  (* [left = right] written as containment, where it is one. *)
  let containment compiler left right item next =
    let* contained = contained compiler left right item in
    match contained with
    | None -> Ok None
    | Some ((gin : Gin.t), pattern) -> (
        let* containing =
          Containment.containing pattern ~document:gin.document ~at:gin.at
            ~param_count:next.param_count
        in
        match containing with
        | None -> Ok None
        | Some (text, values, param_count) ->
            Ok
              (Some
                 ( { text; values; precedence = Pg_precedence.other },
                   { next with param_count } )))

  (* The array of objects or of values at [source] in a document. None: [source] is not
     in a document.

     A path is in a document if it starts in an item of a document's array, or at a
     column the schema declares a document, of the enclosing item's row or of the
     candidate's. *)
  let document_collection compiler source enclosing =
    let names = Path.names source in
    let* entered =
      match enclosing with
      | Some item -> (
          match item.holds with
          | Document (Objects layout) ->
              Ok
                (Some
                   Entered.
                     {
                       document = quoted item.alias;
                       of_ = item.row;
                       layout;
                       names;
                       gins = [];
                     })
          | Document (Values _) -> Error (no_members item.row names)
          | Table | Unnested -> document_at compiler item.row (quoted item.alias) names)
      | None -> (
          match compiler.schema with
          | Some schema ->
              let* row = qualified (Schema.row schema) in
              document_at compiler (Schema.table schema) row names
          | None -> Ok None)
    in
    match entered with
    | None -> Ok None
    | Some { document; of_; layout; names; gins } -> (
        let gin = Gin.serving gins document names in
        let* object_, name, member = in_document document of_ layout names in
        let at = of_ ^ "." ^ String.concat "." names in
        match Layout.items member with
        | Some items ->
            let* key = key name in
            Ok
              (Some
                 Document_collection.
                   { array = object_ ^ " -> " ^ key; row = at; items; gin })
        | None ->
            Error
              (Undeclared_member
                 (Printf.sprintf "%s is not a collection of its document's layout" at)))

  (* The text of [expr], and the count after it. *)
  let rec render compiler expr item next stand =
    match expr with
    | Ast.Value value -> constant value next
    | Field path ->
        let* text, next = member compiler path item next As_value in
        Ok (of_text text, next)
    | Item up -> itself up item next
    | Prefix (op, operand) ->
        let precedence = Pg_precedence.prefix op in
        let alone = Pg_param_type.under_prefix ~param_type:P.param_type op operand in
        let* operand, next = render compiler operand item next Value in
        let operand = of_type operand alone in
        (* [NOT NOT a] reads as it should; [--a] reads as a comment. *)
        let doubled = op = Neg && operand.precedence = precedence in
        let operand = within operand precedence doubled in
        let text =
          match op with
          | Not -> "NOT " ^ operand.text
          | Neg -> "-" ^ operand.text
        in
        Ok (with_text operand text precedence, next)
    | Postfix (operand, op) ->
        let precedence = Pg_precedence.postfix op in
        let alone = Pg_param_type.under_postfix ~param_type:P.param_type operand in
        let* test = null_test compiler op operand item in
        (* A member is read for whether it is null, which an object or a collection of a
           document has as a value has. *)
        let* operand, next =
          match operand with
          | Ast.Field path ->
              let* text, next = member compiler path item next As_null_test in
              Ok (of_text text, next)
          | _ -> render compiler operand item next Value
        in
        let operand = within (of_type operand alone) precedence true in
        Ok (with_text operand (operand.text ^ " " ^ test) precedence, next)
    | Infix (left, op, right) -> infix compiler left op right item next stand
    | Any (source, predicate) -> exists compiler source predicate item next stand

  and infix compiler left op right item next stand =
    let* contained =
      match (stand, op) with
      | Truth, Operator.Comparison Eq -> containment compiler left right item next
      | _ -> Ok None
    in
    match contained with
    | Some contained -> Ok contained
    | None ->
        let precedence, associativity = Pg_precedence.infix op in
        let apart side = associativity <> side && not (Pg_precedence.regroups op) in
        let of_left, of_right =
          Pg_param_type.of_both ~param_type:P.param_type left op right
        in
        let count = Pg_param_type.is_a_count_to_cast op right in
        (* The operands of [AND] and [OR] stand where the connective does; those of any
           other operator are values. *)
        let operands =
          match op with
          | Logical _ -> stand
          | Comparison _ | Arithmetic _ | Is -> Value
        in
        let* left, next = render compiler left item next operands in
        let* right, next = render compiler right item next operands in
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

  (* [EXISTS (SELECT 1 FROM ... AS alias WHERE ...)] over an array of the parent's row,
     over the rows of a table that point at the parent, or over an array of objects in a
     document.

     The predicate stands where only its truth counts: an item is a witness if it is true
     of it. Where only the truth of [any] counts, too, a collection of a document with a
     GIN index has beside it what the document contains if it is true: the equalities of
     the predicate, which follow from it, written as containment for the index. *)
  and exists compiler source predicate item next stand =
    let* enclosing =
      match Path.root source with
      | Global -> Ok None
      | Item up -> Result.map Option.some (item_out item up)
    in
    let* collection = document_collection compiler source enclosing in
    match collection with
    | Some collection ->
        exists_in_document compiler collection source predicate item next stand
    | None -> exists_in_rows compiler source enclosing predicate item next

  (* [EXISTS] over an array of a document, and beside it, where only its truth counts and
     the document has a GIN index, what the document contains if it is true. The array is
     read if it is one: what is no array - a JSON null, an object, a scalar, a member that
     is not there - is no rows, whatever the plan evaluates first (ADR-0033). *)
  and exists_in_document
      compiler
      (collection : Document_collection.t)
      source
      predicate
      item
      next
      stand =
    let* contained =
      match (stand, collection.gin) with
      | Truth, Some gin -> (
          match
            Containment.of_array gin.names
              (Containment.elements predicate collection.items)
          with
          | Some pattern ->
              Containment.containing pattern ~document:gin.document ~at:gin.at
                ~param_count:next.param_count
          | None -> Ok None)
      | _ -> Ok None
    in
    let next =
      match contained with
      | Some (_, _, param_count) -> { next with param_count }
      | None -> next
    in
    let number = next.alias_count + 1 in
    let* singular = singular_of (Path.name source) in
    let inner =
      {
        alias = Printf.sprintf "%s_%d" singular number;
        row = collection.row;
        holds = Document collection.items;
        outer = item;
      }
    in
    let next = { next with alias_count = number } in
    let* predicate, next = render compiler predicate (Some inner) next Truth in
    (* Values are read as text, and the text as their kind. *)
    let elements =
      match collection.items with
      | Objects _ -> "jsonb_array_elements"
      | Values _ -> "jsonb_array_elements_text"
    in
    let text =
      Printf.sprintf "EXISTS (SELECT 1 FROM %s(%s) AS %s WHERE %s)" elements
        (if_array collection.array) (quoted inner.alias) predicate.text
    in
    let exists = with_text predicate text Pg_precedence.atom in
    match contained with
    | None -> Ok (exists, next)
    | Some (contains, values, _) ->
        let and_, _ = Pg_precedence.infix (Logical And) in
        Ok
          ( {
              text = contains ^ " AND " ^ exists.text;
              values = values @ exists.values;
              precedence = and_;
            },
            next )

  (* [EXISTS] over an array of the parent's row, or over the rows of a table that point
     at the parent. *)
  and exists_in_rows compiler source enclosing predicate item next =
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
      match of_ with
      | Some of_ -> key_of_collection compiler of_ name
      | None -> Ok None
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
        holds =
          (match key with
          | Some _ -> Table
          | None -> Unnested);
        outer = item;
      }
    in
    let next = { next with alias_count = number } in
    let* predicate, next = render compiler predicate (Some inner) next Truth in
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
      render compiler specification None { param_count = offset; alias_count = 0 } Truth
    in
    Ok { sql = fragment.text; params = fragment.values }
end

include Make (Pg_param_type.Of_value)
