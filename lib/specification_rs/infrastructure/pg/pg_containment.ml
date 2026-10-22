(* Equalities on members of a document as what the document contains:
   ["p"."document" @> {"status": "active"}], which a GIN index on the document serves,
   where [("p"."document" ->> 'status') = $1] is served by no index of the document as a
   whole.

   The two agree where only truth counts. Of a member that is null, or not there, the
   equality is null and containment false; at the root of a condition, under [AND] and
   [OR] there, and as the predicate of [any], null and false select alike. Under [NOT]
   they do not, and nothing there is written as containment. They agree for a kind with
   one spelling in JSON alone: a text, an integer, a boolean. A number with a fraction
   has several ([0.10000000000000001] is [0.1] as a [double precision] and not as JSON),
   a point in time has its offsets and its fractions, a UUID its case. And they agree for
   a constant that is not null: [= NULL] is null, and [{"status": null}] is contained in
   a document whose status is null.

   The pattern is built of functions the server folds into a constant, [jsonb_object] and
   [jsonb_set], which are immutable: [jsonb_build_object] is stable, and is called again
   for every row a sequential scan reads - three times the time of the scan, measured on
   PostgreSQL 14. The fold is a custom plan's, where the parameters are known; under a
   generic plan the pattern is evaluated for every row of a sequential scan, three times
   the time of the equality it replaced, and once for an index scan, where it costs
   nothing (ADR-0033). *)

module Layout = Pg_layout
module Kind = Pg_layout.Kind

let ( let* ) = Result.bind

(* A kind with one spelling in JSON, and the type its constant is said. *)
module Spelled = struct
  type t = Text | Int | Bool

  (* The kind, if it has one spelling in JSON. *)
  let of_kind : Kind.t -> t option = function
    | Kind.Text -> Some Text
    | Int -> Some Int
    | Bool -> Some Bool
    | Float | Timestamp | Date | Uuid -> None
end

let split_last names =
  match List.rev names with
  | [] -> None
  | name :: objects_reversed -> Some (name, List.rev objects_reversed)

(* The layout at the end of the objects [objects], if each is declared one. *)
let rec through layout = function
  | [] -> Some layout
  | object_ :: rest -> (
      match Layout.member layout object_ with
      | Some (Layout.Object inner) -> through inner rest
      | _ -> None)

module Make (P : Pg_param_type.S) = struct
  (* What a document is asked to contain: an object of members, each a constant of a
     kind, an object, or an array that contains objects or constants. *)
  type pattern = { members : (string * contained) list }

  and contained =
    | Constant of Spelled.t * P.t
    | Object of pattern
    | Array of contained list

  let none = { members = [] }
  let of_ name member = { members = [ (name, member) ] }

  (* Whether the pattern asks for nothing: every document contains it. *)
  let is_empty pattern =
    match pattern.members with
    | [] -> true
    | _ :: _ -> false

  (* [pattern] at the end of the objects [objects]. *)
  let nested objects pattern =
    List.fold_right (fun object_ inner -> of_ object_ (Object inner)) objects pattern

  (* What contains both: the members of objects at one name merged, the items of arrays
     at one name both asked for. Of two constants at one name the first is kept, as of a
     constant and an object: the pattern then asks for less, and still follows from what
     it is made of. *)
  let rec and_ mine yours =
    let members =
      List.fold_left
        (fun members (name, theirs) ->
          if List.exists (fun (ours, _) -> String.equal ours name) members then
            List.map
              (fun (ours, contained) ->
                if String.equal ours name then (ours, and_contained contained theirs)
                else (ours, contained))
              members
          else members @ [ (name, theirs) ])
        mine.members yours.members
    in
    { members }

  and and_contained mine yours =
    match (mine, yours) with
    | Object mine, Object yours -> Object (and_ mine yours)
    | Array mine, Array yours -> Array (mine @ yours)
    | mine, _ -> mine

  (* The member at [names] of an object that holds what [layout] says, equal to [value]:
     none if the member is not a value of a kind with one spelling, or [value] is
     null. *)
  let equal layout names value =
    Option.bind (split_last names) @@ fun (name, objects) ->
    Option.bind (through layout objects) @@ fun layout ->
    match Layout.member layout name with
    | Some (Layout.Value kind) ->
        Option.bind (Spelled.of_kind kind) @@ fun spelled ->
        Option.map
          (fun _ -> nested objects (of_ name (Constant (spelled, value))))
          (P.param_type value)
    | _ -> None

  (* An object that contains, at [names], an array that contains [elements]: none if
     there are none. *)
  let of_array names elements =
    match (split_last names, elements) with
    | None, _ | _, [] -> None
    | Some (name, objects), elements -> Some (nested objects (of_ name (Array elements)))

  (* What an object that holds what [layout] contains when [predicate] is true of it as
     an item: the equalities of its members with constants that [AND] joins, and those of
     its own collections' items under [any]. Anything else is left out: what the pattern
     says follows from the predicate, not the other way. *)
  let rec of_item predicate layout =
    match predicate with
    | Ast.Infix (left, Logical And, right) ->
        and_ (of_item left layout) (of_item right layout)
    | Infix (left, Comparison Eq, right) -> (
        match (left, right) with
        | (Ast.Field path, Ast.Value value | Ast.Value value, Ast.Field path)
          when Path.root path = Path.Item 0 ->
            Option.value ~default:none (equal layout (Path.names path) value)
        | _ -> none)
    | Any (source, predicate) when Path.root source = Path.Item 0 ->
        of_collection layout source predicate
    | _ -> none

  (* What an object that holds what [layout] contains when an item of its collection at
     [source] satisfies [predicate]. *)
  and of_collection layout source predicate =
    let names = Path.names source in
    match split_last names with
    | None -> none
    | Some (name, objects) -> (
        match through layout objects with
        | None -> none
        | Some layout -> (
            match Option.bind (Layout.member layout name) Layout.items with
            | None -> none
            | Some items ->
                Option.value ~default:none (of_array names (elements predicate items))))

  (* What an array whose items are [items] contains when one of them satisfies
     [predicate]: an object of what the predicate says of its members, or the constants
     it says the value is equal to. *)
  and elements predicate (items : Layout.Items.t) =
    match items with
    | Objects layout ->
        let object_ = of_item predicate layout in
        if is_empty object_ then [] else [ Object object_ ]
    | Values kind -> (
        match Spelled.of_kind kind with
        | Some spelled -> constants predicate spelled
        | None -> [])

  (* The constants that [predicate], by [AND], says the item itself is equal to. An item
     equal to two is equal to both, so the array contains both. *)
  and constants predicate spelled =
    match predicate with
    | Ast.Infix (left, Logical And, right) ->
        constants left spelled @ constants right spelled
    | Infix (left, Comparison Eq, right) -> (
        match (left, right) with
        | (Ast.Item 0, Ast.Value value | Ast.Value value, Ast.Item 0)
          when Option.is_some (P.param_type value) -> [ Constant (spelled, value) ]
        | _ -> [])
    | _ -> []

  (* [value] as the next parameter. *)
  let parameter value params param_count =
    if P.nul_in_text value then Error Pg_error.Nul_in_text
    else
      let param = param_count + 1 in
      Ok (param, params @ [ value ], param)

  (* The object: [jsonb_object] of a text member, [jsonb_set] of [{}] with any other, and
     [||] between them, parenthesised where there are several - [||] and [@>] bind
     alike, from the left. *)
  let rec object_ pattern params param_count =
    let several =
      match pattern.members with
      | _ :: _ :: _ -> true
      | _ -> false
    in
    let* entries, params, param_count =
      List.fold_left
        (fun acc (name, member) ->
          let* entries, params, param_count = acc in
          let* key = Pg_identifier.key name in
          let* entry, params, param_count =
            match member with
            | Constant (Spelled.Text, value) ->
                let* param, params, param_count = parameter value params param_count in
                Ok
                  ( Printf.sprintf "jsonb_object(ARRAY[%s], ARRAY[$%d::text])" key param,
                    params,
                    param_count )
            | member ->
                let* value, params, param_count = json member params param_count in
                Ok
                  ( Printf.sprintf "jsonb_set('{}'::jsonb, ARRAY[%s], %s)" key value,
                    params,
                    param_count )
          in
          Ok (entries @ [ entry ], params, param_count))
        (Ok ([], params, param_count))
        pattern.members
    in
    let sql = String.concat " || " entries in
    Ok ((if several then "(" ^ sql ^ ")" else sql), params, param_count)

  (* [contained] as a [jsonb] value: a constant cast to its type and read as JSON - a text
     through [jsonb_object], which writes it as a JSON string - an object, or an array,
     ['[]'::jsonb ||] each of its items. *)
  and json contained params param_count =
    match contained with
    | Constant (spelled, value) ->
        let* param, params, param_count = parameter value params param_count in
        let json =
          match spelled with
          | Spelled.Text ->
              Printf.sprintf "(jsonb_object(ARRAY['v'], ARRAY[$%d::text]) -> 'v')" param
          | Int -> Printf.sprintf "$%d::bigint::text::jsonb" param
          | Bool -> Printf.sprintf "$%d::boolean::text::jsonb" param
        in
        Ok (json, params, param_count)
    | Object pattern -> object_ pattern params param_count
    | Array items ->
        let* written, params, param_count =
          List.fold_left
            (fun acc item ->
              let* written, params, param_count = acc in
              let* item, params, param_count = json item params param_count in
              Ok (written @ [ item ], params, param_count))
            (Ok ([ "'[]'::jsonb" ], params, param_count))
            items
        in
        Ok (String.concat " || " written, params, param_count)

  (* What the pattern asks of the value at [path], where it asks nothing beside it on the
     way there. *)
  let rec at pattern path =
    match (path, pattern.members) with
    | [], _ | _, ([] | _ :: _ :: _) -> None
    | name :: rest, [ (member, contained) ] -> (
        if not (String.equal member name) then None
        else
          match (rest, contained) with
          | [], contained -> Some contained
          | _ :: _, Object inner -> at inner rest
          | _ :: _, (Constant _ | Array _) -> None)

  (* The condition that [document] contains the pattern, its constants as parameters
     numbered after [param_count]: of the document itself, or of the value at [at] in it,
     where a GIN index is - [("g"."document" -> 'subgroups') @> ...], the expression the
     index is on. None if the pattern is not a path to that value alone. *)
  let containing pattern ~document ~at:path ~param_count =
    match path with
    | [] ->
        let* sql, params, param_count = object_ pattern [] param_count in
        Ok (Some (Printf.sprintf "%s @> %s" document sql, params, param_count))
    | _ :: _ -> (
        match at pattern path with
        | None -> Ok None
        | Some contained ->
            let* json, params, param_count = json contained [] param_count in
            let* keys = Pg_identifier.traverse Pg_identifier.key path in
            (* [->], [@>] and [||] bind alike, from the left. *)
            Ok
              (Some
                 ( Printf.sprintf "(%s -> %s) @> (%s)" document
                     (String.concat " -> " keys) json,
                   params,
                   param_count )))
end
