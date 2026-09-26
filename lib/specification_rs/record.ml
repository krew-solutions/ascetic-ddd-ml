type 'v t = Value of 'v | Object of (string * 'v t) list | Collection of 'v t list
[@@deriving show { with_path = false }, eq]

let value v = Value v
let object_ members = Object members
let collection items = Collection items

let member record name =
  match record with
  | Object members ->
      Option.to_result ~none:(Context.Missing name) (List.assoc_opt name members)
  (* A value and a collection have no members to miss. *)
  | Value _ | Collection _ -> Error (Context.Missing name)

let rec to_context record =
  let ( let* ) = Result.bind in
  {
    Context.field =
      (fun name ->
        let* member = member record name in
        match member with
        | Value v -> Ok v
        | Object _ | Collection _ -> Error (Context.Not_a_value name));
    object_ =
      (fun name ->
        let* member = member record name in
        match member with
        | Object _ as object_ -> Ok (to_context object_)
        | Value _ | Collection _ -> Error (Context.Not_an_object name));
    collection =
      (fun name ->
        let* member = member record name in
        match member with
        | Collection items -> Ok (List.map to_context items)
        | Value _ | Object _ -> Error (Context.Not_a_collection name));
  }
