module Kind = struct
  type t = Bool | Int | Float | Text | Timestamp | Date | Uuid
  [@@deriving show { with_path = false }, eq]

  let read kind text =
    match kind with
    | Text -> Printf.sprintf "(%s COLLATE \"C\")" text
    | Bool -> text ^ "::boolean"
    | Int -> text ^ "::bigint"
    | Float -> text ^ "::double precision"
    | Timestamp -> text ^ "::timestamptz"
    | Date -> text ^ "::date"
    | Uuid -> text ^ "::uuid"
end

type t = { members : (string * member) list }
and member = Value of Kind.t | Object of t | Collection of t | Values of Kind.t
[@@deriving show { with_path = false }, eq]

module Items = struct
  type layout = t
  type t = Objects of layout | Values of Kind.t
end

let empty = { members = [] }
let declare name member layout = { members = layout.members @ [ (name, member) ] }
let value name kind layout = declare name (Value kind) layout
let object_ name inner layout = declare name (Object inner) layout
let collection name inner layout = declare name (Collection inner) layout
let values name kind layout = declare name (Values kind) layout

(* The last declaration of that name. *)
let member layout name =
  List.fold_left
    (fun found (declared, member) ->
      if String.equal declared name then Some member else found)
    None layout.members

let items = function
  | Collection layout -> Some (Items.Objects layout)
  | Values kind -> Some (Items.Values kind)
  | Value _ | Object _ -> None
