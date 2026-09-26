type root = Global | Item of int [@@deriving show { with_path = false }, eq, ord]

type t = { root : root; objects : string list; name : string }
[@@deriving show { with_path = false }, eq, ord]

let make root name = { root; objects = []; name }
let global name = make Global name
let item name = make (Item 0) name
let outer up name = make (Item up) name

let child { root; objects; name = object_ } name =
  { root; objects = objects @ [ object_ ]; name }

let sibling path name = { path with name }

let dotted root names =
  match String.split_on_char '.' names with
  | [] -> make root ""
  | first :: rest -> List.fold_left child (make root first) rest

let of_string names = dotted Global names
let root path = path.root
let objects path = path.objects
let name path = path.name
let names path = path.objects @ [ path.name ]
