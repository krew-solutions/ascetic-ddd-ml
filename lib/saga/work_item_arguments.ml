(** Input arguments for an activity.

    Stores key/value pairs (with stable insertion order) representing the
    parameters needed by an activity to perform its work — a vehicle type,
    a room type, a destination, and so on. The values are JSON-shaped so
    that the routing slip can travel over a message bus. *)

type value = Yojson.Safe.t
type t = (string * value) list

let empty : t = []

let of_list (items : (string * value) list) : t = items

let to_list (args : t) : (string * value) list = args

let find (args : t) (key : string) : value option =
  List.assoc_opt key args

let find_exn (args : t) (key : string) : value =
  match find args key with
  | Some v -> v
  | None ->
    invalid_arg (Printf.sprintf "Work_item_arguments.find_exn: missing key %S" key)

let add (args : t) (key : string) (v : value) : t =
  args @ [ (key, v) ]
