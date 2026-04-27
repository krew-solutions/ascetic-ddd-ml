(** Result values produced by an activity's work execution.

    Stores key/value pairs representing the outcome of [do_work] — a
    reservation id, a confirmation number, and so on. The values are
    JSON-shaped so the routing slip can travel over a message bus. *)

type value = Yojson.Safe.t
type t = (string * value) list

let empty : t = []

let of_list (items : (string * value) list) : t = items

let to_list (r : t) : (string * value) list = r

let find (r : t) (key : string) : value option =
  List.assoc_opt key r

let find_exn (r : t) (key : string) : value =
  match find r key with
  | Some v -> v
  | None ->
    invalid_arg (Printf.sprintf "Work_result.find_exn: missing key %S" key)

let add (r : t) (key : string) (v : value) : t =
  r @ [ (key, v) ]
