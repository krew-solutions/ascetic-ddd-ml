type t = { key : string option; payload : string; headers : (string * string) list }

let make payload = { key = None; payload; headers = [] }
let with_key t key = { t with key = Some key }
let with_header t name value = { t with headers = t.headers @ [ (name, value) ] }
let with_payload t payload = { t with payload }

let without_header t name =
  { t with headers = List.filter (fun (n, _) -> n <> name) t.headers }

let header t name = List.assoc_opt name t.headers
let headers t = t.headers
let key t = t.key
let payload t = t.payload
let equal (a : t) (b : t) = a = b

let pp ppf t =
  Format.fprintf ppf "{ key = %s; payload = %S; headers = [%s] }"
    (match t.key with None -> "None" | Some key -> Printf.sprintf "Some %S" key)
    t.payload
    (String.concat "; " (List.map (fun (n, v) -> Printf.sprintf "%S, %S" n v) t.headers))
