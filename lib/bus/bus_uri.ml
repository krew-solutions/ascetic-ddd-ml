(* What follows [://], if the URI has it. *)
let rest uri =
  let rec find i =
    if i + 3 > String.length uri then None
    else if String.sub uri i 3 = "://" then
      Some (i + 3, String.sub uri (i + 3) (String.length uri - i - 3))
    else find (i + 1)
  in
  find 0

let scheme uri =
  match String.index_opt uri ':' with
  | Some i -> Ok (String.sub uri 0 i)
  | None -> Error (Bus_error.Unknown_scheme uri)

let channel uri =
  match rest uri with
  | None -> Error (Bus_error.Unknown_scheme uri)
  | Some (_, rest) -> (
      match String.split_on_char '/' rest with
      | channel :: _ when channel <> "" -> Ok channel
      | _ -> Error (Bus_error.Transport (Printf.sprintf "`%s` names no channel" uri)))

let key uri =
  match rest uri with
  | None -> None
  | Some (_, rest) -> (
      match String.index_opt rest '/' with
      | None -> None
      | Some slash ->
          let key = String.sub rest (slash + 1) (String.length rest - slash - 1) in
          if key = "" then None else Some key)

let without_key uri =
  match rest uri with
  | None -> uri
  | Some (start, rest) -> (
      match String.index_opt rest '/' with
      | None -> uri
      | Some slash -> String.sub uri 0 (start + slash))
