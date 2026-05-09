(** Causal dependency descriptor for the inbox pattern.

    A message can declare a list of causal dependencies — earlier messages
    that must already be marked as processed before this message becomes
    eligible. The dispatcher skips messages whose dependencies are not yet
    satisfied and tries the next one in [received_position] order. *)

type t = {
  tenant_id : string;
  stream_type : string;
  stream_id : Yojson.Safe.t;
  stream_position : int;
}

let make ~tenant_id ~stream_type ~stream_id ~stream_position =
  { tenant_id; stream_type; stream_id; stream_position }

let to_json (d : t) : Yojson.Safe.t =
  `Assoc
    [
      ("tenant_id", `String d.tenant_id);
      ("stream_type", `String d.stream_type);
      ("stream_id", d.stream_id);
      ("stream_position", `Int d.stream_position);
    ]

let of_json (j : Yojson.Safe.t) : t option =
  match j with
  | `Assoc fs ->
      let get k = List.assoc_opt k fs in
      let str = function Some (`String s) -> Some s | _ -> None in
      let int = function Some (`Int n) -> Some n | _ -> None in
      let any = function Some v -> Some v | None -> None in
      (match (str (get "tenant_id"), str (get "stream_type"),
              any (get "stream_id"), int (get "stream_position")) with
       | Some tenant_id, Some stream_type, Some stream_id, Some stream_position ->
           Some { tenant_id; stream_type; stream_id; stream_position }
       | _ -> None)
  | _ -> None
