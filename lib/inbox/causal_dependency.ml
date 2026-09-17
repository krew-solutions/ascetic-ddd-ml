(** A message that must be processed before another: the identity of a row in the inbox.
*)

type t = {
  tenant_id : string;  (** The tenant of the message depended on. *)
  stream_type : string;  (** Its stream type. *)
  stream_id : Yojson.Safe.t;  (** Its stream id. *)
  stream_position : int;  (** Its position in the stream. *)
}

let make ~tenant_id ~stream_type ~stream_id ~stream_position =
  { tenant_id; stream_type; stream_id; stream_position }

let equal (a : t) (b : t) = a = b

(** The identity as the JSON a waiting row names it by, and as it travels in
    [metadata.causal_dependencies]. *)
let to_json d : Yojson.Safe.t =
  `Assoc
    [
      ("tenant_id", `String d.tenant_id);
      ("stream_type", `String d.stream_type);
      ("stream_id", d.stream_id);
      ("stream_position", `Int d.stream_position);
    ]

let of_json (json : Yojson.Safe.t) =
  match json with
  | `Assoc fields -> (
      let field name = List.assoc_opt name fields in
      match
        ( field "tenant_id",
          field "stream_type",
          field "stream_id",
          field "stream_position" )
      with
      | ( Some (`String tenant_id),
          Some (`String stream_type),
          Some stream_id,
          Some (`Int p) ) ->
          Some { tenant_id; stream_type; stream_id; stream_position = p }
      | _ -> None)
  | _ -> None

(** The identity as one string, [tenant/type/id/position]: how a trace names a row. *)
let to_string d =
  Printf.sprintf "%s/%s/%s/%d" d.tenant_id d.stream_type
    (Yojson.Safe.to_string d.stream_id)
    d.stream_position

let pp ppf d = Format.pp_print_string ppf (to_string d)
