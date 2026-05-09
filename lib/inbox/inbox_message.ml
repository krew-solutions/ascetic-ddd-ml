(** Inbox message — structure for incoming integration messages.

    See [init.sql] for the schema and the [README.md] for context on
    fields. *)

type t = {
  tenant_id : string;
      (** Tenant identifier. Use ["1"] (or any sentinel) when not multi-tenant. *)
  stream_type : string;
      (** Stream type — bounded-context.aggregate, or topic/channel name. *)
  stream_id : Yojson.Safe.t;
      (** Stream identity stored as JSONB. May be a single primitive
          ([`Int]/[`String]), a composite [`Assoc], or anything else
          serializable. *)
  stream_position : int;
      (** Monotonically increasing position within the stream. *)
  uri : string;
      (** Routing URI (e.g. ["kafka://orders"], ["amqp://exchange/key"]). *)
  payload : Yojson.Safe.t;
      (** Event payload. Should contain a ["type"] field by convention. *)
  metadata : Yojson.Safe.t option;
      (** Optional metadata. May contain ["event_id"] (used as a unique
          deduplication key) and/or ["causal_dependencies"]
          (a list of {!Causal_dependency.t} descriptors). *)
  received_position : int64 option;
      (** Position assigned at insertion (auto-filled by the database). *)
  processed_position : int64 option;
      (** Position at which the message was marked processed
          ([None] until the dispatcher acks it). *)
}

let make ?metadata ?received_position ?processed_position ~tenant_id
    ~stream_type ~stream_id ~stream_position ~uri ~payload () =
  {
    tenant_id;
    stream_type;
    stream_id;
    stream_position;
    uri;
    payload;
    metadata;
    received_position;
    processed_position;
  }

(** Causal dependencies declared in [metadata.causal_dependencies].
    Returns the empty list when metadata is absent or malformed. *)
let causal_dependencies (m : t) : Causal_dependency.t list =
  match m.metadata with
  | None -> []
  | Some (`Assoc fs) -> (
      match List.assoc_opt "causal_dependencies" fs with
      | Some (`List items) -> List.filter_map Causal_dependency.of_json items
      | _ -> [])
  | _ -> []

(** [event_id] from [metadata]; [None] if absent. *)
let event_id (m : t) : string option =
  match m.metadata with
  | Some (`Assoc fs) -> (
      match List.assoc_opt "event_id" fs with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None
