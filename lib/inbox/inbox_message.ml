(** An incoming message.

    Its identity is [(tenant_id, stream_type, stream_id, stream_position)]: the same
    message arriving twice is the same row, which is what makes processing idempotent. The
    stream is usually an aggregate, its type, its id and its version, but may as well be a
    topic, a partition key and an offset. *)

type t = {
  tenant_id : string;  (** The tenant; a fixed value when there is only one. *)
  stream_type : string;  (** The kind of stream: [context.Aggregate], or a topic. *)
  stream_id : Yojson.Safe.t;  (** Which stream: an aggregate id, simple or composite. *)
  stream_position : int;
      (** Where in the stream: the aggregate's version, or an offset. *)
  uri : string;  (** Where the message came from: [kafka://orders/order-123]. *)
  payload : string;
      (** The message as it came off the wire: serialized, and encrypted where the
          deployment requires it (ADR-0001). The inbox stores and hands over these bytes
          and never inspects them. *)
  metadata : Yojson.Safe.t option;
      (** About the message: [message_id], and [causal_dependencies], messages that must
          be processed before this one. *)
  received_position : int option;  (** Order of arrival, assigned by the database. *)
  processed_position : int option;
      (** Order of processing, assigned when processed; [None] until then. *)
  attempts : int;  (** Failed attempts so far (ADR-0004). *)
  last_error : string option;
      (** What the subscriber returned the last time it failed. *)
  waiting_for : Causal_dependency.t option;
      (** The dependency the row is set aside to wait for, if any (ADR-0005). *)
}

(** A message to receive. *)
let make ~tenant_id ~stream_type ~stream_id ~stream_position ~uri ~payload =
  {
    tenant_id;
    stream_type;
    stream_id;
    stream_position;
    uri;
    payload;
    metadata = None;
    received_position = None;
    processed_position = None;
    attempts = 0;
    last_error = None;
    waiting_for = None;
  }

(** The same message with metadata. *)
let with_metadata t metadata = { t with metadata = Some metadata }

(** The same message, to be processed only after [dependencies]. *)
let depending_on t dependencies =
  let fields =
    match t.metadata with Some (`Assoc fields) -> fields | Some _ | None -> []
  in
  let fields = List.remove_assoc "causal_dependencies" fields in
  let dependencies = `List (List.map Causal_dependency.to_json dependencies) in
  { t with metadata = Some (`Assoc (fields @ [ ("causal_dependencies", dependencies) ])) }

(** The messages this one must wait for. Entries that are not dependencies are ignored.
    The list may arrive as its JSON text, which is how flat broker headers carry it. *)
let causal_dependencies t =
  let entries =
    match t.metadata with
    | Some (`Assoc fields) -> (
        match List.assoc_opt "causal_dependencies" fields with
        | Some (`String text) -> (
            match Yojson.Safe.from_string text with
            | `List entries -> entries
            | _ | (exception Yojson.Json_error _) -> [])
        | Some (`List entries) -> entries
        | _ -> [])
    | _ -> []
  in
  List.filter_map Causal_dependency.of_json entries

(** The message id, if the metadata carries one. *)
let message_id t =
  match t.metadata with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "message_id" fields with
      | Some (`String id) -> Some id
      | _ -> None)
  | _ -> None

(** The identity of the message, as a dependency on it. *)
let identity t =
  {
    Causal_dependency.tenant_id = t.tenant_id;
    stream_type = t.stream_type;
    stream_id = t.stream_id;
    stream_position = t.stream_position;
  }
