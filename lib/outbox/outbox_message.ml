(** What the outbox stores.

    The three fields a producer sets are the routing URI, the payload and the metadata;
    the rest the database assigns on insert and the dispatcher reads back. *)

type t = {
  uri : string;
      (** Where the message goes: [kafka://orders], [kafka://orders/order-123]. The part
          after the topic is a partition key: messages with one full URI are in one slot,
          and go out in order. *)
  payload : string;
      (** The message as it goes on the wire: serialized, and encrypted where the
          deployment requires it, before it reaches the outbox (ADR-0001). The outbox
          stores and relays these bytes and never inspects them. *)
  metadata : Yojson.Safe.t;
      (** About the message: must carry a [message_id] (a UUID) for idempotency, and may
          carry [correlation_id], [causation_id] and the like. *)
  created_at : Ptime.t option;  (** When the row was inserted. *)
  position : int option;  (** Order within the transaction that inserted it. *)
  transaction_id : int option;  (** The inserting transaction, [pg_current_xact_id()]. *)
}

(** A message to publish. *)
let make ~uri ~payload ~metadata =
  { uri; payload; metadata; created_at = None; position = None; transaction_id = None }

(** The message id, if the metadata carries one. *)
let message_id t =
  match t.metadata with
  | `Assoc fields -> (
      match List.assoc_opt "message_id" fields with
      | Some (`String id) -> Some id
      | _ -> None)
  | _ -> None
