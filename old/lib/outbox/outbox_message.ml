(** Outbox message — structure for outgoing integration messages.

    See {!Ascetic_outbox.Outbox} and [init.sql] for context. *)

type t = {
  uri : string;
      (** Routing address (e.g. ["kafka://orders"], ["amqp://exchange/key"]). *)
  payload : string;
      (** The message as it goes on the wire: serialized, and encrypted
          where the deployment requires it, before it reaches the outbox.
          The outbox stores and relays these bytes and never inspects
          them. *)
  metadata : Yojson.Safe.t;
      (** Message metadata. Must contain ["message_id"] (a UUID) for
          idempotency; may carry ["correlation_id"], ["causation_id"] and
          the like. *)
  created_at : Ptime.t option;
      (** Timestamp the message was inserted (assigned by the database). *)
  position : int64 option;
      (** Position within the outbox (BIGSERIAL, assigned by the database). *)
  transaction_id : string option;
      (** PostgreSQL [xid8] transaction identifier as decimal text.

          Stored as text rather than [int64] because [xid8] is unsigned 64-bit
          and may not fit into a signed [int64]. *)
}

let make ?created_at ?position ?transaction_id ~uri ~payload ~metadata () =
  { uri; payload; metadata; created_at; position; transaction_id }
