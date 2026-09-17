(** How messages are shared between slots.

    A row's slot is the hash of its partition key, a SQL expression over the inbox row,
    computed once at insert and stored with the row; the choice is made when the table is
    created, and the database does the hashing. *)

type t = { sql_expression : string  (** [hashtext(<expression>) % slots] is the slot. *) }

(** By URI: messages of one URI go to one slot. For messages whose order comes from the
    broker's topic and partition. *)
let by_uri = { sql_expression = "uri" }

(** By stream: messages of one [(tenant, stream_type, stream_id)] go to one slot. For
    messages whose order is the stream's own, the usual case for aggregate events, and the
    right one when causal dependencies stay within a stream. *)
let by_stream =
  { sql_expression = "tenant_id || ':' || stream_type || ':' || stream_id::text" }
