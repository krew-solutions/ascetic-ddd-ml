(** Partition-key strategy: how to compute a SQL expression that, fed
    through [hashtext()], distributes inbox messages across workers.

    The dispatcher injects this expression as
    [hashtext(<expression>) %% num_workers = worker_id] into its WHERE
    clause. Two strategies are provided; users can supply their own by
    implementing the [S] signature. *)

module type S = sig
  val sql_expression : string
  (** A Postgres SQL expression (referring to inbox table columns) that
      yields the partition key. *)
end

(** Partition by [uri]: messages with the same URI go to the same worker.
    Use when ordering follows topic/channel partitions from the broker. *)
module Uri : S = struct
  let sql_expression = "uri"
end

(** Partition by stream identity: all messages for the same
    [(tenant_id, stream_type, stream_id)] go to the same worker. Use when
    causal ordering is a property of the stream itself (most common for
    aggregate-event streams). *)
module Stream : S = struct
  let sql_expression =
    "tenant_id || ':' || stream_type || ':' || stream_id::text"
end

type t = (module S)

let uri : t = (module Uri)
let stream : t = (module Stream)
