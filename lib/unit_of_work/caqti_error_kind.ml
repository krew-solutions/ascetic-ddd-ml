(** What a Caqti error means to a caller that must decide what to do next.

    Caqti tags its errors by the stage that failed. Here they are grouped by
    what the caller can do about them: reconnect and retry, look at the
    statement, or fix a defect. The grouping is driver-neutral; the text of
    the error travels alongside it as [Caqti_error.show]. *)

type t =
  | Connection
      (** A driver could not be loaded or a connection could not be
          established: retry once the database is reachable. *)
  | Request
      (** The database refused or failed a statement, or the response could
          not be retrieved: permanent for a bad statement, transient for a
          deadlock or a connection lost mid-statement. Look before retrying. *)
  | Malformed
      (** A value could not be encoded for, or decoded from, the database: a
          defect in the caller's types or in the stored data, not a retry. *)

let of_error : Caqti_error.t -> t = function
  | `Load_rejected _ | `Load_failed _ | `Connect_rejected _ | `Connect_failed _
  | `Post_connect _ ->
      Connection
  | `Encode_rejected _ | `Encode_failed _ | `Decode_rejected _ -> Malformed
  | `Request_failed _ | `Response_failed _ | `Response_rejected _ -> Request
