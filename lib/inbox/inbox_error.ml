(** What can go wrong in the inbox, as a value the caller can act on. A subscriber's
    failure is not an error but an {!Outcome.t}: the inbox records it in the row. *)

type t =
  | Session of Ascetic_session.Session_error.t
      (** The session could not be opened or closed: no connection, or a scope boundary
          the driver refused. *)
  | Database of Ascetic_session.Driver_error.t
      (** The database refused a statement, or the connection failed under it. The reason
          says whether the failure is of the moment. *)
  | Malformed of string
      (** The table is not what the inbox expects: a value that cannot be read back, a row
          that is gone, a cut other than the configured one. A defect, not a retry. *)

(** Whether the error is of the moment, a lock cycle the server broke, a connection lost,
    a server going down, so that a loop meeting it waits and goes on, rather than a defect
    to stop on (ADR-0009). *)
let is_transient = function
  | Session error -> Ascetic_session.Session_error.is_transient error
  | Database reason -> reason.transient
  | Malformed _ -> false

let pp ppf = function
  | Session error ->
      Format.fprintf ppf "session: %a" Ascetic_session.Session_error.pp error
  | Database reason ->
      Format.fprintf ppf "database: %a" Ascetic_session.Driver_error.pp reason
  | Malformed what -> Format.fprintf ppf "malformed inbox: %s" what

let to_string error = Format.asprintf "%a" pp error
