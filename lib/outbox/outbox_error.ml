(** What can go wrong in the outbox, as a value the caller can act on.

    The subscriber's own error type is the parameter ['e], so a supervisor matching
    [Subscriber e] holds the subscriber's value, not its rendering. Operations without a
    subscriber never produce [Subscriber]; their error type stays polymorphic in ['e], so
    it unifies with the caller's without conversion (ADR-0002). *)

type 'e t =
  | Session of Ascetic_session.Session_error.t
      (** The session could not be opened or closed: no connection, or a scope boundary
          the driver refused. *)
  | Database of Ascetic_session.Driver_error.t
      (** The database refused a statement, or the connection failed under it. The reason
          says whether the failure is of the moment. *)
  | Subscriber of 'e
      (** The subscriber declined a message with its own error. The batch was rolled back
          and is delivered again by the next dispatch. *)
  | Malformed of string
      (** A stored value could not be read back, such as a transaction id that is not a
          number, or the table is cut into another number of slots than this outbox asks
          for. A defect, not a retry. *)

(** Whether the error is of the moment, a lock cycle the server broke, a connection lost,
    a server going down, so that a loop meeting it waits and goes on, rather than a defect
    to stop on (ADR-0009). A subscriber's error is neither: the loop treats it on its own.
*)
let is_transient = function
  | Session error -> Ascetic_session.Session_error.is_transient error
  | Database reason -> reason.transient
  | Subscriber _ | Malformed _ -> false

(** The same error with the subscriber's error mapped. *)
let map f = function
  | Session error -> Session error
  | Database reason -> Database reason
  | Subscriber error -> Subscriber (f error)
  | Malformed what -> Malformed what

let pp pp_subscriber ppf = function
  | Session error ->
      Format.fprintf ppf "session: %a" Ascetic_session.Session_error.pp error
  | Database reason ->
      Format.fprintf ppf "database: %a" Ascetic_session.Driver_error.pp reason
  | Subscriber error -> Format.fprintf ppf "subscriber: %a" pp_subscriber error
  | Malformed what -> Format.fprintf ppf "malformed value in the outbox: %s" what

let to_string subscriber_to_string error =
  Format.asprintf "%a"
    (pp (fun ppf e -> Format.pp_print_string ppf (subscriber_to_string e)))
    error
