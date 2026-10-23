(** The scope algorithm, written once over a backend that issues the statements. The
    PostgreSQL session and the in-memory session are both instances of {!Make}; what they
    share is everything that makes a scope correct, and what differs is only where the
    statements go. *)

(** What the close of a scope that succeeded is refused with. *)
type refusal =
  | Refused of Driver_error.t
      (** For a reason of the close's own: the server refused the commit, the connection
          was lost on the way. *)
  | Aborted of Session_error.cause option
      (** The transaction was aborted by a statement that failed inside it, whose failure
          the scope absorbed: there was nothing left to commit. The cause, where the
          backend saw the failure. *)

(** What a backend issues: the six statements of a scope. Each says, when it fails, what
    the driver said and whether the failure is of the moment; none raises for a failure of
    the driver, so that a scope unwinds with errors rather than exceptions.

    [commit] and [release] close a scope that succeeded, and the server alone knows whether
    the transaction is still whole: a statement that failed inside it aborted it, and a
    scope that absorbed the failure would otherwise be committed into nothing. The backend
    asks the server where it has reason to doubt, and answers [Aborted] where the
    transaction was aborted (ADR-0034). *)
module type BACKEND = sig
  type conn

  val begin_ : conn -> (unit, Driver_error.t) result
  val commit : conn -> (unit, refusal) result
  val rollback : conn -> (unit, Driver_error.t) result
  val savepoint : conn -> string -> (unit, Driver_error.t) result
  val release : conn -> string -> (unit, refusal) result
  val rollback_to : conn -> string -> (unit, Driver_error.t) result
end

(** A session over a backend. Only what a backend's own interface needs is visible: the
    record behind [t], the guard flag and the steps of a scope are not, so nothing can
    open a scope past the guard or commit past an abandonment. *)
module Make (B : BACKEND) : sig
  type t

  include Session.S with type t := t

  val of_conn : ?observer:Session_observer.t -> B.conn -> t
  (** A session at depth 0 over the connection; no scope is open until {!atomic}. *)

  val conn : t -> B.conn
  (** The connection of the current scope, for the backend's capability. *)

  val depth : t -> int
  (** Number of scopes open around this session: 0 outside any. *)

  val is_abandoned : t -> bool
  (** Whether a rollback on this connection failed or a statement was cut short; see
      [Session_error.Abandoned]. *)
end
