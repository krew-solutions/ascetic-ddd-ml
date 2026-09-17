(** The scope algorithm, written once over a backend that issues the statements. The
    PostgreSQL session and the in-memory session are both instances of {!Make}; what they
    share is everything that makes a scope correct, and what differs is only where the
    statements go. *)

(** What a backend issues: the six statements of a scope. Each says, when it fails, what
    the driver said and whether the failure is of the moment; none raises for a failure of
    the driver, so that a scope unwinds with errors rather than exceptions. *)
module type BACKEND = sig
  type conn

  val begin_ : conn -> (unit, Driver_error.t) result
  val commit : conn -> (unit, Driver_error.t) result
  val rollback : conn -> (unit, Driver_error.t) result
  val savepoint : conn -> string -> (unit, Driver_error.t) result
  val release : conn -> string -> (unit, Driver_error.t) result
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
