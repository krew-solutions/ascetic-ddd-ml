(** The scope algorithm, written once over a backend that issues the statements. The
    PostgreSQL session and the in-memory session are both instances of {!Make}; what they
    share is everything that makes a scope correct, and what differs is only where the
    statements go. *)

(** What a backend issues: the six statements of a scope, and how to give a connection up.
    Each returns the reason as text when it fails. *)
module type BACKEND = sig
  type conn

  val begin_ : conn -> (unit, string) result
  val commit : conn -> (unit, string) result
  val rollback : conn -> (unit, string) result
  val savepoint : conn -> string -> (unit, string) result
  val release : conn -> string -> (unit, string) result
  val rollback_to : conn -> string -> (unit, string) result

  val discard : conn -> unit
  (** The connection may not be used again: its transaction is in an unknown state. A
      pooled connection is disconnected so that the pool drops it. *)
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
