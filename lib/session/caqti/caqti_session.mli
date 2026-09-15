(** A session over one Caqti connection to PostgreSQL.

    The outermost scope is [BEGIN] / [COMMIT] / [ROLLBACK] through the driver; nested
    scopes are [SAVEPOINT spN] / [RELEASE SAVEPOINT spN] / [ROLLBACK TO SAVEPOINT spN],
    named from a counter shared by the session tree. A Caqti connection serves one fiber
    at a time, so work inside a scope is sequential.

    Only {!connection} and the accessors below are more than the port shows, and they are
    reachable only where [type uow = Caqti_session.t] is written, which is the
    infrastructure layer. *)

type t

include Ascetic_session.Session.S with type t := t

val create :
  ?observer:Ascetic_session.Session_observer.t -> (module Caqti_eio.CONNECTION) -> t
(** A session at depth 0 over the connection; no transaction is open until {!atomic}. *)

val connection : t -> (module Caqti_eio.CONNECTION)
(** The capability a repository asks for: the connection of the current scope. Statements
    run through it take part in the scope's transaction. *)

val depth : t -> int
(** Number of transaction scopes open around this session: 0 outside any. *)

val is_abandoned : t -> bool
(** Whether a rollback on this connection failed; see
    [Ascetic_session.Session_error.Abandoned]. *)
