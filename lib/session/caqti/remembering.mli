(** A connection that remembers the first failure the server reported through it, so that
    a scope which absorbed the failure can be refused with the failure named, and the
    server asked before a commit only where a statement failed (ADR-0034).

    The session hands a repository this connection and not the driver's: every statement
    of a scope goes through it, so what it does not remember did not fail. Statements of
    the session's own, [BEGIN], [COMMIT] and the savepoints, go past it, so the memory is
    of the scope's statements alone. *)

type t
(** What the connection remembers: the first failure the server reported since the
    memory was last cleared, if any. *)

val create : unit -> t

val remembered : t -> Ascetic_session.Session_error.cause option
(** The failure remembered, if any; the memory keeps it. *)

val take : t -> Ascetic_session.Session_error.cause option
(** The failure remembered, forgotten as it is taken. *)

val forget : t -> unit
(** Forgets what aborted a transaction that has since ended or been restored. *)

val wrap : t -> (module Caqti_eio.CONNECTION) -> (module Caqti_eio.CONNECTION)
(** The connection, every failure of a statement through it remembered: the first one
    the server reported, by its SQLSTATE and message where the driver gives them. A
    failure of the driver's own, a value it could not decode, is not a failure of the
    server and is not remembered. *)
