(** Sessions over Caqti connections: one fixed connection, or a Caqti pool. *)

type t

include
  Ascetic_session.Session_pool.S with type t := t and type session := Caqti_session.t

val of_connection :
  ?observer:Ascetic_session.Session_observer.t -> (module Caqti_eio.CONNECTION) -> t
(** Every session scope runs on the same connection, in turn. Fine for one fiber; Caqti
    refuses concurrent use of a connection from several fibers, so pair concurrency with
    {!of_pool}. *)

val of_pool :
  ?observer:Ascetic_session.Session_observer.t ->
  ((module Caqti_eio.CONNECTION), Caqti_error.t) Caqti_eio.Pool.t ->
  t
(** Each session scope takes a connection from the pool and returns it afterwards; an
    abandoned connection is dropped by the pool instead. *)
