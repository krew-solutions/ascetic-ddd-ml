(** Sessions over Caqti connections: one fixed connection, or a Caqti pool. *)

type t

include Ascetic_session.Session_pool.S with type t := t and type session = Caqti_session.t

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
    abandoned connection is dropped by the pool instead.

    The pool must outlive every fiber that takes a session from it. Caqti drains a pool
    when the switch it was connected on ends, and waits for every connection to come back;
    a daemon fiber cancelled by that same switch with a connection in hand cannot give it
    back any more, and the switch never ends. So a loop that runs as a daemon, a
    dispatcher of the outbox, the processing of the inbox, goes on a switch inside the
    pool's (ADR-0015):

    {[
    Eio.Switch.run @@ fun pool_sw ->
    let pool = connect_pool ~sw:pool_sw uri in
    Eio.Switch.run @@ fun loops_sw -> run_the_loops ~sw:loops_sw pool
    ]} *)
