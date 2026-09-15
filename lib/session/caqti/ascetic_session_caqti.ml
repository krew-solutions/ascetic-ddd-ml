(** The session over PostgreSQL through Caqti: {!Caqti_session} and the pools of
    {!Caqti_session_pool}. A repository pins [type uow = Caqti_session.t] and reaches the
    connection through {!Caqti_session.connection}. *)

module Caqti_session = Caqti_session
module Caqti_session_pool = Caqti_session_pool
