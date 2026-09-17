(** The session over PostgreSQL through Caqti: {!Caqti_session} and the pools of
    {!Caqti_session_pool}. A repository pins [type uow = Caqti_session.t] and reaches the
    connection through {!Caqti_session.connection}. Beside them, what every PostgreSQL
    adapter over this session needs: {!Identifier}, a table name safe to splice into SQL,
    and {!Transient}, which errors of the database are of the moment. *)

module Caqti_session = Caqti_session
module Caqti_session_pool = Caqti_session_pool
module Identifier = Identifier
module Transient = Transient
