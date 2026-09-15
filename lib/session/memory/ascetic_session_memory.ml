(** The in-memory session, for testing a use case without a database: {!Memory_session}
    records the statements of its scopes into a journal, {!Memory_session_pool} hands such
    sessions out. *)

module Memory_session = Memory_session
module Memory_session_pool = Memory_session_pool
