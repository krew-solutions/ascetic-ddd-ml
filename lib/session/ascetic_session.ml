(** Unit of Work as a session: an opaque handle with one operation, [atomic], that runs a
    scope inside a transaction and nests as savepoints.

    This library is the port and the algorithm, with no database behind it: {!Session.S}
    is what the application layer sees, {!Session_pool.S} is what the edge uses to obtain
    a session, and {!Scope.Make} is the scope algorithm every backend instantiates. The
    backends are sub-libraries: [ascetic_ddd.session.caqti] for PostgreSQL through Caqti,
    and [ascetic_ddd.session.memory] for a journal-recording session in tests. See
    [README.md]. *)

module Session = Session
module Session_error = Session_error
module Session_observer = Session_observer
module Session_pool = Session_pool
module Scope = Scope
