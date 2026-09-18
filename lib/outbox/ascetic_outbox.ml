(** Transactional Outbox on PostgreSQL.

    A business operation that changes state {i and} tells the outside world about it
    cannot do both atomically over two systems: a crash between the commit and the publish
    loses the message. The outbox writes the message into a table in the same transaction
    as the state change, and a separate dispatcher reads what was committed and sends it
    on. Both happen, or neither.

    The application layer sees {!Outbox_port.S}, one operation; dispatching is on the
    adapter, {!Pg_outbox}, over the session of [ascetic_ddd.session.caqti]. See
    [README.md] for what the tables and statements guarantee. *)

module Outbox_message = Outbox_message
module Position = Position
module Outbox_error = Outbox_error
module Outbox_port = Outbox_port
module Outbox_observer = Outbox_observer
module Loops = Loops
module Selection = Selection
module Pg_outbox = Pg_outbox
module Outbox_channel = Outbox_channel
module Log = Log
