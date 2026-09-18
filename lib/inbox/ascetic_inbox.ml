(** Transactional Inbox on PostgreSQL.

    A message from outside may arrive twice, and may arrive before one it depends on. The
    inbox stores every message under its identity, so a second arrival is the same row;
    processes each one in a transaction that also marks it processed, so the subscriber's
    writes and the mark commit together; holds a message back until the messages it names
    as causal dependencies have been processed; and, when the subscriber fails, records
    the attempt, lets the message wait for its backoff without letting newer ones pass it,
    and parks it after the attempts allowed (ADR-0004).

    The edge sees {!Inbox_port.S}, one operation; processing is on the adapter,
    {!Pg_inbox}, over the session of [ascetic_ddd.session.caqti]. See [README.md] for what
    the table and statements guarantee. *)

module Inbox_message = Inbox_message
module Causal_dependency = Causal_dependency
module Partition_key = Partition_key
module Snapshot = Snapshot
module Failure = Failure
module Inbox_error = Inbox_error
module Inbox_port = Inbox_port
module Outcome = Outcome
module Retries = Retries
module Loops = Loops
module Inbox_observer = Inbox_observer
module Pg_inbox = Pg_inbox
module Inbox_channel = Inbox_channel
module Log = Log
