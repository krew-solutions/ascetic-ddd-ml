(** Transactional Inbox pattern: idempotent ingestion of incoming
    integration messages with causal-consistency guarantees.

    See {!Inbox} for the PostgreSQL implementation and [init.sql] /
    [README.md] for usage. *)

module Causal_dependency = Causal_dependency
module Inbox_message = Inbox_message
module Partition_strategy = Partition_strategy
module Inbox_port = Inbox_port
module Inbox = Inbox
