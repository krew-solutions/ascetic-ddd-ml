(** Transactional Outbox pattern for reliable message publishing.

    See {!Outbox} and [init.sql] for the schema and a full description
    of the design. *)

module Outbox_message = Outbox_message
module Connection_provider = Connection_provider
module Outbox_port = Outbox_port
module Outbox = Outbox
