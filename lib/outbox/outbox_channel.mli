(** The outbox as a channel of the bus (ADR-0011).

    The outbox offers the bus two things. A {e transactional producer}, from {!producer}:
    it publishes inside the caller's transaction, and the URI it was built for is the
    destination stored with the row, [kafka://orders/order-7], key included. And a
    {e consumer}, through {!adapter} registered under {!scheme}:
    [Bus.consumer bus ~uri:"outbox://all" ~group ...] runs the dispatcher for that
    consumer group and hands every committed row to the handler as a wire message whose
    [destination] header is the row's URI. A {!Ascetic_bus.Bridge} from that consumer to
    [Header "destination"] is the dispatcher process.

    Headers become [metadata]: each header is a string field of the JSONB object, so
    [message_id] keeps its unique index, and every metadata field comes back as a header.
    The channel name after [outbox://] is not read yet: the whole outbox is one channel.
*)

module Session = Ascetic_session_caqti.Caqti_session

val scheme : string
(** The scheme the outbox is registered under: ["outbox"]. *)

val producer :
  _ Pg_outbox.t ->
  destination:string ->
  encode:('a -> Ascetic_bus.Message.t) ->
  ('a, Session.t) Ascetic_bus.Transactional.Producer.t
(** A producer to [destination] that publishes inside the caller's transaction.
    [destination] is a URI of another channel, [kafka://orders/order-7], stored with the
    row for the dispatcher; a destination without a key takes the message's. Publishes
    through {!Pg_outbox.publish}, so that the row is written in one place and the observer
    sees it. *)

val adapter :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  ?loops:Loops.t ->
  Ascetic_bus.Failure.t Pg_outbox.t ->
  Ascetic_bus.Adapter.t
(** The outbox as a bus adapter: what {!Ascetic_bus.Bus.register} takes. Its consumer is
    the dispatcher of a consumer group, run as a daemon fiber on [sw] with [loops] until
    the subscription is cancelled: a batch whose handler fails is rolled back and
    delivered again, and the position of the group moves only past messages the handler
    accepted. A defect that stops {!Pg_outbox.run} is reported and the dispatcher starts
    again after the poll interval. Its producer is refused: the outbox publishes only
    inside a transaction, see {!producer}. The outbox is typed by the bus's failure, the
    error a wire handler returns. *)
