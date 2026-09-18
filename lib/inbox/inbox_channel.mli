(** The inbox as a channel of the bus (ADR-0011).

    The inbox offers the bus two things. A {e producer}, through {!adapter} registered
    under {!scheme}: [Bus.producer bus ~uri:"inbox://orders" ...] stores each wire message
    under the identity its headers name, so a {!Ascetic_bus.Bridge} from a broker channel
    to [Fixed "inbox://orders"] is the intake. And a {e transactional consumer}, from
    {!consumer}: it runs the handler inside the transaction that marks a message
    processed, and hands it that transaction. A bus consumer of [inbox://...] is refused,
    because through it the handler's writes would fall outside that transaction.

    Headers become columns: [tenant_id], [stream_type], [stream_id], the JSON text of the
    id, or the id as a string when it is not JSON, [stream_position], and [destination]
    for [uri]: the channel the message was sent to, as the outbox stamps it; without one,
    the inbox channel it was published to, key included. Every other header is a field of
    [metadata], as text, or structured again when the text is a JSON array or object, so
    [causal_dependencies] come back as they left. The channel name after [inbox://] is not
    read yet: the whole inbox is one channel. *)

module Session = Ascetic_session_caqti.Caqti_session

val scheme : string
(** The scheme the inbox is registered under: ["inbox"]. *)

val adapter : Pg_inbox.t -> Ascetic_bus.Adapter.t
(** The inbox as a bus adapter: what {!Ascetic_bus.Bus.register} takes. Its producer is
    the intake: each message is stored in a transaction of its own, and the same identity
    again is ignored. Its consumer is refused: the inbox hands the handler its
    transaction, which a bus consumer cannot, see {!consumer}. *)

val consumer :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  ?loops:Loops.t ->
  Pg_inbox.t ->
  decode:(Ascetic_bus.Message.t -> ('a, string) result) ->
  ('a, Session.t) Ascetic_bus.Transactional.Consumer.t
(** A consumer whose handler runs inside the transaction that marks each message
    processed, reading values with [decode]. The processing loop runs as a daemon fiber on
    [sw] with [loops] until the subscription is cancelled. A handler's error is a failure
    of the moment, and the message is retried as the inbox's retries say, unless the bus
    carries the one verdict it knows: [Ascetic_bus.Failure.Permanent], from a stage or a
    handler that can tell, and the message is parked at once (ADR-0009). A defect that
    stops {!Pg_inbox.run} is reported and the loop starts again after the poll interval.
*)
