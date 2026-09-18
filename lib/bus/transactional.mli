(** Producers and consumers that are transactional by nature.

    An outbox publishes inside a transaction the caller holds; an inbox runs the handler
    inside a transaction of its own. Such a producer or consumer is obtained from its
    adapter rather than from the registry, and names the transaction at the call: the
    extra argument is the guarantee itself, named at the call site (ADR-0011). The bus
    does not know sessions: ['s] is whatever the transaction is, and the bus only passes
    one through. *)

type 's wire_producer = {
  publish : 's -> Message.t -> (unit, Bus_error.t) result;
      (** Sends one message within the session's transaction: it is committed with the
          caller's state change, or not at all. *)
}
(** A producer of wire messages that publishes inside a transaction the caller holds: the
    outbox. *)

type 's handler = 's -> Message.t -> (unit, Failure.t) result
(** A wire-level handler that is given a transaction along with the message. *)

type 's wire_consumer = {
  subscribe : 's handler -> (Subscription.t, Bus_error.t) result;
      (** Starts delivering messages, each with its transaction, to the handler. *)
}
(** A consumer of wire messages that runs the handler inside a transaction of its own: the
    inbox. The handler's writes through the session it is given commit with the
    acknowledgement of the message, or not at all. *)

(** A typed producer that publishes inside the caller's transaction. Built once, at the
    composition root, from the adapter that offers it, the outbox, and used with the
    transaction of the moment: the command handler's, or the inbox's. *)
module Producer : sig
  type ('a, 's) t

  val make : 's wire_producer -> encode:('a -> Message.t) -> ('a, 's) t
  (** A typed producer over a transactional wire producer. *)

  val through : ('a, 's) t -> Stage.t -> ('a, 's) t
  (** The same producer with one more {!Stage.t} on the way out, as {!Producer.through}.
  *)

  val publish : ('a, 's) t -> 's -> 'a -> (unit, Bus_error.t) result
  (** Sends one value within the session's transaction. A stage that fails on the way out
      fails the publish, and so the transaction. *)
end

(** A typed consumer that runs the handler inside the transport's own transaction.
    Obtained from the adapter that offers it, the inbox, as the transactional producer is
    obtained from the outbox. The handler is given the session of the transaction that
    acknowledges the message, so its writes and the acknowledgement commit together. *)
module Consumer : sig
  type ('a, 's) t

  val make : 's wire_consumer -> decode:(Message.t -> ('a, string) result) -> ('a, 's) t
  (** A typed consumer over a transactional wire consumer. *)

  val through : ('a, 's) t -> Stage.t -> ('a, 's) t
  (** The same consumer with one more {!Stage.t} on the way in, as {!Consumer.through}. *)

  val subscribe :
    ('a, 's) t ->
    ('s -> 'a -> (unit, Failure.t) result) ->
    (Subscription.t, Bus_error.t) result
  (** Runs the handler for every message, with the transaction the message is acknowledged
      in, until the subscription is cancelled.

      A message that [decode] rejects is reported and acknowledged without a handler, as
      {!Consumer.subscribe} does: a poison message must not stop the rest. A stage that
      fails on the way in fails the handling instead, so the message is kept and tried
      again, or parked at once when the stage's verdict is permanent. *)
end
