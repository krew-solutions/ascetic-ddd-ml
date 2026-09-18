(** Kafka transport, on [kafka-eio] (librdkafka).

    A URI [kafka://orders] names the topic [orders]. A consumer in [group] is a member of
    the Kafka consumer group of that name, so several processes in one group share the
    topic's partitions; the in-memory adapter's one-consumer-per-group rule does not apply
    here. A message's key is its Kafka key, and messages with one key stay in order; a
    producer built for [kafka://orders/order-7] keys the messages that have no key of
    their own.

    Delivery is at least once. The offset of a message is committed after its handler
    returns, so a crash mid-handler redelivers the message on restart. A handler that
    fails is retried until it succeeds: the partition waits, which is what keeps its
    order. A handler that raises is reported and its message is skipped, as is one the
    consumer cannot decode, and one whose failure is {!Ascetic_bus.Failure.Permanent}: a
    poison message must not stop the partition.

    A consumer and the broker's producer are not domain-safe: share them between fibers of
    one Eio domain only, as [kafka-eio] requires. *)

type t

val create :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?security:Kafka.Security.t ->
  ?properties:(string * string) list ->
  ?offset_reset:Kafka.Consumer.offset_reset ->
  ?send_timeout:float ->
  ?retry_after:float ->
  brokers:string list ->
  unit ->
  t
(** A broker at [brokers], [host:port] each. Consumers, the producer and their fibers live
    on [sw]. [security] is plaintext by default; [properties] are raw librdkafka settings
    applied to every consumer and to the producer, so TLS details and tuning are the
    caller's. [offset_reset] says where a group with no committed offset starts, the
    driver's default, the latest message, unless given. A producer gives up on a send
    after [send_timeout] seconds, thirty by default; a consumer waits [retry_after]
    seconds, one by default, before it retries a handler that failed. *)

val adapter : t -> Ascetic_bus.Adapter.t
(** The broker as a transport of the bus: pass it to {!Ascetic_bus.Bus.register}, usually
    under the scheme ["kafka"]. *)
