(** In-memory transport: topics in a process-local registry, no network.

    The monolithic counterpart of a broker adapter: same surface, same group semantics.
    Each broker is an isolated world: two brokers do not share topics or subscribers, so
    tests run in parallel on brokers of their own.

    One consumer per [(uri, group)] is an invariant: in a monolith each logical role is
    one instance, and a second consumer in the same group is a configuration bug. A broker
    over a network has no such rule: there a group is several instances sharing the work.

    Delivery is a fiber per topic: it takes messages off a bounded queue and hands each,
    in order, to the handler of every group. A handler that fails or raises is reported
    and the next message is delivered; a handler that is slow holds the topic, which is
    the back-pressure the queue exists for.

    Cancelling a subscription detaches its handler and waits for it if it is running
    (ADR-0016); the topic goes on for the other groups. A call is admitted under the lock
    a handler is detached under, so a handler is not called once its cancel has returned.
*)

type t

val default_capacity : int
(** Messages a topic queues before producers wait: 1024. *)

val create : sw:Eio.Switch.t -> ?capacity:int -> unit -> t
(** A broker whose topics queue [capacity] messages before producers wait. The delivery
    fibers run on [sw] as daemons: they end with the switch. *)

val adapter : t -> Ascetic_bus.Adapter.t
(** The broker as a transport of the bus: pass it to {!Ascetic_bus.Bus.register}, usually
    under the scheme ["in-memory"]. A URI's key does not make a topic of its own:
    [in-memory://orders/order-7] publishes to the channel [in-memory://orders], and a
    message without a key gets the URI's. *)
