(** Scheme-dispatched message bus.

    The bus is a registry of transports keyed by URI scheme, {!Bus}. It carries opaque
    wire messages, {!Message}: a producer encodes its values on the way out and a consumer
    decodes them on the way in, each with a function of its own, so two consumers of one
    topic may read the same bytes as different types: the wire format is the contract, the
    types are local to each side. That is what lets two bounded contexts share a topic
    without a translation layer between them.

    A message may go through {!Stage}s between the typed layer and the transport, which is
    where sealing goes. {!Bridge} moves what arrives on one channel to another.
    {!Transactional} is for the producers and consumers that are transactional by nature,
    the outbox and the inbox. A transport implements {!Adapter}; see
    [ascetic_ddd.bus.in_memory] for the process-local one. Usage is described in
    [README.md]. *)

module Message = Message
module Failure = Failure
module Bus_error = Bus_error
module Bus_uri = Bus_uri
module Subscription = Subscription
module Handling = Handling
module Stage = Stage
module Adapter = Adapter
module Consumer = Consumer
module Producer = Producer
module Transactional = Transactional
module Bus = Bus
module Bridge = Bridge
module Log = Log
