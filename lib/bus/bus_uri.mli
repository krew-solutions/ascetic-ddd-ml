(** The shape of a URI on the bus: [scheme://channel[/key]].

    The scheme picks the adapter, the channel is the topic or queue, and what follows the
    channel is a key: messages with one key stay in order where the transport partitions.
    [kafka://orders/order-7] is topic [orders], key [order-7]; [in-memory://orders] is the
    same channel with no key. *)

val scheme : string -> (string, Bus_error.t) result
(** The scheme: what comes before the first [:]. A URI without one is
    [Bus_error.Unknown_scheme]. *)

val channel : string -> (string, Bus_error.t) result
(** The channel: what follows [://], up to the first [/]. *)

val key : string -> string option
(** The key: what follows the channel, if anything. *)

val without_key : string -> string
(** The URI without its key: the channel a consumer subscribes to. *)
