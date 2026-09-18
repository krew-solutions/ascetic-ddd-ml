(** The contract a transport implements.

    An adapter deals in wire messages only; the typed layer, {!Consumer} and {!Producer},
    encodes and decodes around it. A transport is a record of functions, so the registry
    holds transports of any kind side by side and picks one by URI scheme at run time. *)

type handler = Message.t -> (unit, Failure.t) result
(** A wire-level handler: what a consumer runs for each message. An error means the
    message was not handled: a transport that can, redelivers it. *)

type consumer = {
  subscribe : handler -> (Subscription.t, Bus_error.t) result;
      (** Starts delivering messages to the handler, replacing the previous handler of
          this consumer if there was one. *)
}
(** A consumer of wire messages. *)

type producer = {
  publish : Message.t -> (unit, Bus_error.t) result;  (** Sends one message. *)
}
(** A producer of wire messages. *)

type t = {
  consumer : uri:string -> group:string -> (consumer, Bus_error.t) result;
      (** A consumer of [uri] in [group]. *)
  producer : uri:string -> (producer, Bus_error.t) result;  (** A producer to [uri]. *)
}
(** A transport, bound to a URI scheme by {!Bus.register}. *)
