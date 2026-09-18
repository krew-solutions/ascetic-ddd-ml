(** A typed consumer of one [(uri, group)]. Obtained from {!Bus.consumer}. *)

type 'a t

val make :
  uri:string ->
  group:string ->
  Adapter.consumer ->
  decode:(Message.t -> ('a, string) result) ->
  'a t
(** A typed consumer over a wire consumer; what {!Bus.consumer} builds. *)

val through : 'a t -> Stage.t -> 'a t
(** The same consumer with one more {!Stage.t} on the way in: messages come back through
    the stages in the reverse of the order they are given, before they are decoded. *)

val subscribe :
  'a t -> ('a -> (unit, Failure.t) result) -> (Subscription.t, Bus_error.t) result
(** Runs the handler for every message, in order, until the subscription is cancelled.
    Subscribing again replaces the handler.

    An error of the handler means the message was not handled: a transport that can,
    redelivers it. So does a stage that fails on the way in. A message that [decode]
    rejects is reported and skipped: a poison message must not stop the rest. *)
