(** A typed producer to one URI. Obtained from {!Bus.producer}. *)

type 'a t

val make : Adapter.producer -> encode:('a -> Message.t) -> 'a t
(** A typed producer over a wire producer; what {!Bus.producer} builds. *)

val through : 'a t -> Stage.t -> 'a t
(** The same producer with one more {!Stage.t} on the way out: messages go through the
    stages in the order they are given, after encoding. *)

val publish : 'a t -> 'a -> (unit, Bus_error.t) result
(** Sends one value. Waits while the transport applies back-pressure. A stage that fails
    on the way out fails the publish, with [Bus_error.Stage]. *)
