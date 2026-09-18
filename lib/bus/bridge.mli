(** A Messaging Bridge: what arrives on one channel is published on another.

    The bridge subscribes to a channel and, for each message, publishes it, bytes, key and
    headers untouched, to a target: one fixed URI, or the URI a header names, so that one
    channel may feed many. It acknowledges a message only after the target accepted it: a
    failed publish is an error of the handler, and the source keeps the message for
    another try.

    An outbox dispatcher is a bridge from the outbox channel to the destination each
    message names; an inbox intake is a bridge from a broker channel to the inbox channel.
*)

(** Where a bridge publishes. *)
type target =
  | Fixed of string  (** Every message goes to this URI. *)
  | Header of string  (** Each message goes to the URI in the header of this name. *)

type t

val create : Bus.t -> t
(** A bridge that publishes through the bus. *)

val run :
  t -> from:string -> group:string -> target -> (Subscription.t, Bus_error.t) result
(** Starts moving messages from [from], as consumer group [group], to the target. Runs
    until the subscription is cancelled. *)
