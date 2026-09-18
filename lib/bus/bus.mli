(** A registry of transports by URI scheme.

    A call site writing [Bus.consumer bus ~uri:"in-memory://orders" ~group ~decode] does
    not name the transport: the scheme picks the adapter registered for it. Replacing the
    transport is one line at the composition root, the {!register} call, and nothing else
    moves.

    The registry is a value: {!register} gives a new bus and leaves the old one as it was.
    It is built once at the composition root, then shared. There is no global bus: tests
    run on buses of their own. *)

type t

val empty : t
(** A bus with no transports. *)

val register : t -> scheme:string -> Adapter.t -> (t, Bus_error.t) result
(** The same bus with [scheme] bound to the adapter: every [scheme://...] URI resolves to
    it. A scheme already bound is [Bus_error.Already_registered]. *)

val consumer :
  t ->
  uri:string ->
  group:string ->
  decode:(Message.t -> ('a, string) result) ->
  ('a Consumer.t, Bus_error.t) result
(** A consumer of [uri] in [group], reading values with [decode]. A message that [decode]
    rejects is reported and skipped. *)

val producer :
  t -> uri:string -> encode:('a -> Message.t) -> ('a Producer.t, Bus_error.t) result
(** A producer to [uri], writing values with [encode]. *)
