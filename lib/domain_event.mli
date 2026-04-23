(** Domain event envelope.

    Every domain event is wrapped in an envelope carrying metadata:
    unique ID, timestamp, aggregate identity, and causality version. *)

(** The envelope wrapping any domain event payload. *)
type 'a t = {
  event_id : Uuidm.t;
  occurred_at : Ptime.t;
  aggregate_id : string;
  aggregate_version : int;
  payload : 'a;
}

val create :
  aggregate_id:string ->
  aggregate_version:int ->
  occurred_at:Ptime.t ->
  'a ->
  'a t
(** [create ~aggregate_id ~aggregate_version ~occurred_at payload]
    creates a new event envelope with a fresh UUID. *)

val map_payload : ('a -> 'b) -> 'a t -> 'b t
(** Transform the payload while preserving envelope metadata. *)
