(** Aggregate root module type.

    Defines the contract for aggregate roots in the domain layer.
    Supports both state-based and event-sourced aggregates.

    For event-sourced aggregates, the state is reconstructed by
    folding [apply_event] over the event stream starting from
    an initial state (or snapshot). *)

(** Module type for event-sourced aggregate roots. *)
module type EVENT_SOURCED = sig
  type t
  (** The aggregate state. *)

  type id
  (** The aggregate's identity type. *)

  type event
  (** The domain event payload type for this aggregate. *)

  val id : t -> id
  (** Extract the aggregate's identity. *)

  val version : t -> int
  (** The current version (number of events applied). *)

  val uncommitted_events : t -> event Domain_event.t list
  (** Events produced by the current operation, not yet persisted. *)

  val clear_uncommitted_events : t -> t
  (** Returns the aggregate with an empty uncommitted events list.
      Called after events are persisted. *)

  val apply_event : t -> event -> t
  (** [apply_event state event] produces the new state by applying
      a single event. Used for reconstitution from event stream.
      Must be a pure function with no side effects. *)
end

(** Module type for state-based aggregate roots (with event outbox). *)
module type STATE_BASED = sig
  type t
  (** The aggregate state. *)

  type id
  (** The aggregate's identity type. *)

  type event
  (** The domain event payload type for this aggregate. *)

  val id : t -> id
  val version : t -> int
  val uncommitted_events : t -> event Domain_event.t list
  val clear_uncommitted_events : t -> t
end
