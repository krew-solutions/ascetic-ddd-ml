(** Aggregate root module types. *)

module type EVENT_SOURCED = sig
  type t
  type id
  type event

  val id : t -> id
  val version : t -> int
  val uncommitted_events : t -> event Domain_event.t list
  val clear_uncommitted_events : t -> t
  val apply_event : t -> event -> t
end

module type STATE_BASED = sig
  type t
  type id
  type event

  val id : t -> id
  val version : t -> int
  val uncommitted_events : t -> event Domain_event.t list
  val clear_uncommitted_events : t -> t
end
