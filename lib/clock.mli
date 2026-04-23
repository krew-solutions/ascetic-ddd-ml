(** Clock abstraction for time-dependent domain logic.

    Decouples domain from system time, enabling deterministic testing
    and reproducible scenarios via [FixedClock]. *)

(** Abstract clock interface. *)
module type S = sig
  val now : unit -> Ptime.t
  (** Returns the current time. *)
end

(** System clock using real time. *)
module SystemClock : S

(** Fixed clock for testing. Returns the time set at creation. *)
module FixedClock : sig
  include S

  val set : Ptime.t -> unit
  (** [set t] changes the time returned by [now]. *)

  val advance : Ptime.span -> unit
  (** [advance span] moves the clock forward by [span]. *)

  val create : Ptime.t -> (module S)
  (** [create t] returns a fresh [S] module pinned to time [t].
      This is the preferred way to create test clocks — each test
      gets its own independent clock instance. *)
end
