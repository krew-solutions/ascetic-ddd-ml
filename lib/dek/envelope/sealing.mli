(** The sealing of a bus's channels: one envelope stage per channel, each over a KMS
    instance that serves the channel as its derivation context, behind a cache of
    unwrapped keys of its own (ADR-0024).

    A producer takes the stage of the channel it writes on its way out, a consumer the
    stage of the channel it reads on its way in; a data key drawn for a channel unwraps
    there and nowhere else, and a policy at the KMS may pin the context a caller passes.
    The stages are built up front, from the channels a service's contracts name, the same
    text at both ends, so that a channel nobody declared is not sealed under a context
    nobody admits: {!Make.of_channel} answers [None] for it, and a composition root refuses
    to start rather than seal under a typo.

    The KMS behind a stage is the caller's to name: Vault Transit on the bus's mount under
    the service's own identity as the service runs, the PostgreSQL adapter in a component
    test; both serving the channel as the context, both making no key, which is custody's.
    The stage's parameters are the deployment's: how many messages and how long one data
    key serves at the sealing end, how long an unwrapped key is kept at the opening end.

    An inbox that receives several channels has one subscriber for every row, and
    [through] takes one stage: {!Make.by_destination} gives that stage, which reads the
    row's [destination], the intake's name for the channel it received on, and delegates
    to the stage of the channel the row was sealed under (ADR-0025). *)

type parameters = {
  reuse : Reuse.t;
      (** How many messages and how long one data key serves at the sealing end before a
          fresh one is drawn. *)
  cache : float;
      (** How long an unwrapped data key is kept at the opening end, in seconds. *)
}
(** The stage's parameters: deployment configuration, bounded from the security side; the
    window after an off-boarding in which a key already drawn or unwrapped still serves is
    the reuse's lifetime plus the cache's. *)

val default_parameters : parameters
(** A thousand messages or a minute, five minutes: the initial norm. *)

type unsealed_channel = { destination : string; channel : string }
(** A channel the sealing was not built for, named by a destination. *)

val pp_unsealed_channel : Format.formatter -> unsealed_channel -> unit

(** The stage of an inbox that receives several channels: one of the sealing's stages per
    destination, chosen per message by its [destination] header, the key left aside. A
    message without the header, or with a destination the stage was not built for, is
    refused for good without a call to any KMS: it is not opened under a guessed context.
*)
module By_destination : sig
  type t

  val stage : t -> Ascetic_bus.Stage.t
  (** The stage as the bus takes it: what [through] is given. *)

  val destinations : t -> string list
  (** The destinations the stage was built for, keys left aside. *)
end

(** The sealing over a KMS, reached through sessions of a pool. *)
module Make
    (Sessions : Ascetic_session.Session_pool.S)
    (Kms : Ascetic_kms.Kms_port.S with type session = Sessions.session) : sig
  type t

  val create :
    clock:_ Eio.Time.Mono.t ->
    ?parameters:parameters ->
    Sessions.t ->
    channels:string list ->
    (string -> Kms.t) ->
    t
  (** The sealing of the channels over sessions of the pool, with the parameters, each
      channel's stage over the KMS the function makes for it, an instance serving the
      channel as its derivation context, behind a cache of its own. A channel named twice
      gets one stage. The clock times the reuse and the cache. *)

  val of_channel : t -> string -> Ascetic_bus.Stage.t option
  (** The stage of the channel, or [None] for a channel the sealing was not built for. *)

  val channels : t -> string list
  (** The channels the sealing was built for. *)

  val by_destination :
    t -> (string * string) list -> (By_destination.t, unsealed_channel) result
  (** The stage of an inbox that receives several channels: for each
      [(destination, channel)], a row whose [destination] is the first, key or no key,
      opens with the stage of the second, the channel it was sealed under at the
      producer's end. Built up front, like the sealing itself: a channel the sealing was
      not built for is refused, so that a composition root does not start with a
      destination routed to nothing. *)
end
