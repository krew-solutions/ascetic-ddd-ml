(** A cache of unwrapped DEKs in front of a key management service.

    Unwrapping is a function of the tenant and the wrapped bytes, so its result may be
    kept: a consumer that opens a thousand messages sealed under one DEK, or a store that
    loads every version of a resource's keys, asks the service once instead of a thousand
    times, which matters when the service is a network away, Vault. Everything else passes
    through, and deleting a tenant's KEK forgets what was cached for the tenant here;
    elsewhere the cache's time to live is the bound, so a shredded tenant's keys open for
    that long at most. That, and keys held in memory for that long, is the price; the
    capacity bounds the memory. A refusal is not kept. *)

module Make (K : Kms_port.S) : sig
  type t

  include Kms_port.S with type t := t and type session = K.session

  val default_capacity : int
  (** A thousand keys. *)

  val default_ttl : float
  (** Five minutes, in seconds. *)

  val create : ?capacity:int -> ?ttl:float -> clock:_ Eio.Time.Mono.t -> K.t -> t
  (** A cache in front of the service, holding at most [capacity] keys, the oldest going
      when one more comes, each kept [ttl] seconds after it was unwrapped; a capacity or a
      time to live of zero keeps nothing. *)

  val inner : t -> K.t
  (** The service behind the cache. *)

  val length : t -> int
  (** How many keys are held now, expired or not. *)

  val is_empty : t -> bool
  (** Whether nothing is held. *)
end
