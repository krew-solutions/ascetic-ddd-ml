(** A cache of unwrapped DEKs in front of a key management service.

    Unwrapping is a function of the tenant and the wrapped bytes, so its result may be
    kept: a consumer that opens a thousand messages sealed under one DEK, or a store that
    loads every version of a resource's keys, asks the service once instead of a thousand
    times, which matters when the service is a network away, Vault. Everything else passes
    through, and deleting a tenant's KEK forgets what was cached for the tenant here;
    elsewhere the cache's time to live is the bound, so a shredded tenant's keys open for
    that long at most. That, and keys held in memory for that long, is the price; the
    capacity bounds the memory. A refusal is not kept.

    The signing and MAC ports are not served here: a signature and a verification are not
    functions of the cache's key, and an adapter that serves them is called directly. *)

(** What a cache offers beside the port it serves. *)
module type S = sig
  type t

  type service
  (** The service behind the cache. *)

  include Kms_port.S with type t := t

  val default_capacity : int
  (** A thousand keys. *)

  val default_ttl : float
  (** Five minutes, in seconds. *)

  val create : ?capacity:int -> ?ttl:float -> clock:_ Eio.Time.Mono.t -> service -> t
  (** A cache in front of the service, holding at most [capacity] keys, the oldest going
      when one more comes, each kept [ttl] seconds after it was unwrapped; a capacity or a
      time to live of zero keeps nothing. *)

  val inner : t -> service
  (** The service behind the cache. *)

  val length : t -> int
  (** How many keys are held now, expired or not. *)

  val is_empty : t -> bool
  (** Whether nothing is held. *)

  val forget : t -> tenant_id:string -> unit
  (** Forgets the tenant's keys held here: what a deletion or a retirement does on its way
      through the cache, for a caller that did either behind the cache's back. *)
end

module Make (K : Kms_port.S) : S with type service = K.t and type session = K.session

(** The same cache over a service that has custody of the keys too: custody passes
    through, and retiring a tenant's versions forgets the tenant's keys held here, as a
    deletion does, where the time to live would bound it otherwise (ADR-0024). *)
module With_custody (K : sig
  include Kms_port.S
  include Key_custody.S with type t := t and type session := session
end) : sig
  include S with type service = K.t and type session = K.session
  include Key_custody.S with type t := t and type session := session
end
