(** Keys in memory, for the tests of what consumes the signing and MAC ports: the adapter
    over them is exercised as it is in production, with the network behind the port and
    nothing else replaced.

    The keys are the model's: a {!Signing_key} per tenant, algorithm and version,
    registered from a seed so that a test's signatures are the same from run to run, or
    made from the operating system's randomness on first signing, as Vault would; a
    {!Mac_key} per tenant, purpose and version, made on first use. Built with
    [managed_keys], nothing is made on first use, as with an adapter whose keys are made
    by custody. The keys' custody is served as the adapters serve it,
    {!Signing_key_custody.S} and {!Mac_key_custody.S}: made, rotated, retired and deleted
    through the ports, with their rules (ADR-0036); a version once used is never numbered
    again. Nothing persists past the process. The KEK port is not served: the stage's and
    the custody's tests run on the PostgreSQL adapter, the simple one already
    (ADR-0024). *)

module Make (Session : sig
  type t
end) : sig
  type t

  include Signing_port.S with type t := t and type session = Session.t
  include Signing_key_custody.S with type t := t and type session := session
  include Public_key_port.S with type t := t and type session := session
  include Mac_port.S with type t := t and type session := session
  include Mac_key_custody.S with type t := t and type session := session

  val create : ?managed_keys:bool -> unit -> t
  (** No keys yet. With [managed_keys], [sign] and [mac] for a tenant without a key are
      [Signing_key_not_found] and [Mac_key_not_found], as with an adapter built so; the
      keys are made through custody, or registered. *)

  val register :
    t -> tenant_id:string -> seed:string -> (Key_version.t, Kms_error.t) result
  (** Registers the next version of the tenant's Ed25519 key, derived from the seed,
      thirty-two bytes; answers the version, one for the first. *)
end
