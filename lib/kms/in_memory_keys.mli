(** Keys in memory, for the tests of what consumes the signing and MAC ports: the adapter
    over them is exercised as it is in production, with the network behind the port and
    nothing else replaced.

    The keys are the model's: a {!Signing_key} per tenant and algorithm, registered from a
    seed so that a test's signatures are the same from run to run, or made from the
    operating system's randomness on first signing, as Vault would; a {!Mac_key} per
    tenant and purpose, made on first use. Nothing persists past the process. The KEK
    port is not served: the stage's and the custody's tests run on the PostgreSQL
    adapter, the simple one already (ADR-0024). *)

module Make (Session : sig
  type t
end) : sig
  type t

  include Signing_port.S with type t := t and type session = Session.t
  include Mac_port.S with type t := t and type session := session

  val create : unit -> t
  (** No keys yet. *)

  val register :
    t -> tenant_id:string -> seed:string -> (Key_version.t, Kms_error.t) result
  (** Registers the next version of the tenant's Ed25519 key, derived from the seed, thirty-two
      bytes; answers the version, one for the first. *)
end
