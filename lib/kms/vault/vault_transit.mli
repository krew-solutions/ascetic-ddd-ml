(** HashiCorp Vault Transit as the key management service: one adapter serving the four
    ports, the KEKs, their custody, the signing keys and the MAC keys.

    Vault keeps the keys and does the cryptography; nothing but a DEK in the clear crosses
    into this process. A wrapped DEK is Vault's own ciphertext text, [vault:v1:...],
    carried as bytes and opaque here. The HTTP client is the session's, and every call
    goes through [Rest_session.request] so that the session's observer times it; the
    client speaks to Vault through a {!Vault_transport.S}.

    One mount holds a tenant's keys of every kind, told apart by name: the KEK is the
    tenant's id, percent-encoded, what the Python and Go ports name; a signing key is
    [{tenant}.sign.{algorithm}], Transit types [ed25519] and [ecdsa-p256]; a MAC key
    [{tenant}.mac.{purpose}], type [hmac]. The dot is the separator Transit allows that
    the parts cannot contain: for the dotted names the tenant's id is checked to be
    letters, digits, [_] and [-], not encoded, since a [/] is "unsupported path" to
    Transit even encoded (ADR-0023).

    Signatures and MACs are Vault's [vault:v{n}:...] texts parsed once into their version
    and bytes; signatures are asked for and verified in JWS marshaling, so an ECDSA
    signature is [r || s] and never DER. Transit answers [400] both for a key it has not
    got and for a version a key has not got or no longer serves; the adapter tells them
    apart by reading the key once, on that path only, rather than by Vault's text. A
    [403] on a KEK operation is read the same way: the key there, it is [Forbidden], the
    policy does not allow the operation or the context is not one it admits, and no retry
    mends it; the key not there, [Kek_not_found]; the read itself refused, the [403] as it
    came, for the operator (ADR-0024).

    Built with a [derivation_context], the service serves a derived key, Transit's
    [derived]: every KEK operation carries the context, base64, and a KEK it makes is made
    [derived]; what was wrapped under one context does not unwrap under another, and a
    policy may pin the context a caller passes. Built with [managed_keys], it makes no KEK
    on first contact, which Transit's [encrypt] would under a token that may create or
    update keys, and a tenant without one is [Kek_not_found] on every wrap; the key is
    made by whoever has custody. *)

val key_name : string -> string
(** The Transit key of a tenant: the id as a path segment, everything but the unreserved
    characters, letters, digits, [-], [_], [.] and [~], percent-encoded byte by byte. *)

val signing_key_name :
  tenant_id:string ->
  Ascetic_kms.Signature_algorithm.t ->
  (string, Ascetic_kms.Kms_error.t) result
(** The Transit key a tenant signs with, per algorithm: the model's name for it, once the
    tenant's id is known to fit a dotted key name; [Malformed] otherwise. *)

val mac_key_name :
  tenant_id:string -> Ascetic_kms.Key_label.t -> (string, Ascetic_kms.Kms_error.t) result
(** The Transit key a tenant makes MACs with, per purpose, likewise. *)

module Make (Transport : Vault_transport.S) : sig
  type t

  include
    Ascetic_kms.Kms_port.S
      with type t := t
       and type session = Transport.client Ascetic_session_rest.Rest_session.t

  include Ascetic_kms.Key_custody.S with type t := t and type session := session
  include Ascetic_kms.Signing_port.S with type t := t and type session := session
  include Ascetic_kms.Mac_port.S with type t := t and type session := session

  val create :
    ?mount:string ->
    ?key_type:string ->
    ?derivation_context:string ->
    ?managed_keys:bool ->
    addr:string ->
    token:string ->
    unit ->
    t
  (** A service over the Vault at [addr] with [token], on the [transit] mount and making
      KEKs of type [aes256-gcm96] unless told otherwise. A trailing slash in the address
      is dropped. [derivation_context] makes this the instance of one channel of the bus,
      built in the composition root from the channel's name; [managed_keys] makes no KEK
      on first contact, for a producer or a consumer of the bus under a token that may
      neither create nor update keys. *)
end
