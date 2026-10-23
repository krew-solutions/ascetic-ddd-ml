(** HashiCorp Vault Transit as the key management service: one adapter serving every
    port, the KEKs, the signing keys, their public halves and the MAC keys, and the
    custody of each kind.

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
    got and for a version a key has not got or no longer serves, and refuses a signature
    at a named version with a [500] for a version above the latest, retired, or below the
    key's [min_encryption_version] (measured, Vault 1.21); so a refusal of signing, of a
    MAC or of verifying is read by one read of the key, on that path only and never by
    Vault's text, whether [400], [403] or [500] (ADR-0035): the key missing is its
    absence, a [403] with the key there [Forbidden], a version the key does not sign
    with, or, for a verification, does not serve, [No_key_of_version], an unexplained
    [400] [Malformed]. The public keys are [GET keys/{name}]'s [data.keys.{n}.public_key]
    of the versions served: base64 of the raw bytes for Ed25519, a PEM
    SubjectPublicKeyInfo for P-256, read by its fixed DER prefix. A [403] on a KEK
    operation is read the same way: the key there, it is [Forbidden], the policy does not
    allow the operation or the context is not one it admits, and no retry mends it; the
    key not there, [Kek_not_found]; the read itself refused, the [403] as it came, for
    the operator (ADR-0024).

    Custody follows the rules of ADR-0036: a making is [POST keys/{name}], which Transit
    answers for a key that exists with its description and without touching it, held to
    what was asked, another type or another [derived] being [Conflict]; a rotation is
    [POST keys/{name}/rotate], which Transit refuses for a key it has not got, so a
    rotation never makes the first version; a retirement reads the key once and writes
    only a higher [min_decryption_version], raising a signing floor of the key's own with
    it, since Transit lowers the floor when asked; a deletion of nothing is nothing; a
    custody operation refused under the identity is [Forbidden] naming it.

    Built with a [derivation_context], the service serves a derived key, Transit's
    [derived]: every KEK operation carries the context, base64, and a KEK it makes is made
    [derived]; what was wrapped under one context does not unwrap under another, and a
    policy may pin the context a caller passes. Built with [managed_keys], it makes no key
    of any kind on first use, which Transit's [encrypt], [sign] and [hmac] would under a
    token that may create or update keys: a tenant without one is [Kek_not_found] on
    every wrap, [Signing_key_not_found] on [sign], [Mac_key_not_found] on [mac]; the key
    is made by whoever has custody. *)

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
  include Ascetic_kms.Signing_key_custody.S with type t := t and type session := session
  include Ascetic_kms.Public_key_port.S with type t := t and type session := session
  include Ascetic_kms.Mac_port.S with type t := t and type session := session
  include Ascetic_kms.Mac_key_custody.S with type t := t and type session := session

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
      built in the composition root from the channel's name; [managed_keys] makes no key
      on first use, for a producer or a consumer of the bus, or a holder that signs and
      MACs, under a token that may neither create nor update keys. *)
end
