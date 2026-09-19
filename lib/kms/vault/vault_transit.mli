(** HashiCorp Vault Transit as the key management service.

    Vault keeps the KEKs and does the cryptography; nothing but a DEK in the clear crosses
    into this process. A wrapped DEK is Vault's own ciphertext text, [vault:v1:...],
    carried as bytes and opaque here. Transit's key name is the tenant's id,
    percent-encoded. The HTTP client is the session's, and every call goes through
    [Rest_session.request] so that the session's observer times it; the client speaks to
    Vault through a {!Vault_transport.S}. *)

val key_name : string -> string
(** The Transit key of a tenant: the id as a path segment, everything but the unreserved
    characters, letters, digits, [-], [_], [.] and [~], percent-encoded byte by byte. *)

module Make (Transport : Vault_transport.S) : sig
  type t

  include
    Ascetic_kms.Kms_port.S
      with type t := t
       and type session = Transport.client Ascetic_session_rest.Rest_session.t

  val create :
    ?mount:string -> ?key_type:string -> addr:string -> token:string -> unit -> t
  (** A service over the Vault at [addr] with [token], on the [transit] mount and making
      keys of type [aes256-gcm96] unless told otherwise. A trailing slash in the address
      is dropped. *)
end
