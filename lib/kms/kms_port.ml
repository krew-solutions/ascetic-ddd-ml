(** The port: what the application layer sees.

    Envelope encryption for one tenant at a time: data-encryption keys are wrapped and
    unwrapped by the tenant's current key-encryption key, which the service makes on first
    contact, rotates on request, and deletes for crypto-shredding. The surface of Vault
    Transit; the PostgreSQL adapter keeps the same surface.

    A wrapped key is bytes opaque to the caller, in the adapter's own form: only the
    adapter that wrapped it unwraps it. Each operation runs in the caller's session. *)

module type S = sig
  type t
  (** A key management service. *)

  type session
  (** The session of the caller. The adapter pins it: a session over PostgreSQL for the
      table of keys, one over HTTP for Vault. *)

  val encrypt_dek :
    t -> session -> tenant_id:string -> Key.t -> (string, Kms_error.t) result
  (** Wraps the DEK under the tenant's current KEK, making one if the tenant has none. *)

  val decrypt_dek :
    t -> session -> tenant_id:string -> string -> (Key.t, Kms_error.t) result
  (** Unwraps the wrapped DEK with the KEK version it names. *)

  val generate_dek :
    t -> session -> tenant_id:string -> (Key.t * string, Kms_error.t) result
  (** A fresh DEK, in the clear and wrapped under the tenant's current KEK, making one if
      the tenant has none. *)

  val rotate_kek : t -> session -> tenant_id:string -> (Key_version.t, Kms_error.t) result
  (** The next version of the tenant's KEK, or the first; returns the new version. Earlier
      versions stay, so what they wrapped still unwraps. *)

  val rewrap_dek :
    t -> session -> tenant_id:string -> string -> (string, Kms_error.t) result
  (** The wrapped DEK wrapped again under the current KEK, after a rotation. *)

  val delete_kek : t -> session -> tenant_id:string -> (unit, Kms_error.t) result
  (** Deletes every version of the tenant's KEK: nothing wrapped under them unwraps again.
      Nothing to delete is not an error. *)
end
