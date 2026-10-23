(** The port: what the application layer sees.

    Envelope encryption for one tenant at a time: data-encryption keys are wrapped and
    unwrapped by the tenant's current key-encryption key, which the service makes on first
    contact unless its keys are managed elsewhere. The use of the KEK only; making,
    rotating, retiring and deleting it is its custody, {!Key_custody.S} (ADR-0036). The
    surface of Vault Transit; the PostgreSQL adapter keeps the same surface.

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

  val rewrap_dek :
    t -> session -> tenant_id:string -> string -> (string, Kms_error.t) result
  (** The wrapped DEK wrapped again under the current KEK, after a rotation. *)
end
