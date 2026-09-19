(** The port: what a repository sees.

    A resource's data-encryption keys, as ciphers, in the caller's session. The write path
    takes {!S.get_or_create}: the current version, made on first contact. The read path
    takes {!S.get_all}: every version, so that what any of them sealed opens. {!S.get} is
    for a version already known, read from a ciphertext. After the tenant's KEK rotates,
    {!S.rewrap} wraps the tenant's DEKs again under the new version; {!S.delete} forgets
    one resource for good. *)

module type S = sig
  type t
  (** A store of DEKs. *)

  type session
  (** The session of the caller's transaction; the adapter pins it. *)

  val get_or_create :
    t -> session -> Resource.t -> (Versioned_cipher.t, Dek_error.t) result
  (** The resource's current DEK, made as version 1 if the resource has none. *)

  val get :
    t ->
    session ->
    Resource.t ->
    Ascetic_kms.Key_version.t ->
    (Versioned_cipher.t, Dek_error.t) result
  (** The resource's DEK of the version; [Dek_not_found] if there is none. *)

  val get_all : t -> session -> Resource.t -> (Keyring.t, Dek_error.t) result
  (** Every version of the resource's DEK; [Dek_not_found] if there is none. *)

  val rewrap : t -> session -> tenant_id:string -> (int, Dek_error.t) result
  (** Wraps every DEK of the tenant again, under the tenant's current KEK; how many. *)

  val delete : t -> session -> Resource.t -> (unit, Dek_error.t) result
  (** Deletes every version of the resource's DEK: nothing they sealed opens again. *)
end
