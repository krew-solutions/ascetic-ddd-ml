(** The master key: wraps a tenant's key-encryption keys.

    The one key of the system, from configuration. It makes, loads and rotates a tenant's
    KEKs, and is what wraps them; the tenant is named at each operation, and is the
    associated data of the wrapping, so a key wrapped for one tenant does not unwrap for
    another. A KEK made by the master key is of the master key's algorithm. *)

type t

val version : Key_version.t
(** The version of every master key: 1. *)

val make : Key.t -> Algorithm.t -> (t, Kms_error.t) result
(** The master key, for the algorithm. Refuses a key of the wrong length for it. *)

val algorithm : t -> Algorithm.t
(** The algorithm of the key, and so of every KEK it makes. *)

val wrap : t -> tenant_id:string -> Key.t -> (Wrapped_key.t, Kms_error.t) result
(** Wraps the key for the tenant. *)

val unwrap : t -> tenant_id:string -> Wrapped_key.t -> (Key.t, Kms_error.t) result
(** Unwraps what {!wrap} wrapped for the tenant. *)

val generate_kek : t -> tenant_id:string -> (Kek.t, Kms_error.t) result
(** The tenant's first KEK: a fresh key of this master key's algorithm, wrapped by it,
    version 1. *)

val load_kek :
  t ->
  tenant_id:string ->
  version:Key_version.t ->
  algorithm:Algorithm.t ->
  Wrapped_key.t ->
  (Kek.t, Kms_error.t) result
(** A KEK read back from storage: the wrapped form unwrapped by this master key. *)

val rotate_kek : t -> Kek.t -> (Kek.t, Kms_error.t) result
(** The next version of the KEK: a fresh key of this master key's algorithm, wrapped by
    it, one version up. *)

val pp : Format.formatter -> t -> unit
(** The algorithm; never the key. *)
