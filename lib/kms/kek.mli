(** The key-encryption key: one version of a tenant's key, wrapping its data-encryption
    keys.

    A KEK carries the form the master key wrapped it in, which is what storage keeps. The
    two belong together by construction: a KEK is either made and wrapped at once,
    {!generate}, or unwrapped from the form it carries, {!load}; there is no way to pair a
    key with a wrapped form that is not its own. {!Master_key} is who wraps and unwraps.

    A KEK wraps under associated data that binds the wrap to the tenant, and, where the
    key serves one derivation context, to that context too, so a DEK wrapped for one
    channel does not unwrap as another's: the property Vault's derived keys give by
    deriving a key per context, given here by the binding of the wrap (ADR-0024). *)

type t

val generate :
  context:string option ->
  tenant_id:string ->
  version:Key_version.t ->
  algorithm:Algorithm.t ->
  wrap:(Key.t -> (Wrapped_key.t, Kms_error.t) result) ->
  (t, Kms_error.t) result
(** A fresh key of [algorithm] as version [version] of the tenant's KEK, wrapped by
    [wrap]; serving [context] when one is given. The bytes are not kept: the cipher holds
    what it needs. *)

val load :
  context:string option ->
  tenant_id:string ->
  version:Key_version.t ->
  algorithm:Algorithm.t ->
  unwrap:(Wrapped_key.t -> (Key.t, Kms_error.t) result) ->
  Wrapped_key.t ->
  (t, Kms_error.t) result
(** A KEK read back from storage: the wrapped form unwrapped by [unwrap]. *)

val tenant_id : t -> string
(** The tenant this key belongs to. *)

val context : t -> string option
(** The derivation context this key serves, if one. *)

val version : t -> Key_version.t
(** The version, from 1. *)

val algorithm : t -> Algorithm.t
(** The algorithm of the key, and so of every DEK it makes. *)

val wrapped : t -> Wrapped_key.t
(** The key as the master key wrapped it. *)

val wrap : t -> Key.t -> (Wrapped_key.t, Kms_error.t) result
(** Wraps a DEK, naming this version. *)

val unwrap : t -> Wrapped_key.t -> (Key.t, Kms_error.t) result
(** Unwraps what {!wrap} wrapped. A key wrapped by another version is [Wrong_key_version]
    before the cipher is tried. *)

val rewrap : t -> from:t -> Wrapped_key.t -> (Wrapped_key.t, Kms_error.t) result
(** Wraps again, under this version, what [from], an earlier version of the tenant's key,
    wrapped: what a DEK goes through after a rotation. *)

val generate_dek : t -> (Key.t * Wrapped_key.t, Kms_error.t) result
(** A fresh DEK of this KEK's algorithm, made by its cipher and so of its kind, in the
    clear and wrapped by this KEK. *)

val binding : tenant_id:string -> string option -> string
(** The associated data a KEK wraps under: the tenant's id alone, as it always was, or,
    for a key serving a derivation context, the tenant's id length-prefixed by four
    big-endian bytes, followed by the context, so that neither can run into the other.
    The same bytes in every port. *)

val pp : Format.formatter -> t -> unit
(** The tenant, the context, the version and the algorithm; never the key. *)
