(** The MAC key: one version of a tenant's key for a purpose, making the tags a holder of
    the same key checks by recomputation.

    Thirty-two bytes, making HMAC-SHA-256 tags. The key knows its name,
    [{tenant}.mac.{purpose}], which is Transit's key name on one adapter and the
    associated data of the wrapping on the other. *)

type t

val generate :
  tenant_id:string ->
  purpose:Key_label.t ->
  version:Key_version.t ->
  (t, Kms_error.t) result
(** A fresh key for the tenant and purpose, as that version, from the operating system's
    randomness; [Entropy] when it gives none. *)

val load : tenant_id:string -> purpose:Key_label.t -> version:Key_version.t -> Key.t -> t
(** The key from its bytes, as storage kept them. *)

val tenant_id : t -> string
val purpose : t -> Key_label.t

val version : t -> Key_version.t
(** From 1. *)

val key : t -> Key.t
(** The bytes: what storage wraps and keeps. *)

val name_of : tenant_id:string -> Key_label.t -> string
(** The name of a tenant's key for a purpose, the same on every adapter:
    [{tenant}.mac.{purpose}]. *)

val name : t -> string
(** This key's name. *)

val mac : t -> string -> Mac.t
(** The HMAC-SHA-256 of the message under this key: a MAC of this version, thirty-two
    bytes. *)

val pp : Format.formatter -> t -> unit
(** The tenant, the purpose and the version; never the key. *)
