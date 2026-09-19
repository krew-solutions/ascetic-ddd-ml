(** One version of a resource's DEK: seals under that version, opens only that. What the
    write path uses.

    What it seals is [version || sealed]: four big-endian bytes, then the sealed bytes in
    the cipher's own layout, the layout of the other ports and of a wrapped key in the
    KMS. A codec takes it as a cipher, {!cipher}, and sees bytes; the version is read
    here, in memory, to pick the key, where the KMS reads it to fetch one from its store.
*)

module Cipher = Ascetic_kms.Cipher
module Key_version = Ascetic_kms.Key_version

type t

val make : Key_version.t -> Cipher.t -> t
(** The cipher over DEK version [version]. *)

val version : t -> Key_version.t
(** The version. *)

val encrypt : t -> string -> (string, Ascetic_kms.Kms_error.t) result
(** Seals, with the version in front. *)

val decrypt : t -> string -> (string, Ascetic_kms.Kms_error.t) result
(** Opens what this version sealed; another version's work is [Wrong_key_version] before
    the key is tried, and bytes too short to name a version are [Malformed]. *)

val generate_key : t -> (Ascetic_kms.Key.t, Ascetic_kms.Kms_error.t) result
(** A fresh key of the cipher's kind. *)

val cipher : t -> Cipher.t
(** This version as a cipher, for a codec that takes one. *)

val unversioned : t -> Cipher.t
(** The cipher underneath, which seals without the version: what a {!Keyring} is built
    from. *)

val pp : Format.formatter -> t -> unit
(** The version; never the key. *)
