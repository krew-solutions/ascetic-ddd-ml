(** Every version of a resource's DEK: seals under the newest, opens whatever version a
    ciphertext names. What the read path uses.

    Never empty: made with one version, and given more. The newest is held apart from the
    rest, so sealing needs no lookup that could miss. *)

module Cipher = Ascetic_kms.Cipher
module Key_version = Ascetic_kms.Key_version

type t

val make : Key_version.t -> Cipher.t -> t
(** A keyring of one version. *)

val add : t -> Key_version.t -> Cipher.t -> t
(** The same keyring with one more version. A newer version becomes the one sealing is
    done under; a version already there is replaced. *)

val newest : t -> Key_version.t
(** The version sealing is done under. *)

val versions : t -> Key_version.t list
(** Every version, oldest first. *)

val encrypt : t -> string -> (string, Ascetic_kms.Kms_error.t) result
(** Seals under the newest version, which the sealed bytes name. *)

val decrypt : t -> string -> (string, Ascetic_kms.Kms_error.t) result
(** Opens with the version the ciphertext names; [No_key_of_version] when the keyring has
    none. *)

val generate_key : t -> (Ascetic_kms.Key.t, Ascetic_kms.Kms_error.t) result
(** A fresh key of the newest cipher's kind. *)

val cipher : t -> Cipher.t
(** The keyring as a cipher, for a codec that takes one. *)

val pp : Format.formatter -> t -> unit
(** The versions; never a key. *)
