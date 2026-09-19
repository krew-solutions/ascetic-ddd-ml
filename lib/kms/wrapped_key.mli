(** A key wrapped by one version of another key.

    On the wire: the version of the wrapping key as four big-endian bytes, then the sealed
    bytes in the cipher's own layout. What the table of key-encryption keys stores as
    [encrypted_key] and what the port hands out as a wrapped data-encryption key: the
    layout of the Python, Go and Rust ports. *)

type t

val make : key_version:Key_version.t -> sealed:string -> t
(** [sealed], sealed under version [key_version] of the wrapping key. *)

val key_version : t -> Key_version.t
(** The version of the key that wrapped this one. *)

val sealed : t -> string
(** The sealed bytes, in the cipher's layout. *)

val parse : string -> (t, Kms_error.t) result
(** Reads the wire form. Bytes too short to name a version are [Malformed]; whether the
    rest unwraps is known only when a key tries. *)

val to_bytes : t -> string
(** The wire form. *)

val wrap : Key_version.t -> Cipher.t -> Key.t -> (t, Kms_error.t) result
(** Wraps the key with the cipher, which is the given version of the wrapping key. *)

val unwrap : t -> Key_version.t -> Cipher.t -> (Key.t, Kms_error.t) result
(** Unwraps with the cipher, which is the given version of the wrapping key: a wrapped key
    of another version is [Wrong_key_version] before the cipher is tried. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
