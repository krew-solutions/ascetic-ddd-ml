(** The primitive: seals bytes under a key and associated data, opens them again, and
    makes keys for ciphers of its kind.

    A cipher is a value, three functions closed over a key and the data its work is bound
    to, so that a key-encryption key, a versioned cipher and a keyring are all ciphers to
    a codec that takes one. {!Algorithm.cipher} makes the ciphers there are. *)

type t = {
  encrypt : string -> (string, Kms_error.t) result;
      (** Seals the plaintext under a fresh nonce. The result opens only with this
          cipher's key and associated data. *)
  decrypt : string -> (string, Kms_error.t) result;
      (** Opens what [encrypt] sealed; [Decrypt] for anything else. *)
  generate_key : unit -> (Key.t, Kms_error.t) result;
      (** A fresh key for a cipher of this kind, from the operating system. This cipher's
          own key plays no part; the kind does: a key made here is for this algorithm and
          no other. *)
}
