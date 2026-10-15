(** The signature algorithms there are.

    A signature's wire form is fixed per algorithm, so a verifier that reads the algorithm
    knows the bytes, and two adapters never put two encodings of one algorithm side by
    side. The name is what storage records and what Transit names its key type by. *)

type t =
  | Ed25519  (** RFC 8032 Ed25519 over the message itself, no prehash: 64 bytes. *)
  | Ecdsa_p256
      (** ECDSA over NIST P-256 with SHA-256 of the message: 64 bytes, [r || s], never
          DER. *)

val all : t list
(** Every algorithm, for a test that tries each. *)

val to_string : t -> string
(** The algorithm's id, stable text: [ed25519], [ecdsa-p256]. *)

val of_string : string -> t option
(** The algorithm from its id; [None] for any other text. The caller names the error, since
    this module is below the errors. *)

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
