(** AES-256-GCM over one key and one associated data: the adapter of [mirage-crypto] to
    {!Cipher.t}.

    The sealed form is [nonce || ciphertext || tag], the layout the Python, Go and Rust
    ports write. Everything the algorithm knows, the sizes of its key, nonce and tag and
    how to draw a key, lives here. Randomness, for nonces and for keys, is read from the
    operating system at each call, [getrandom(2)] on Linux: there is no generator to seed
    and none to forget to seed. *)

type t

val name : string
(** The name storage records: [AES-256-GCM]. *)

val key_size : int
(** The length of a key, in bytes: 32. *)

val nonce_size : int
(** The length of a nonce, in bytes: 12, the 96 bits GCM is specified for. The library
    would take a nonce of any length; this adapter makes and accepts no other. *)

val tag_size : int
(** The length of an authentication tag, in bytes: 16, the library's. *)

val make : Key.t -> aad:string -> (t, Kms_error.t) result
(** The cipher over the key, binding what it seals to [aad]. Refuses a key of the wrong
    length with [Malformed]: the library would take a key of 16 or 24 bytes and quietly be
    AES-128 or AES-192. *)

val random_key : unit -> (Key.t, Kms_error.t) result
(** A fresh key for this algorithm, from the operating system. *)

val encrypt : t -> string -> (string, Kms_error.t) result
(** Seals under a fresh nonce; [Entropy] when the operating system gives none. *)

val decrypt : t -> string -> (string, Kms_error.t) result
(** Opens what {!encrypt} sealed. Bytes too short to carry a nonce and a tag are
    [Malformed]; anything else that does not open is [Decrypt]. *)

val cipher : t -> Cipher.t
(** The adapter as the port. *)

(** The half of {!encrypt} that takes the nonce, for checking against what another port
    sealed and for nothing else: a nonce used twice under one key gives away the
    difference of the two plaintexts and the key that authenticates them. *)
module Known_answer : sig
  val seal : t -> nonce:string -> string -> (string, Kms_error.t) result
  (** [nonce || ciphertext || tag] under the given nonce; a nonce of another length than
      {!nonce_size} is [Malformed]. *)
end
