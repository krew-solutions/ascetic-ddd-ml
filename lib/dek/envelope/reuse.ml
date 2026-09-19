(** How long a tenant's DEK serves at the sealing side before a fresh one is drawn: so
    many messages, or so long, whichever comes first. *)

type t = {
  messages : int;  (** Messages sealed under one DEK, at most. *)
  lifetime : float;  (** How long one DEK serves, at most, in seconds. *)
}

(** A thousand messages, or a minute: far under the four billion messages NIST SP 800-38D
    allows a key with random nonces, and a thousandth of the KMS calls. *)
let default = { messages = 1_000; lifetime = 60.0 }
