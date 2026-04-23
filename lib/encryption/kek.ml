(** Key Encryption Key (KEK) — one per community.

    Used to encrypt/decrypt DEKs. Stored in external KMS (Vault).
    This module defines the abstraction; infrastructure provides
    the actual key loading. *)

type t = { key_bytes : string } [@@deriving eq]

let pp fmt _ = Format.fprintf fmt "<KEK>"
let show _ = "<KEK>"

let of_bytes bytes =
  if String.length bytes = 32 then Some { key_bytes = bytes }
  else None

let to_bytes t = t.key_bytes

(** Module type for KEK provider (Vault, env vars, etc.) *)
module type PROVIDER = sig
  val get_kek : community_id:string -> (t, string) result
  val rotate_kek : community_id:string -> (t, string) result
end
