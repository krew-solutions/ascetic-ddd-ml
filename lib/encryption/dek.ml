(** Data Encryption Key (DEK) — one per aggregate instance. *)

module GCM = Mirage_crypto.AES.GCM

type t = { key_bytes : string; nonce : string } [@@deriving eq]

let pp fmt _ = Format.fprintf fmt "<DEK>"
let show _ = "<DEK>"

let generate () =
  let key_bytes = Mirage_crypto_rng.generate 32 in
  let nonce = Mirage_crypto_rng.generate 12 in
  { key_bytes; nonce }

let key_bytes t = t.key_bytes
let nonce t = t.nonce

let encrypt_with_kek (kek : Kek.t) (dek : t) : string =
  let key = GCM.of_secret (Kek.to_bytes kek) in
  let encrypted = GCM.authenticate_encrypt ~key ~nonce:dek.nonce dek.key_bytes in
  dek.nonce ^ encrypted

let decrypt_with_kek (kek : Kek.t) (encrypted : string) : t option =
  if String.length encrypted < 12 + 16 then None
  else
    let nonce_s = String.sub encrypted 0 12 in
    let ct_with_tag = String.sub encrypted 12 (String.length encrypted - 12) in
    let key = GCM.of_secret (Kek.to_bytes kek) in
    match GCM.authenticate_decrypt ~key ~nonce:nonce_s ct_with_tag with
    | Some plaintext -> Some { key_bytes = plaintext; nonce = nonce_s }
    | None -> None
