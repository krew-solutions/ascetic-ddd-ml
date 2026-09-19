module GCM = Mirage_crypto.AES.GCM

type t = { key : GCM.key; aad : string }

let name = "AES-256-GCM"
let key_size = 32
let nonce_size = 12
let tag_size = GCM.tag_size

(* What GCM seals under one nonce: 2^39 - 256 bits (NIST SP 800-38D, 5.2.1.1). *)
let max_plaintext = (1 lsl 36) - 32

let make key ~aad =
  if Key.length key <> key_size then
    Error
      (Kms_error.Malformed
         (Printf.sprintf "a key for %s is %d bytes, this one is %d" name key_size
            (Key.length key)))
  else Ok { key = GCM.of_secret (Key.to_string key); aad }

let random size =
  match Mirage_crypto_rng_unix.getrandom size with
  | bytes -> Ok bytes
  | exception Unix.Unix_error (code, _, _) ->
      Error (Kms_error.Entropy (Unix.error_message code))

let random_key () = Result.map Key.of_string (random key_size)

let seal t ~nonce plaintext =
  if String.length nonce <> nonce_size then
    Error
      (Kms_error.Malformed
         (Printf.sprintf "a nonce for %s is %d bytes, this one is %d" name nonce_size
            (String.length nonce)))
  else if String.length plaintext > max_plaintext then
    Error (Kms_error.Malformed "the plaintext is longer than AES-GCM seals")
  else Ok (nonce ^ GCM.authenticate_encrypt ~key:t.key ~nonce ~adata:t.aad plaintext)

let encrypt t plaintext =
  Result.bind (random nonce_size) (fun nonce -> seal t ~nonce plaintext)

let decrypt t sealed =
  let length = String.length sealed in
  if length < nonce_size + tag_size then
    Error
      (Kms_error.Malformed
         (Printf.sprintf "%d bytes cannot be sealed: a nonce and a tag alone are %d"
            length (nonce_size + tag_size)))
  else
    let nonce = String.sub sealed 0 nonce_size in
    let rest = String.sub sealed nonce_size (length - nonce_size) in
    match GCM.authenticate_decrypt ~key:t.key ~nonce ~adata:t.aad rest with
    | Some plaintext -> Ok plaintext
    | None -> Error Kms_error.Decrypt

let cipher t : Cipher.t =
  { encrypt = encrypt t; decrypt = decrypt t; generate_key = random_key }

module Known_answer = struct
  let seal = seal
end
