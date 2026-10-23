module Ed25519 = Mirage_crypto_ec.Ed25519
module P256 = Mirage_crypto_ec.P256.Dsa

type parsed = Ed of Ed25519.pub | P of P256.pub
type t = { algorithm : Signature_algorithm.t; bytes : string; parsed : parsed }

let malformed what = Error (Kms_error.Malformed what)

let make algorithm bytes =
  match algorithm with
  | Signature_algorithm.Ed25519 -> (
      if String.length bytes <> 32 then
        malformed "an Ed25519 public key is thirty-two bytes"
      else
        match Ed25519.pub_of_octets bytes with
        | Ok key -> Ok { algorithm; bytes; parsed = Ed key }
        | Error _ -> malformed "not an Ed25519 public key")
  | Signature_algorithm.Ecdsa_p256 -> (
      (* Whatever SEC1 form the point came in, it is held uncompressed, so
         that a key's bytes are one form per algorithm, as a signature's
         are. *)
      match P256.pub_of_octets bytes with
      | Ok key -> Ok { algorithm; bytes = P256.pub_to_octets key; parsed = P key }
      | Error _ -> malformed "not a P-256 public key")

let algorithm t = t.algorithm
let to_string t = t.bytes

(* SHA-256 of the message: what ECDSA over P-256 signs here. *)
let digest message = Digestif.SHA256.(to_raw_string (digest_string message))

let verify t message signature =
  let bytes = Signature.to_string signature in
  let valid =
    Signature_algorithm.equal (Signature.algorithm signature) t.algorithm
    && String.length bytes = 64
    &&
    match t.parsed with
    | Ed key -> Ed25519.verify ~key bytes ~msg:message
    | P key ->
        let r = String.sub bytes 0 32 and s = String.sub bytes 32 32 in
        P256.verify ~key (r, s) (digest message)
  in
  if valid then Ok () else Error Kms_error.Invalid_signature

let equal a b =
  Signature_algorithm.equal a.algorithm b.algorithm && String.equal a.bytes b.bytes

let pp ppf t =
  Format.fprintf ppf "Public_key(%a, %d bytes)" Signature_algorithm.pp t.algorithm
    (String.length t.bytes)
