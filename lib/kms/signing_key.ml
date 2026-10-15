module Ed25519 = Mirage_crypto_ec.Ed25519
module P256 = Mirage_crypto_ec.P256.Dsa

type parsed = Ed of Ed25519.priv | P of P256.priv

type t = {
  tenant_id : string;
  algorithm : Signature_algorithm.t;
  version : Key_version.t;
  private_key : Key.t;
  parsed : parsed;
  public : Public_key.t;
}

let ( let* ) = Result.bind
let malformed what = Error (Kms_error.Malformed what)

let parse algorithm private_key =
  let bytes = Key.to_string private_key in
  match algorithm with
  | Signature_algorithm.Ed25519 -> (
      if String.length bytes <> 32 then
        malformed "an Ed25519 private key is thirty-two bytes"
      else
        match Ed25519.priv_of_octets bytes with
        | Ok key -> Ok (Ed key, Ed25519.pub_to_octets (Ed25519.pub_of_priv key))
        | Error _ -> malformed "not an Ed25519 private key")
  | Signature_algorithm.Ecdsa_p256 -> (
      match P256.priv_of_octets bytes with
      | Ok key -> Ok (P key, P256.pub_to_octets (P256.pub_of_priv key))
      | Error _ -> malformed "a P-256 private key is a scalar of the field")

let load ~tenant_id ~algorithm ~version private_key =
  let* parsed, public_bytes = parse algorithm private_key in
  let* public = Public_key.make algorithm public_bytes in
  Ok { tenant_id; algorithm; version; private_key; parsed; public }

let of_seed ~tenant_id ~algorithm ~version seed =
  load ~tenant_id ~algorithm ~version (Key.of_string seed)

let rec generate ~tenant_id ~algorithm ~version =
  let* seed = Key.random 32 in
  match load ~tenant_id ~algorithm ~version seed with
  | Ok key -> Ok key
  | Error (Kms_error.Malformed _) -> generate ~tenant_id ~algorithm ~version
  | Error _ as error -> error

let tenant_id t = t.tenant_id
let algorithm t = t.algorithm
let version t = t.version
let private_key t = t.private_key
let public t = t.public

let name_of ~tenant_id algorithm =
  Printf.sprintf "%s.sign.%s" tenant_id (Signature_algorithm.to_string algorithm)

let name t = name_of ~tenant_id:t.tenant_id t.algorithm

let sign t message =
  let bytes =
    match t.parsed with
    | Ed key -> Ed25519.sign ~key message
    | P key ->
        let digest = Digestif.SHA256.(to_raw_string (digest_string message)) in
        let r, s = P256.sign ~key digest in
        r ^ s
  in
  Signature.make ~algorithm:t.algorithm ~key_version:t.version bytes

let pp ppf t =
  Format.fprintf ppf "Signing_key(tenant `%s`, %a, version %a)" t.tenant_id
    Signature_algorithm.pp t.algorithm Key_version.pp t.version
