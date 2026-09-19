type t = Aes_256_gcm

let to_string = function Aes_256_gcm -> Aes256gcm.name

let of_string name =
  if String.equal name Aes256gcm.name then Ok Aes_256_gcm
  else Error (Kms_error.Unsupported_algorithm name)

let generate_key = function Aes_256_gcm -> Aes256gcm.random_key ()

let cipher t key ~aad =
  match t with Aes_256_gcm -> Result.map Aes256gcm.cipher (Aes256gcm.make key ~aad)

let equal (a : t) (b : t) = a = b
let pp ppf t = Format.pp_print_string ppf (to_string t)
