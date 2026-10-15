type t = string

let of_string bytes = bytes
let to_string t = t

let random size =
  match Mirage_crypto_rng_unix.getrandom size with
  | bytes -> Ok bytes
  | exception Unix.Unix_error (code, _, _) ->
      Error (Kms_error.Entropy (Unix.error_message code))

let length = String.length
let equal a b = Eqaf.equal a b
let pp ppf t = Format.fprintf ppf "Key(%d bytes)" (String.length t)
