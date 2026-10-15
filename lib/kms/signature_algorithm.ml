type t = Ed25519 | Ecdsa_p256

let all = [ Ed25519; Ecdsa_p256 ]
let to_string = function
  | Ed25519 -> "ed25519"
  | Ecdsa_p256 -> "ecdsa-p256"

let of_string = function
  | "ed25519" -> Some Ed25519
  | "ecdsa-p256" -> Some Ecdsa_p256
  | _ -> None

let equal (a : t) (b : t) = a = b
let pp ppf t = Format.pp_print_string ppf (to_string t)
