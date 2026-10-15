type t = {
  algorithm : Signature_algorithm.t;
  key_version : Key_version.t;
  bytes : string;
}

let make ~algorithm ~key_version bytes = { algorithm; key_version; bytes }
let algorithm t = t.algorithm
let key_version t = t.key_version
let to_string t = t.bytes

let equal a b =
  Signature_algorithm.equal a.algorithm b.algorithm
  && Key_version.equal a.key_version b.key_version
  && String.equal a.bytes b.bytes

let pp ppf t =
  Format.fprintf ppf "Signature(%a, version %a, %d bytes)" Signature_algorithm.pp
    t.algorithm Key_version.pp t.key_version (String.length t.bytes)
