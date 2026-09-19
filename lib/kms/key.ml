type t = string

let of_string bytes = bytes
let to_string t = t
let length = String.length
let equal a b = Eqaf.equal a b
let pp ppf t = Format.fprintf ppf "Key(%d bytes)" (String.length t)
