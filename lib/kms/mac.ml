type t = { key_version : Key_version.t; bytes : string }

let make ~key_version bytes = { key_version; bytes }
let key_version t = t.key_version
let to_string t = t.bytes
let equal a b =
  Key_version.equal a.key_version b.key_version && Eqaf.equal a.bytes b.bytes

let pp ppf t =
  Format.fprintf ppf "Mac(version %a, %d bytes)" Key_version.pp t.key_version
    (String.length t.bytes)
