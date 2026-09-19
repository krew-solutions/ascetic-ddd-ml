type t = { key_version : Key_version.t; sealed : string }

let make ~key_version ~sealed = { key_version; sealed }
let key_version t = t.key_version
let sealed t = t.sealed

let parse bytes =
  Result.map
    (fun (key_version, sealed) -> { key_version; sealed })
    (Key_version.read ~what:"a wrapped key" bytes)

let to_bytes t = Key_version.stamp t.key_version t.sealed

let wrap version (cipher : Cipher.t) key =
  Result.map
    (fun sealed -> { key_version = version; sealed })
    (cipher.encrypt (Key.to_string key))

let unwrap t version (cipher : Cipher.t) =
  if not (Key_version.equal t.key_version version) then
    Error
      (Kms_error.Wrong_key_version
         {
           expected = Key_version.to_int version;
           found = Key_version.to_int t.key_version;
         })
  else Result.map Key.of_string (cipher.decrypt t.sealed)

let equal a b =
  Key_version.equal a.key_version b.key_version && String.equal a.sealed b.sealed

let pp ppf t =
  Format.fprintf ppf "Wrapped_key(version %a, %d bytes sealed)" Key_version.pp
    t.key_version (String.length t.sealed)
