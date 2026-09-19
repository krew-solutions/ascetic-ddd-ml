module Cipher = Ascetic_kms.Cipher
module Key_version = Ascetic_kms.Key_version
module Error = Ascetic_kms.Kms_error

type t = { version : Key_version.t; cipher : Cipher.t }

let ( let* ) = Result.bind
let make version cipher = { version; cipher }
let version t = t.version
let unversioned t = t.cipher

let encrypt t plaintext =
  Result.map (Key_version.stamp t.version) (t.cipher.encrypt plaintext)

let decrypt t sealed =
  let* version, sealed = Key_version.read ~what:"versioned" sealed in
  if not (Key_version.equal version t.version) then
    Error
      (Error.Wrong_key_version
         { expected = Key_version.to_int t.version; found = Key_version.to_int version })
  else t.cipher.decrypt sealed

let generate_key t = t.cipher.generate_key ()

let cipher t : Cipher.t =
  { encrypt = encrypt t; decrypt = decrypt t; generate_key = (fun () -> generate_key t) }

let pp ppf t = Format.fprintf ppf "Versioned_cipher(version %a)" Key_version.pp t.version
