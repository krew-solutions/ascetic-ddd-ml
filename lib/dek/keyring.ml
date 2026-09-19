module Cipher = Ascetic_kms.Cipher
module Key_version = Ascetic_kms.Key_version
module Error = Ascetic_kms.Kms_error
module Versions = Map.Make (Key_version)

type t = { newest : Versioned_cipher.t; older : Cipher.t Versions.t }

let ( let* ) = Result.bind

let make version cipher =
  { newest = Versioned_cipher.make version cipher; older = Versions.empty }

let add t version cipher =
  let current = Versioned_cipher.version t.newest in
  match Key_version.compare version current with
  | 0 -> { t with newest = Versioned_cipher.make version cipher }
  | order when order > 0 ->
      {
        newest = Versioned_cipher.make version cipher;
        older = Versions.add current (Versioned_cipher.unversioned t.newest) t.older;
      }
  | _ -> { t with older = Versions.add version cipher t.older }

let newest t = Versioned_cipher.version t.newest

let versions t =
  List.map fst (Versions.bindings t.older) @ [ Versioned_cipher.version t.newest ]

let encrypt t plaintext = Versioned_cipher.encrypt t.newest plaintext

let decrypt t sealed =
  let* version, rest = Key_version.read ~what:"versioned" sealed in
  if Key_version.equal version (Versioned_cipher.version t.newest) then
    (Versioned_cipher.unversioned t.newest).decrypt rest
  else
    match Versions.find_opt version t.older with
    | Some cipher -> cipher.decrypt rest
    | None -> Error (Error.No_key_of_version (Key_version.to_int version))

let generate_key t = Versioned_cipher.generate_key t.newest

let cipher t : Cipher.t =
  { encrypt = encrypt t; decrypt = decrypt t; generate_key = (fun () -> generate_key t) }

let pp ppf t =
  Format.fprintf ppf "Keyring(versions %a)"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
       Key_version.pp)
    (versions t)
