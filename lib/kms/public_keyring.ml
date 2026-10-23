module Versions = Map.Make (Key_version)

type t = { algorithm : Signature_algorithm.t; keys : Public_key.t Versions.t }

let malformed what = Error (Kms_error.Malformed what)

let make algorithm keys =
  let ( let* ) = Result.bind in
  let* ring =
    List.fold_left
      (fun ring (version, key) ->
        let* ring = ring in
        if Key_version.to_int version = 0 then
          malformed "a keyring's versions count from one"
        else if not (Signature_algorithm.equal (Public_key.algorithm key) algorithm) then
          malformed
            (Format.asprintf "version %a is a %a key in a %a keyring" Key_version.pp
               version Signature_algorithm.pp (Public_key.algorithm key)
               Signature_algorithm.pp algorithm)
        else if Versions.mem version ring then
          malformed
            (Format.asprintf "version %a is named twice in a keyring" Key_version.pp
               version)
        else Ok (Versions.add version key ring))
      (Ok Versions.empty) keys
  in
  if Versions.is_empty ring then
    malformed "a keyring has at least one version that verifies"
  else Ok { algorithm; keys = ring }

let algorithm t = t.algorithm
let get t version = Versions.find_opt version t.keys
let to_list t = Versions.bindings t.keys
let versions t = List.map fst (to_list t)

let equal a b =
  Signature_algorithm.equal a.algorithm b.algorithm
  && Versions.equal Public_key.equal a.keys b.keys

let pp ppf t =
  Format.fprintf ppf "Public_keyring(%a, versions %a)" Signature_algorithm.pp t.algorithm
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
       Key_version.pp)
    (versions t)
