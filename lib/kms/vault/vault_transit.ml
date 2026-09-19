module Error = Ascetic_kms.Kms_error
module Key = Ascetic_kms.Key
module Key_version = Ascetic_kms.Key_version
module Rest_session = Ascetic_session_rest.Rest_session

let ( let* ) = Result.bind

let key_name tenant_id =
  let unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> true
    | _ -> false
  in
  let out = Buffer.create (String.length tenant_id) in
  String.iter
    (fun c ->
      if unreserved c then Buffer.add_char out c
      else Buffer.add_string out (Printf.sprintf "%%%02X" (Char.code c)))
    tenant_id;
  Buffer.contents out

let without_trailing_slashes addr =
  let rec last i = if i > 0 && addr.[i - 1] = '/' then last (i - 1) else i in
  String.sub addr 0 (last (String.length addr))

(* What Vault said in [{"errors": [...]}], joined. *)
let errors_of (body : Yojson.Safe.t option) =
  match body with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "errors" fields with
      | Some (`List errors) ->
          String.concat "; "
            (List.filter_map (function `String text -> Some text | _ -> None) errors)
      | Some _ | None -> "")
  | Some _ | None -> ""

let data_field (answer : Yojson.Safe.t) field =
  match answer with
  | `Assoc fields -> (
      match List.assoc_opt "data" fields with
      | Some (`Assoc data) -> List.assoc_opt field data
      | Some _ | None -> None)
  | _ -> None

(* [data.<field>] of an answer, as text. *)
let text_field answer field =
  match data_field answer field with
  | Some (`String text) -> Ok text
  | Some _ | None ->
      Error (Error.Malformed (Printf.sprintf "vault answered without `data.%s`" field))

(* [data.<field>] of an answer, base64 as Vault writes it, decoded. *)
let base64_field answer field =
  let* text = text_field answer field in
  match Base64.decode text with
  | Ok bytes -> Ok bytes
  | Error (`Msg reason) ->
      Error
        (Error.Malformed
           (Printf.sprintf "vault's `data.%s` is not base64: %s" field reason))

(* A wrapped DEK is Vault's ciphertext text. *)
let ciphertext_text encrypted_dek =
  if String.is_valid_utf_8 encrypted_dek then Ok encrypted_dek
  else Error (Error.Malformed "a Vault ciphertext is text, `vault:v1:...`")

module Make (Transport : Vault_transport.S) = struct
  type session = Transport.client Rest_session.t
  type t = { addr : string; token : string; mount : string; key_type : string }

  let create ?(mount = "transit") ?(key_type = "aes256-gcm96") ~addr ~token () =
    { addr = without_trailing_slashes addr; token; mount; key_type }

  (* One call under the mount; [404] is [Kek_not_found], any other refusal
     [Vault], [204] an empty object. *)
  let request t session ~tenant_id ~meth ~path body =
    let url = Printf.sprintf "%s/v1/%s%s" t.addr t.mount path in
    let* (response : Vault_transport.response) =
      Rest_session.request session ~meth ~url (fun () ->
          Result.map_error
            (fun reason -> Error.Transport reason)
            (Transport.send (Rest_session.http session)
               { meth; url; token = t.token; body }))
    in
    match response.status with
    | 404 -> Error (Error.Kek_not_found { tenant_id; key_version = None })
    | 204 -> Ok (`Assoc [])
    | status when status >= 200 && status < 300 ->
        Ok (Option.value response.body ~default:(`Assoc []))
    | status ->
        Error (Error.Vault { status; meth; path; message = errors_of response.body })

  let key_exists t session ~tenant_id =
    match
      request t session ~tenant_id ~meth:"GET" ~path:("/keys/" ^ key_name tenant_id) None
    with
    | Ok _ -> Ok true
    | Error (Error.Kek_not_found _) -> Ok false
    | Error _ as error -> error

  let create_key t session ~tenant_id =
    let* _ =
      request t session ~tenant_id ~meth:"POST"
        ~path:("/keys/" ^ key_name tenant_id)
        (Some (`Assoc [ ("type", `String t.key_type) ]))
    in
    Ok ()

  (* Makes the tenant's key if there is none; Transit's [POST /keys/:name]
     leaves an existing key as it is. *)
  let ensure_key = create_key

  let encrypt_dek t session ~tenant_id dek =
    let* () = ensure_key t session ~tenant_id in
    let* answer =
      request t session ~tenant_id ~meth:"POST"
        ~path:("/encrypt/" ^ key_name tenant_id)
        (Some
           (`Assoc [ ("plaintext", `String (Base64.encode_string (Key.to_string dek))) ]))
    in
    text_field answer "ciphertext"

  let decrypt_dek t session ~tenant_id encrypted_dek =
    let* ciphertext = ciphertext_text encrypted_dek in
    let* answer =
      request t session ~tenant_id ~meth:"POST"
        ~path:("/decrypt/" ^ key_name tenant_id)
        (Some (`Assoc [ ("ciphertext", `String ciphertext) ]))
    in
    Result.map Key.of_string (base64_field answer "plaintext")

  let generate_dek t session ~tenant_id =
    let* () = ensure_key t session ~tenant_id in
    let* answer =
      request t session ~tenant_id ~meth:"POST"
        ~path:("/datakey/plaintext/" ^ key_name tenant_id)
        (Some (`Assoc [ ("bits", `Int 256) ]))
    in
    let* dek = base64_field answer "plaintext" in
    let* encrypted_dek = text_field answer "ciphertext" in
    Ok (Key.of_string dek, encrypted_dek)

  let rotate_kek t session ~tenant_id =
    let name = key_name tenant_id in
    let* exists = key_exists t session ~tenant_id in
    if not exists then
      let* () = create_key t session ~tenant_id in
      Ok Key_version.first
    else
      let* _ =
        request t session ~tenant_id ~meth:"POST"
          ~path:(Printf.sprintf "/keys/%s/rotate" name)
          (Some (`Assoc []))
      in
      let* answer =
        request t session ~tenant_id ~meth:"GET" ~path:("/keys/" ^ name) None
      in
      match data_field answer "latest_version" with
      | Some (`Int version) -> Key_version.of_int version
      | Some _ | None ->
          Error (Error.Malformed "vault answered without `data.latest_version`")

  let rewrap_dek t session ~tenant_id encrypted_dek =
    let* ciphertext = ciphertext_text encrypted_dek in
    let* answer =
      request t session ~tenant_id ~meth:"POST"
        ~path:("/rewrap/" ^ key_name tenant_id)
        (Some (`Assoc [ ("ciphertext", `String ciphertext) ]))
    in
    text_field answer "ciphertext"

  let delete_kek t session ~tenant_id =
    let* exists = key_exists t session ~tenant_id in
    if not exists then Ok ()
    else
      let name = key_name tenant_id in
      let* _ =
        request t session ~tenant_id ~meth:"POST"
          ~path:(Printf.sprintf "/keys/%s/config" name)
          (Some (`Assoc [ ("deletion_allowed", `Bool true) ]))
      in
      let* _ = request t session ~tenant_id ~meth:"DELETE" ~path:("/keys/" ^ name) None in
      Ok ()
end
