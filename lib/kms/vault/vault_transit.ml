module Error = Ascetic_kms.Kms_error
module Key = Ascetic_kms.Key
module Key_version = Ascetic_kms.Key_version
module Kek_options = Ascetic_kms.Kek_options
module Signature_algorithm = Ascetic_kms.Signature_algorithm
module Signature = Ascetic_kms.Signature
module Signing_key = Ascetic_kms.Signing_key
module Key_label = Ascetic_kms.Key_label
module Mac = Ascetic_kms.Mac
module Mac_key = Ascetic_kms.Mac_key
module Rest_session = Ascetic_session_rest.Rest_session

let ( let* ) = Result.bind

(* ------------------------------------------------------------------------ *)
(* Names                                                                      *)

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

(* The tenant's id as the first part of a dotted key name. *)
let tenant_part tenant_id =
  if Key_label.is_part tenant_id then Ok tenant_id
  else
    Error
      (Error.Malformed
         (Printf.sprintf
            "a tenant id in a key name is letters, digits, `_` and `-`, not `%s`"
            tenant_id))

let signing_key_name ~tenant_id algorithm =
  Result.map
    (fun tenant_id -> Signing_key.name_of ~tenant_id algorithm)
    (tenant_part tenant_id)

let mac_key_name ~tenant_id purpose =
  Result.map (fun tenant_id -> Mac_key.name_of ~tenant_id purpose) (tenant_part tenant_id)

let without_trailing_slashes addr =
  let rec last i = if i > 0 && addr.[i - 1] = '/' then last (i - 1) else i in
  String.sub addr 0 (last (String.length addr))

(* ------------------------------------------------------------------------ *)
(* Answers                                                                    *)

(* What Vault said in [{"errors": [...]}], joined. *)
let errors_of (body : Yojson.Safe.t option) =
  match body with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "errors" fields with
      | Some (`List errors) ->
          String.concat "; "
            (List.filter_map
               (function
                 | `String text -> Some text
                 | _ -> None)
               errors)
      | Some _ | None -> "")
  | Some _ | None -> ""

let data_field (answer : Yojson.Safe.t) field =
  match answer with
  | `Assoc fields -> (
      match List.assoc_opt "data" fields with
      | Some (`Assoc data) -> List.assoc_opt field data
      | Some _ | None -> None)
  | _ -> None

let without_field field =
  Error (Error.Malformed (Printf.sprintf "vault answered without `data.%s`" field))

(* [data.<field>] of an answer, as text. *)
let text_field answer field =
  match data_field answer field with
  | Some (`String text) -> Ok text
  | Some _ | None -> without_field field

(* [data.<field>] of an answer, base64 as Vault writes it, decoded. *)
let base64_field answer field =
  let* text = text_field answer field in
  match Base64.decode text with
  | Ok bytes -> Ok bytes
  | Error (`Msg reason) ->
      Error
        (Error.Malformed
           (Printf.sprintf "vault's `data.%s` is not base64: %s" field reason))

(* [data.<field>] of an answer as a version. *)
let version_field answer field =
  match data_field answer field with
  | Some (`Int version) -> Key_version.of_int version
  | Some _ | None -> without_field field

(* [data.<field>] of an answer as a boolean, [false] when absent. *)
let bool_field answer field =
  match data_field answer field with
  | Some (`Bool value) -> value
  | Some _ | None -> false

(* Whether the key, as Vault describes it, serves the version: made, and not
   retired by [min_decryption_version]. *)
let serves answer version =
  match version_field answer "latest_version" with
  | Error _ -> false
  | Ok latest ->
      let oldest =
        Result.value
          (version_field answer "min_decryption_version")
          ~default:Key_version.first
      in
      Key_version.compare oldest version <= 0 && Key_version.compare version latest <= 0

(* A wrapped DEK is Vault's ciphertext text. *)
let ciphertext_text encrypted_dek =
  if String.is_valid_utf_8 encrypted_dek then Ok encrypted_dek
  else Error (Error.Malformed "a Vault ciphertext is text, `vault:v1:...`")

(* The version and the payload of a [vault:v{n}:{payload}] text. *)
let versioned text ~what =
  let malformed () =
    Error
      (Error.Malformed
         (Printf.sprintf "a Vault %s is `vault:v<n>:...`, not `%s`" what text))
  in
  match String.split_on_char ':' text with
  | "vault" :: version :: payload when String.length version > 1 && version.[0] = 'v' -> (
      match int_of_string_opt (String.sub version 1 (String.length version - 1)) with
      | Some n when n >= 0 -> (
          match Key_version.of_int n with
          | Ok version -> Ok (version, String.concat ":" payload)
          | Error _ -> malformed ())
      | Some _ | None -> malformed ())
  | _ -> malformed ()

let base64_url_encode bytes =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet bytes

let base64_url_decode ~what text =
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet text with
  | Ok bytes -> Ok bytes
  | Error (`Msg reason) ->
      Error
        (Error.Malformed (Printf.sprintf "vault's %s is not JWS base64: %s" what reason))

(* The parameters a signing operation names beside the input: JWS marshaling
   for every algorithm, so the bytes are the algorithm's raw form; the hash
   where the algorithm takes one. *)
let signing_parameters algorithm =
  match algorithm with
  | Signature_algorithm.Ed25519 -> [ ("marshaling_algorithm", `String "jws") ]
  | Signature_algorithm.Ecdsa_p256 ->
      [ ("marshaling_algorithm", `String "jws"); ("hash_algorithm", `String "sha2-256") ]

(* The Transit type of the key an algorithm signs with. *)
let transit_key_type algorithm =
  match algorithm with
  | Signature_algorithm.Ed25519 -> "ed25519"
  | Signature_algorithm.Ecdsa_p256 -> "ecdsa-p256"

let with_input parameters input : Yojson.Safe.t =
  `Assoc (("input", `String (Base64.encode_string input)) :: parameters)

(* ------------------------------------------------------------------------ *)
(* The service                                                                *)

module Make (Transport : Vault_transport.S) = struct
  type session = Transport.client Rest_session.t

  type t = {
    addr : string;
    token : string;
    mount : string;
    key_type : string;
    context : string option;
    managed_keys : bool;
  }

  let create
      ?(mount = "transit")
      ?(key_type = "aes256-gcm96")
      ?derivation_context
      ?(managed_keys = false)
      ~addr
      ~token
      () =
    {
      addr = without_trailing_slashes addr;
      token;
      mount;
      key_type;
      context = derivation_context;
      managed_keys;
    }

  (* What a KEK this service makes is made with: its type, and [derived] when
     the service serves a context. *)
  let kek_body t : Yojson.Safe.t =
    `Assoc [ ("type", `String t.key_type); ("derived", `Bool (Option.is_some t.context)) ]

  (* The body with the derivation context, base64, when the service serves
     one. *)
  let in_context t (fields : (string * Yojson.Safe.t) list) : Yojson.Safe.t =
    match t.context with
    | None -> `Assoc fields
    | Some context ->
        `Assoc (fields @ [ ("context", `String (Base64.encode_string context)) ])

  (* One call under the mount, classified by status alone: [204] is an empty
     answer, any other [2xx] its body, anything else [Vault], a [404]
     included, for the callers that name the key themselves. *)
  let transit t session ~meth ~path body =
    let url = Printf.sprintf "%s/v1/%s%s" t.addr t.mount path in
    let* (response : Vault_transport.response) =
      Rest_session.request session ~meth ~url (fun () ->
          Result.map_error
            (fun reason -> Error.Transport reason)
            (Transport.send (Rest_session.http session)
               { meth; url; token = t.token; body }))
    in
    match response.status with
    | 204 -> Ok (`Assoc [])
    | status when status >= 200 && status < 300 ->
        Ok (Option.value response.body ~default:(`Assoc []))
    | status ->
        Error (Error.Vault { status; meth; path; message = errors_of response.body })

  (* One call on the tenant's KEK: a [404] is [Kek_not_found]. *)
  let request t session ~tenant_id ~meth ~path body =
    match transit t session ~meth ~path body with
    | Error (Error.Vault { status = 404; _ }) ->
        Error (Error.Kek_not_found { tenant_id; key_version = None })
    | other -> other

  (* [POST keys/{name}] with the body: makes the key, or does nothing to one
     that exists. *)
  let ensure_named t session ~name body =
    let* _ = transit t session ~meth:"POST" ~path:("/keys/" ^ name) (Some body) in
    Ok ()

  (* The key as Vault describes it, [None] when there is none. *)
  let read_named t session ~name =
    match transit t session ~meth:"GET" ~path:("/keys/" ^ name) None with
    | Ok answer -> Ok (Some answer)
    | Error (Error.Vault { status = 404; _ }) -> Ok None
    | Error _ as error -> error

  (* The next version of the named key, or its first, made with the body. *)
  let rotate_named t session ~name body =
    let* found = read_named t session ~name in
    match found with
    | None ->
        let* () = ensure_named t session ~name body in
        Ok Key_version.first
    | Some _ -> (
        let* _ =
          transit t session ~meth:"POST"
            ~path:(Printf.sprintf "/keys/%s/rotate" name)
            (Some (`Assoc []))
        in
        let* found = read_named t session ~name in
        match found with
        | Some answer -> version_field answer "latest_version"
        | None ->
            Error
              (Error.Malformed (Printf.sprintf "key `%s` vanished while rotating" name)))

  (* Deletes the named key, every version; none is not an error. *)
  let delete_named t session ~name =
    let* found = read_named t session ~name in
    match found with
    | None -> Ok ()
    | Some _ ->
        let* _ =
          transit t session ~meth:"POST"
            ~path:(Printf.sprintf "/keys/%s/config" name)
            (Some (`Assoc [ ("deletion_allowed", `Bool true) ]))
        in
        let* _ = transit t session ~meth:"DELETE" ~path:("/keys/" ^ name) None in
        Ok ()

  (* What a [400] from an operation on the named key means: the key is not
     there ([missing]), the version the operation named is outside what the
     key serves ([No_key_of_version]), or something else, left as it was. One
     read of the key, on this path only. *)
  let explain t session ~name ~key_version ~missing error =
    match error with
    | Error.Vault { status = 400; _ } -> (
        match read_named t session ~name with
        | Ok None -> missing ()
        | Ok (Some answer) -> (
            match key_version with
            | Some version when not (serves answer version) ->
                Error.No_key_of_version (Key_version.to_int version)
            | Some _ | None -> error)
        | Error _ -> error)
    | _ -> error

  (* What a refusal of a KEK operation means, by one read of the key: a [400]
     as [explain] reads it, with the tenant's KEK as the key that may be
     missing, and, the key there and the version served, the request's own
     fault, which no retry mends: on [decrypt] and [rewrap] [Decrypt], a
     wrong context, bytes changed since they were sealed, elsewhere
     [Malformed] with Vault's words; a [403] with the key there is
     [Forbidden], with the key not there [Kek_not_found], with the read
     refused too the [403] as it came. *)
  let explain_kek t session ~tenant_id ~key_version ~operation error =
    let name = key_name tenant_id in
    let missing () = Error.Kek_not_found { tenant_id; key_version = None } in
    match error with
    | Error.Vault { status = 400; _ } -> (
        match explain t session ~name ~key_version ~missing error with
        | Error.Vault { status = 400; message; _ } ->
            if String.equal operation "decrypt" || String.equal operation "rewrap" then
              Error.Decrypt
            else Error.Malformed (Printf.sprintf "vault refused %s: %s" operation message)
        | explained -> explained)
    | Error.Vault { status = 403; _ } -> (
        match read_named t session ~name with
        | Ok (Some _) -> Error.Forbidden { tenant_id; operation }
        | Ok None -> missing ()
        | Error _ -> error)
    | error -> error

  (* A KEK operation, its refusal explained. *)
  let kek_operation t session ~tenant_id ~key_version ~operation ~path body =
    Result.map_error
      (explain_kek t session ~tenant_id ~key_version ~operation)
      (request t session ~tenant_id ~meth:"POST" ~path (Some body))

  (* Makes the tenant's KEK unless the keys are managed elsewhere. *)
  let ensure_key t session ~tenant_id =
    if t.managed_keys then Ok ()
    else
      let* _ =
        request t session ~tenant_id ~meth:"POST"
          ~path:("/keys/" ^ key_name tenant_id)
          (Some (kek_body t))
      in
      Ok ()

  (* ---------------------------------------------------------------------- *)
  (* The KEK port                                                             *)

  let encrypt_dek t session ~tenant_id dek =
    let* () = ensure_key t session ~tenant_id in
    let* answer =
      kek_operation t session ~tenant_id ~key_version:None ~operation:"encrypt"
        ~path:("/encrypt/" ^ key_name tenant_id)
        (in_context t
           [ ("plaintext", `String (Base64.encode_string (Key.to_string dek))) ])
    in
    text_field answer "ciphertext"

  let decrypt_dek t session ~tenant_id encrypted_dek =
    let* ciphertext = ciphertext_text encrypted_dek in
    let* key_version, _ = versioned ciphertext ~what:"ciphertext" in
    let* answer =
      kek_operation t session ~tenant_id ~key_version:(Some key_version)
        ~operation:"decrypt"
        ~path:("/decrypt/" ^ key_name tenant_id)
        (in_context t [ ("ciphertext", `String ciphertext) ])
    in
    Result.map Key.of_string (base64_field answer "plaintext")

  let generate_dek t session ~tenant_id =
    let* () = ensure_key t session ~tenant_id in
    let* answer =
      kek_operation t session ~tenant_id ~key_version:None ~operation:"datakey"
        ~path:("/datakey/plaintext/" ^ key_name tenant_id)
        (in_context t [ ("bits", `Int 256) ])
    in
    let* dek = base64_field answer "plaintext" in
    let* encrypted_dek = text_field answer "ciphertext" in
    Ok (Key.of_string dek, encrypted_dek)

  let rotate_kek t session ~tenant_id =
    Result.map_error
      (explain_kek t session ~tenant_id ~key_version:None ~operation:"rotate")
      (rotate_named t session ~name:(key_name tenant_id) (kek_body t))

  let rewrap_dek t session ~tenant_id encrypted_dek =
    let* ciphertext = ciphertext_text encrypted_dek in
    let* key_version, _ = versioned ciphertext ~what:"ciphertext" in
    let* answer =
      kek_operation t session ~tenant_id ~key_version:(Some key_version)
        ~operation:"rewrap"
        ~path:("/rewrap/" ^ key_name tenant_id)
        (in_context t [ ("ciphertext", `String ciphertext) ])
    in
    text_field answer "ciphertext"

  let delete_kek t session ~tenant_id = delete_named t session ~name:(key_name tenant_id)

  (* ---------------------------------------------------------------------- *)
  (* Custody                                                                  *)

  (* [POST keys/{name}] with the type and [derived], idempotent on Transit,
     which leaves a key that exists as it is and says nothing, then one read:
     a [derived] other than asked is [Conflict], since it cannot be changed;
     the rotation period, which can, is set by [config] when asked. *)
  let create_kek t session ~tenant_id (options : Kek_options.t) =
    let name = key_name tenant_id in
    let period =
      Option.map (fun seconds -> `Int (int_of_float seconds)) options.auto_rotate_period
    in
    let body =
      `Assoc
        ([ ("type", `String t.key_type); ("derived", `Bool options.derived) ]
        @ Option.to_list
            (Option.map (fun period -> ("auto_rotate_period", period)) period))
    in
    let* _ =
      request t session ~tenant_id ~meth:"POST" ~path:("/keys/" ^ name) (Some body)
    in
    let* found = read_named t session ~name in
    let* answer =
      Option.to_result
        ~none:(Error.Malformed (Printf.sprintf "key `%s` vanished while being made" name))
        found
    in
    let derived = bool_field answer "derived" in
    if derived <> options.derived then
      Error
        (Error.Conflict
           {
             tenant_id;
             detail =
               Printf.sprintf "the key is %s, a %s one was asked"
                 (if derived then "derived" else "not derived")
                 (if options.derived then "derived" else "plain");
           })
    else
      let* () =
        match period with
        | None -> Ok ()
        | Some period ->
            let* _ =
              request t session ~tenant_id ~meth:"POST"
                ~path:(Printf.sprintf "/keys/%s/config" name)
                (Some (`Assoc [ ("auto_rotate_period", period) ]))
            in
            Ok ()
      in
      version_field answer "latest_version"

  (* [min_decryption_version] of the key raised to the version. *)
  let retire_kek_versions_before t session ~tenant_id version =
    let* _ =
      kek_operation t session ~tenant_id ~key_version:None ~operation:"retire"
        ~path:(Printf.sprintf "/keys/%s/config" (key_name tenant_id))
        (`Assoc [ ("min_decryption_version", `Int (Key_version.to_int version)) ])
    in
    Ok ()

  (* ---------------------------------------------------------------------- *)
  (* Signing                                                                  *)

  let sign t session ~tenant_id algorithm message =
    let* name = signing_key_name ~tenant_id algorithm in
    let* () =
      ensure_named t session ~name
        (`Assoc [ ("type", `String (transit_key_type algorithm)) ])
    in
    let* answer =
      transit t session ~meth:"POST" ~path:("/sign/" ^ name)
        (Some (with_input (signing_parameters algorithm) message))
    in
    let* text = text_field answer "signature" in
    let* key_version, payload = versioned text ~what:"signature" in
    let* bytes = base64_url_decode ~what:"signature" payload in
    Ok (Signature.make ~algorithm ~key_version bytes)

  let verify t session ~tenant_id message signature =
    let algorithm = Signature.algorithm signature in
    let key_version = Signature.key_version signature in
    let* name = signing_key_name ~tenant_id algorithm in
    let body =
      `Assoc
        (("input", `String (Base64.encode_string message))
        :: ( "signature",
             `String
               (Printf.sprintf "vault:v%d:%s"
                  (Key_version.to_int key_version)
                  (base64_url_encode (Signature.to_string signature))) )
        :: signing_parameters algorithm)
    in
    let* answer =
      Result.map_error
        (explain t session ~name ~key_version:(Some key_version) ~missing:(fun () ->
             Error.Signing_key_not_found { tenant_id; algorithm }))
        (transit t session ~meth:"POST" ~path:("/verify/" ^ name) (Some body))
    in
    match data_field answer "valid" with
    | Some (`Bool true) -> Ok ()
    | Some (`Bool false) -> Error Error.Invalid_signature
    | Some _ | None -> Error (Error.Malformed "vault answered without `data.valid`")

  let rotate_signing_key t session ~tenant_id algorithm =
    let* name = signing_key_name ~tenant_id algorithm in
    rotate_named t session ~name
      (`Assoc [ ("type", `String (transit_key_type algorithm)) ])

  let delete_signing_key t session ~tenant_id algorithm =
    let* name = signing_key_name ~tenant_id algorithm in
    delete_named t session ~name

  (* ---------------------------------------------------------------------- *)
  (* MACs                                                                     *)

  let mac t session ~tenant_id purpose message =
    let* name = mac_key_name ~tenant_id purpose in
    let* () =
      ensure_named t session ~name
        (`Assoc [ ("type", `String "hmac"); ("key_size", `Int 32) ])
    in
    let* answer =
      transit t session ~meth:"POST" ~path:("/hmac/" ^ name)
        (Some (with_input [ ("algorithm", `String "sha2-256") ] message))
    in
    let* text = text_field answer "hmac" in
    let* key_version, payload = versioned text ~what:"hmac" in
    match Base64.decode payload with
    | Ok bytes -> Ok (Mac.make ~key_version bytes)
    | Error (`Msg reason) ->
        Error (Error.Malformed (Printf.sprintf "vault's hmac is not base64: %s" reason))

  let delete_mac_key t session ~tenant_id purpose =
    let* name = mac_key_name ~tenant_id purpose in
    delete_named t session ~name
end
