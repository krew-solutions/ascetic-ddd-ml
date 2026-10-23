module Error = Ascetic_kms.Kms_error
module Key = Ascetic_kms.Key
module Key_version = Ascetic_kms.Key_version
module Kek_options = Ascetic_kms.Kek_options
module Key_options = Ascetic_kms.Key_options
module Signature_algorithm = Ascetic_kms.Signature_algorithm
module Signature = Ascetic_kms.Signature
module Signing_key = Ascetic_kms.Signing_key
module Public_key = Ascetic_kms.Public_key
module Public_keyring = Ascetic_kms.Public_keyring
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

(* [data.<field>] of an answer as a version, [default] when it is not there
   or not one. *)
let version_or answer field ~default = Result.value (version_field answer field) ~default

let zero = Key_version.of_int_exn 0

(* The oldest version the key, as Vault describes it, serves: its
   [min_decryption_version], one when unsaid. *)
let floor_of answer =
  version_or answer "min_decryption_version" ~default:Key_version.first

(* The key's floor of signing, [min_encryption_version]: zero, or absent,
   is no floor of its own (measured, Vault 1.21: a retirement does not raise
   it). *)
let signing_floor_of answer = version_or answer "min_encryption_version" ~default:zero

let between ~oldest ~latest version =
  Key_version.compare oldest version <= 0 && Key_version.compare version latest <= 0

(* Whether the key, as Vault describes it, serves the version: made, and not
   retired by [min_decryption_version]. *)
let serves answer version =
  match version_field answer "latest_version" with
  | Error _ -> false
  | Ok latest -> between ~oldest:(floor_of answer) ~latest version

(* Whether the key, as Vault describes it, signs with the version: made, not
   retired, and not below [min_encryption_version], which Transit holds as a
   floor of signing too. *)
let signs answer version =
  match version_field answer "latest_version" with
  | Error _ -> false
  | Ok latest ->
      let oldest =
        List.fold_left
          (fun oldest floor ->
            if Key_version.compare floor oldest > 0 then floor else oldest)
          Key_version.first
          [ floor_of answer; signing_floor_of answer ]
      in
      between ~oldest ~latest version

(* What a use asks of a version of the key: that it signs with it, or that
   it serves it, verifies a signature by it. A version below the key's
   [min_encryption_version] serves and no longer signs, so a refusal of a
   signature by it is the version's, and a refusal of its verification is
   the request's own. *)
type asked = Signs of Key_version.t | Serves of Key_version.t

let asked_version = function
  | Signs version | Serves version -> version

(* Whether the key, as Vault describes it, holds what was asked. *)
let holds asked answer =
  match asked with
  | Signs version -> signs answer version
  | Serves version -> serves answer version

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

(* The signature of the algorithm in a [sign] answer: the version Vault
   names, and the JWS bytes. *)
let signature_of algorithm answer =
  let* text = text_field answer "signature" in
  let* key_version, payload = versioned text ~what:"signature" in
  let* bytes = base64_url_decode ~what:"signature" payload in
  Ok (Signature.make ~algorithm ~key_version bytes)

(* The DER of a SubjectPublicKeyInfo of an uncompressed P-256 point, as Go's
   x509 marshals it and Transit publishes it: [SEQUENCE { SEQUENCE {
   id-ecPublicKey, prime256v1 }, BIT STRING { 0x04 || x || y } }]. Every
   byte before the point is fixed by the algorithm and the curve, so the
   point is what follows this prefix; another prefix is another key
   entirely, another curve or a compressed point, which Transit does not
   publish and the port does not promise to read. *)
let p256_spki_prefix =
  "\x30\x59\x30\x13\x06\x07\x2a\x86\x48\xce\x3d\x02\x01\x06\x08\x2a\x86\x48\xce\x3d\x03\x01\x07\x03\x42\x00"

(* A public key as Transit publishes it, in the model's form: base64 of the
   raw bytes for Ed25519, a PEM SubjectPublicKeyInfo for P-256. *)
let public_key_of algorithm text =
  let malformed what =
    Error
      (Error.Malformed
         (Format.asprintf "vault's %a public key: %s" Signature_algorithm.pp algorithm
            what))
  in
  match algorithm with
  | Signature_algorithm.Ed25519 -> (
      match Base64.decode (String.trim text) with
      | Ok bytes -> Public_key.make algorithm bytes
      | Error _ -> malformed "not base64")
  | Signature_algorithm.Ecdsa_p256 -> (
      (* The PEM without its armor is the DER of the SubjectPublicKeyInfo,
         base64 over several lines. *)
      let lines =
        List.filter
          (fun line -> not (String.equal line ""))
          (List.map String.trim (String.split_on_char '\n' text))
      in
      match lines with
      | "-----BEGIN PUBLIC KEY-----" :: rest when List.mem "-----END PUBLIC KEY-----" rest
        -> (
          let body =
            String.concat ""
              (List.filter
                 (fun line -> not (String.starts_with ~prefix:"-----" line))
                 rest)
          in
          match Base64.decode body with
          | Error _ -> malformed "not base64"
          | Ok der ->
              let prefix = String.length p256_spki_prefix in
              if
                String.length der = prefix + 65
                && String.equal (String.sub der 0 prefix) p256_spki_prefix
              then Public_key.make algorithm (String.sub der prefix 65)
              else malformed "not a P-256 SubjectPublicKeyInfo of an uncompressed point")
      | _ -> malformed "not a PEM public key")

(* The Transit type of the key an algorithm signs with. *)
let transit_key_type algorithm =
  match algorithm with
  | Signature_algorithm.Ed25519 -> "ed25519"
  | Signature_algorithm.Ecdsa_p256 -> "ecdsa-p256"

(* The public halves of the versions a key described by the answer serves:
   [data.keys.{n}.public_key], each read into the model's form. The key must
   be of the algorithm's Transit type. *)
let public_keyring algorithm answer =
  let* kind = text_field answer "type" in
  if not (String.equal kind (transit_key_type algorithm)) then
    Error
      (Error.Malformed
         (Format.asprintf "vault describes the key as `%s`, not as a %a key" kind
            Signature_algorithm.pp algorithm))
  else
    let* keys =
      match data_field answer "keys" with
      | Some (`Assoc keys) -> Ok keys
      | Some _ | None -> Error (Error.Malformed "vault answered without `data.keys`")
    in
    let* ring =
      List.fold_right
        (fun (version, entry) ring ->
          let* ring = ring in
          let* version =
            match int_of_string_opt version with
            | Some n when n >= 0 -> Key_version.of_int n
            | Some _ | None ->
                Error
                  (Error.Malformed
                     (Printf.sprintf "vault names a key version `%s`" version))
          in
          if not (serves answer version) then Ok ring
          else
            let* text =
              match entry with
              | `Assoc fields -> (
                  match List.assoc_opt "public_key" fields with
                  | Some (`String text) -> Ok text
                  | Some _ | None ->
                      Error
                        (Error.Malformed
                           (Format.asprintf "vault answered no public key for v%a"
                              Key_version.pp version)))
              | _ ->
                  Error
                    (Error.Malformed
                       (Format.asprintf "vault answered no public key for v%a"
                          Key_version.pp version))
            in
            let* key = public_key_of algorithm text in
            Ok ((version, key) :: ring))
        keys (Ok [])
    in
    Public_keyring.make algorithm ring

(* The parameters a signing operation names beside the input: JWS marshaling
   for every algorithm, so the bytes are the algorithm's raw form; the hash
   where the algorithm takes one. *)
let signing_parameters algorithm =
  match algorithm with
  | Signature_algorithm.Ed25519 -> [ ("marshaling_algorithm", `String "jws") ]
  | Signature_algorithm.Ecdsa_p256 ->
      [ ("marshaling_algorithm", `String "jws"); ("hash_algorithm", `String "sha2-256") ]

let with_input parameters input : Yojson.Safe.t =
  `Assoc (("input", `String (Base64.encode_string input)) :: parameters)

(* What a MAC key is made with: HMAC over thirty-two bytes. *)
let mac_key_fields : (string * Yojson.Safe.t) list =
  [ ("type", `String "hmac"); ("key_size", `Int 32) ]

let kek_not_found tenant_id = Error.Kek_not_found { tenant_id; key_version = None }

let signing_key_not_found ~tenant_id algorithm =
  Error.Signing_key_not_found { tenant_id; algorithm }

let mac_key_not_found ~tenant_id purpose =
  Error.Mac_key_not_found { tenant_id; purpose = Key_label.to_string purpose }

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

  (* What a refusal of a custody operation on the named key means, by one
     read on a [400] or a [403]: the key not there, [missing]; a [403] with
     the key there, [Forbidden], the identity holds no custody; a [400] with
     the key there, the request's own fault, [Malformed] with Vault's words.
     Anything else, and a read refused, as it came. *)
  let explain_custody t session ~tenant_id ~name ~operation ~missing error =
    match error with
    | Error.Vault { status = (400 | 403) as status; message; _ } -> (
        match read_named t session ~name with
        | Ok None -> missing
        | Ok (Some _) when status = 403 -> Error.Forbidden { tenant_id; operation }
        | Ok (Some _) ->
            Error.Malformed (Printf.sprintf "vault refused %s: %s" operation message)
        | Error _ -> error)
    | error -> error

  (* Makes the named key with the body, [POST keys/{name}], which Transit
     answers for a key that exists with its description and without
     touching it (measured, Vault 1.21), and holds what is there to what was
     asked: another type, or another [derived], is [Conflict]; a rotation
     period asked and not the key's is set by [config]. Answers the latest
     version. One read only where Vault answered without the description. *)
  let create_named
      t
      session
      ~tenant_id
      ~name
      ~key_type
      ~derived
      ~auto_rotate_period
      (fields : (string * Yojson.Safe.t) list) =
    let period = Option.map (fun seconds -> int_of_float seconds) auto_rotate_period in
    let body =
      `Assoc
        (fields
        @ Option.to_list
            (Option.map (fun period -> ("auto_rotate_period", `Int period)) period))
    in
    let* answer =
      match transit t session ~meth:"POST" ~path:("/keys/" ^ name) (Some body) with
      | Ok answer -> Ok answer
      (* A making refused under this identity is the policy's, whether the
         key is there or not: no read explains more. *)
      | Error (Error.Vault { status = 403; _ }) ->
          Error (Error.Forbidden { tenant_id; operation = "create" })
      | Error _ as error -> error
    in
    let* answer =
      match data_field answer "type" with
      | Some _ -> Ok answer
      | None -> (
          let* found = read_named t session ~name in
          match found with
          | Some answer -> Ok answer
          | None ->
              Error
                (Error.Malformed
                   (Printf.sprintf "key `%s` vanished while being made" name)))
    in
    let found_type = Result.value (text_field answer "type") ~default:"" in
    let found_derived = bool_field answer "derived" in
    if not (String.equal found_type key_type && Bool.equal found_derived derived) then
      let kind key_type derived = (if derived then "derived " else "") ^ key_type in
      Error
        (Error.Conflict
           {
             tenant_id;
             detail =
               Printf.sprintf "`%s` is a %s key, a %s one was asked" name
                 (kind found_type found_derived)
                 (kind key_type derived);
           })
    else
      let* () =
        match period with
        | None -> Ok ()
        | Some period ->
            let found =
              match data_field answer "auto_rotate_period" with
              | Some (`Int found) -> found
              | Some _ | None -> 0
            in
            if found = period then Ok ()
            else
              let* _ =
                transit t session ~meth:"POST"
                  ~path:(Printf.sprintf "/keys/%s/config" name)
                  (Some (`Assoc [ ("auto_rotate_period", `Int period) ]))
              in
              Ok ()
      in
      version_field answer "latest_version"

  (* The next version of the named key, [POST keys/{name}/rotate], which
     Transit refuses for a key it has not got: a rotation never makes the
     first version. Answers the new latest version, from the answer or,
     where Vault said nothing, one read. *)
  let rotate_named t session ~tenant_id ~name ~missing =
    let* answer =
      Result.map_error
        (explain_custody t session ~tenant_id ~name ~operation:"rotate" ~missing)
        (transit t session ~meth:"POST"
           ~path:(Printf.sprintf "/keys/%s/rotate" name)
           (Some (`Assoc [])))
    in
    match version_field answer "latest_version" with
    | Ok version -> Ok version
    | Error _ -> (
        let* found = read_named t session ~name in
        match found with
        | Some answer -> version_field answer "latest_version"
        | None ->
            Error
              (Error.Malformed (Printf.sprintf "key `%s` vanished while rotating" name)))

  (* Retires the named key's versions below the bound by its
     [min_decryption_version], after one read: a bound above the latest is
     [No_key_of_version] and nothing is written; one at or below the floor
     there is writes nothing, Transit lowers the floor when asked (measured,
     Vault 1.21), and a retried or a stale retirement must not bring a
     version back. Where the key has a signing floor of its own
     ([min_encryption_version], not zero) below the bound, it is raised
     with it in the same call: Transit holds it at or above the decryption
     floor, and a retired version signs nothing anyway. *)
  let retire_named t session ~tenant_id ~name ~bound ~missing =
    let* found = read_named t session ~name in
    match found with
    | None -> Error missing
    | Some answer ->
        let* latest = version_field answer "latest_version" in
        if Key_version.compare bound latest > 0 then
          Error (Error.No_key_of_version (Key_version.to_int bound))
        else if Key_version.compare bound (floor_of answer) <= 0 then Ok ()
        else
          let signing_floor = signing_floor_of answer in
          let raised =
            if
              between ~oldest:Key_version.first ~latest:bound signing_floor
              && not (Key_version.equal signing_floor bound)
            then [ ("min_encryption_version", `Int (Key_version.to_int bound)) ]
            else []
          in
          let* _ =
            Result.map_error
              (explain_custody t session ~tenant_id ~name ~operation:"retire" ~missing)
              (transit t session ~meth:"POST"
                 ~path:(Printf.sprintf "/keys/%s/config" name)
                 (Some
                    (`Assoc
                       (("min_decryption_version", `Int (Key_version.to_int bound))
                       :: raised))))
          in
          Ok ()

  (* Deletes the named key, every version; none is not an error. A refusal
     under this identity, the key there, is [Forbidden]. *)
  let delete_named t session ~tenant_id ~name =
    let forbidden = function
      | Error.Vault { status = 403; _ } ->
          Error.Forbidden { tenant_id; operation = "delete" }
      | error -> error
    in
    let* found = read_named t session ~name in
    match found with
    | None -> Ok ()
    | Some _ ->
        let* _ =
          Result.map_error forbidden
            (transit t session ~meth:"POST"
               ~path:(Printf.sprintf "/keys/%s/config" name)
               (Some (`Assoc [ ("deletion_allowed", `Bool true) ])))
        in
        let* _ =
          Result.map_error forbidden
            (transit t session ~meth:"DELETE" ~path:("/keys/" ^ name) None)
        in
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

  (* What a refusal of signing, of verifying or of a MAC means, by one read
     of the key, on a [400], a [403] or a [500], since Transit answers [400]
     for a key it has not got and, signing at a named version, [500] for a
     version above the latest, retired, or below the one the key signs from
     (measured, Vault 1.21): the key not there, [missing]; a [403] with the
     key there, [Forbidden]; a version asked that the key does not sign with,
     or, verifying, does not serve, [No_key_of_version]; a [400] the read
     does not explain, the request's own fault, [Malformed] (the rule
     ADR-0024 set for the KEK); a [500] it does not explain stays Vault's, of
     the moment; the read refused, the refusal as it came. Any other status,
     [429], [502], [503], [504], is left as it came, unread: a read would add
     to the load a rate limit protects, or meet the same sealed or standby
     node. Vault's texts are not matched. *)
  let explain_use t session ~tenant_id ~name ~operation ~asked ~missing error =
    match error with
    | Error.Vault { status = (400 | 403 | 500) as status; message; _ } -> (
        match read_named t session ~name with
        | Ok None -> missing
        | Ok (Some _) when status = 403 -> Error.Forbidden { tenant_id; operation }
        | Ok (Some answer)
          when Option.is_some asked && not (holds (Option.get asked) answer) ->
            Error.No_key_of_version
              (Key_version.to_int (asked_version (Option.get asked)))
        | Ok (Some _) when status = 400 ->
            Error.Malformed (Printf.sprintf "vault refused %s: %s" operation message)
        | Ok (Some _) | Error _ -> error)
    | error -> error

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
    let missing () = kek_not_found tenant_id in
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

  (* ---------------------------------------------------------------------- *)
  (* Custody of the KEK                                                       *)

  (* [POST keys/{name}] with the service's key type, [derived] and the
     rotation period asked; a key there of another type or another
     [derived] is [Conflict], since neither can be changed; the period,
     which can, is set by [config] when it differs. *)
  let create_kek t session ~tenant_id (options : Kek_options.t) =
    create_named t session ~tenant_id ~name:(key_name tenant_id) ~key_type:t.key_type
      ~derived:options.derived ~auto_rotate_period:options.auto_rotate_period
      [ ("type", `String t.key_type); ("derived", `Bool options.derived) ]

  let rotate_kek t session ~tenant_id =
    rotate_named t session ~tenant_id ~name:(key_name tenant_id)
      ~missing:(kek_not_found tenant_id)

  (* [min_decryption_version] of the key raised to the version, never
     lowered. *)
  let retire_kek_versions_before t session ~tenant_id bound =
    retire_named t session ~tenant_id ~name:(key_name tenant_id) ~bound
      ~missing:(kek_not_found tenant_id)

  let delete_kek t session ~tenant_id =
    delete_named t session ~tenant_id ~name:(key_name tenant_id)

  (* ---------------------------------------------------------------------- *)
  (* Signing                                                                  *)

  let sign t session ~tenant_id algorithm message =
    let* name = signing_key_name ~tenant_id algorithm in
    let* () =
      if t.managed_keys then Ok ()
      else
        ensure_named t session ~name
          (`Assoc [ ("type", `String (transit_key_type algorithm)) ])
    in
    let* answer =
      Result.map_error
        (explain_use t session ~tenant_id ~name ~operation:"sign" ~asked:None
           ~missing:(signing_key_not_found ~tenant_id algorithm))
        (transit t session ~meth:"POST" ~path:("/sign/" ^ name)
           (Some (with_input (signing_parameters algorithm) message)))
    in
    signature_of algorithm answer

  let sign_with_version t session ~tenant_id algorithm key_version message =
    let* key_version = Key_version.from_one key_version in
    let* name = signing_key_name ~tenant_id algorithm in
    let body =
      with_input
        (signing_parameters algorithm
        @ [ ("key_version", `Int (Key_version.to_int key_version)) ])
        message
    in
    let* answer =
      Result.map_error
        (explain_use t session ~tenant_id ~name ~operation:"sign"
           ~asked:(Some (Signs key_version))
           ~missing:(signing_key_not_found ~tenant_id algorithm))
        (transit t session ~meth:"POST" ~path:("/sign/" ^ name) (Some body))
    in
    let* signature = signature_of algorithm answer in
    if Key_version.equal (Signature.key_version signature) key_version then Ok signature
    else
      Error
        (Error.Malformed
           (Format.asprintf "vault signed with v%a, v%a was asked" Key_version.pp
              (Signature.key_version signature)
              Key_version.pp key_version))

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
        (explain_use t session ~tenant_id ~name ~operation:"verify"
           ~asked:(Some (Serves key_version))
           ~missing:(signing_key_not_found ~tenant_id algorithm))
        (transit t session ~meth:"POST" ~path:("/verify/" ^ name) (Some body))
    in
    match data_field answer "valid" with
    | Some (`Bool true) -> Ok ()
    | Some (`Bool false) -> Error Error.Invalid_signature
    | Some _ | None -> Error (Error.Malformed "vault answered without `data.valid`")

  (* ---- Custody of the signing keys: [{tenant}.sign.{algorithm}], of the
     algorithm's Transit type, never derived ---- *)

  let create_signing_key t session ~tenant_id algorithm (options : Key_options.t) =
    let* name = signing_key_name ~tenant_id algorithm in
    let key_type = transit_key_type algorithm in
    create_named t session ~tenant_id ~name ~key_type ~derived:false
      ~auto_rotate_period:options.auto_rotate_period
      [ ("type", `String key_type) ]

  let rotate_signing_key t session ~tenant_id algorithm =
    let* name = signing_key_name ~tenant_id algorithm in
    rotate_named t session ~tenant_id ~name
      ~missing:(signing_key_not_found ~tenant_id algorithm)

  let retire_signing_versions_before t session ~tenant_id algorithm bound =
    let* name = signing_key_name ~tenant_id algorithm in
    retire_named t session ~tenant_id ~name ~bound
      ~missing:(signing_key_not_found ~tenant_id algorithm)

  let delete_signing_key t session ~tenant_id algorithm =
    let* name = signing_key_name ~tenant_id algorithm in
    delete_named t session ~tenant_id ~name

  (* ---- The public keys: [GET keys/{name}], the versions the key serves,
     read into the model's form; never makes a key ---- *)

  let public_keys t session ~tenant_id algorithm =
    let* name = signing_key_name ~tenant_id algorithm in
    let* found = read_named t session ~name in
    match found with
    | None -> Error (signing_key_not_found ~tenant_id algorithm)
    | Some answer -> public_keyring algorithm answer

  (* ---------------------------------------------------------------------- *)
  (* MACs                                                                     *)

  let mac t session ~tenant_id purpose message =
    let* name = mac_key_name ~tenant_id purpose in
    let* () =
      if t.managed_keys then Ok ()
      else ensure_named t session ~name (`Assoc mac_key_fields)
    in
    let* answer =
      Result.map_error
        (explain_use t session ~tenant_id ~name ~operation:"hmac" ~asked:None
           ~missing:(mac_key_not_found ~tenant_id purpose))
        (transit t session ~meth:"POST" ~path:("/hmac/" ^ name)
           (Some (with_input [ ("algorithm", `String "sha2-256") ] message)))
    in
    let* text = text_field answer "hmac" in
    let* key_version, payload = versioned text ~what:"hmac" in
    match Base64.decode payload with
    | Ok bytes -> Ok (Mac.make ~key_version bytes)
    | Error (`Msg reason) ->
        Error (Error.Malformed (Printf.sprintf "vault's hmac is not base64: %s" reason))

  (* ---- Custody of the MAC keys: [{tenant}.mac.{purpose}], of type [hmac],
     never derived ---- *)

  let create_mac_key t session ~tenant_id purpose (options : Key_options.t) =
    let* name = mac_key_name ~tenant_id purpose in
    create_named t session ~tenant_id ~name ~key_type:"hmac" ~derived:false
      ~auto_rotate_period:options.auto_rotate_period mac_key_fields

  let rotate_mac_key t session ~tenant_id purpose =
    let* name = mac_key_name ~tenant_id purpose in
    rotate_named t session ~tenant_id ~name
      ~missing:(mac_key_not_found ~tenant_id purpose)

  let retire_mac_versions_before t session ~tenant_id purpose bound =
    let* name = mac_key_name ~tenant_id purpose in
    retire_named t session ~tenant_id ~name ~bound
      ~missing:(mac_key_not_found ~tenant_id purpose)

  let delete_mac_key t session ~tenant_id purpose =
    let* name = mac_key_name ~tenant_id purpose in
    delete_named t session ~tenant_id ~name
end
