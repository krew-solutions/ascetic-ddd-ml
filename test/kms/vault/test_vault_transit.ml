(** The Vault Transit adapter against a scripted transport: what it asks of Vault, and how
    it reads the answers, including how it tells Transit's [400]s, [403]s and [500]s apart
    by one read of the key, how a pinned signature asks its version and makes no key, how
    the public keys are read from what Transit publishes, and what custody asks. No Vault
    needed; [test_vault] and [test_vault_signing] run the same service against a dev
    server. *)

open Ascetic_kms
module Transport = Ascetic_kms_vault.Vault_transport
module Rest = Ascetic_session_rest.Rest_session
module Rest_observer = Ascetic_session_rest.Rest_observer

(* A transport that answers from a script and keeps what it was asked. *)
module Scripted = struct
  type client = {
    mutable answers : (Transport.response, string) result list;
    mutable asked : Transport.request list;
  }

  let create answers = { answers; asked = [] }
  let asked client = List.rev client.asked

  let send client request =
    client.asked <- request :: client.asked;
    match client.answers with
    | answer :: rest ->
        client.answers <- rest;
        answer
    | [] -> Alcotest.failf "an unexpected call: %s %s" request.meth request.url
end

module Vault = Ascetic_kms_vault.Vault_transit.Make (Scripted)

let answer ?body status : (Transport.response, string) result =
  Ok { status; body = Option.map Yojson.Safe.from_string body }

let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_key = Alcotest.testable Key.pp Key.equal

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let shown (request : Transport.request) =
  Printf.sprintf "%s %s%s" request.meth request.url
    (match request.body with
    | Some body -> " " ^ Yojson.Safe.to_string body
    | None -> "")

let with_service ?observer env answers body =
  let client = Scripted.create answers in
  let session = Rest.create ?observer ~clock:(Eio.Stdenv.mono_clock env) client in
  let kms = Vault.create ~addr:"http://vault:8200/" ~token:"the-token" () in
  body kms session client

(* A key as [GET keys/{name}] describes it, and, on Vault 1.21, as the answer to
   [POST keys/{name}] does. *)
let key_of_type ?(derived = false) ?(signing_floor = 0) ~latest ~oldest key_type =
  Printf.sprintf
    {|{"data":{"type":"%s","latest_version":%d,"min_decryption_version":%d,"min_encryption_version":%d,"derived":%b}}|}
    key_type latest oldest signing_floor derived

let key_description ?derived ?signing_floor ~latest ~oldest () =
  key_of_type ?derived ?signing_floor ~latest ~oldest "aes256-gcm96"

let test_a_tenant_id_becomes_a_path_segment _env =
  let key_name = Ascetic_kms_vault.Vault_transit.key_name in
  Alcotest.(check string) "unreserved" "tenant-1" (key_name "tenant-1");
  Alcotest.(check string) "a slash and a space" "a%2Fb%20c" (key_name "a/b c");
  Alcotest.(check string) "the other unreserved ones" "x.y_z~" (key_name "x.y_z~");
  Alcotest.(check string) "byte by byte" "%D0%B6" (key_name "\xd0\xb6")

let test_a_trailing_slash_in_the_address_is_dropped env =
  List.iter
    (fun addr ->
      let client = Scripted.create [ answer 404 ] in
      let session = Rest.create ~clock:(Eio.Stdenv.mono_clock env) client in
      let kms = Vault.create ~addr ~token:"t" () in
      ignore (Vault.delete_kek kms session ~tenant_id:"t1");
      Alcotest.(check (list string))
        addr
        [ "GET http://vault:8200/v1/transit/keys/t1" ]
        (List.map shown (Scripted.asked client)))
    [ "http://vault:8200"; "http://vault:8200/"; "http://vault:8200//" ]

let test_a_dek_is_wrapped_under_the_tenant_s_key env =
  with_service env
    [ answer 204; answer 200 ~body:{|{"data":{"ciphertext":"vault:v1:abc"}}|} ]
  @@ fun kms session client ->
  Alcotest.(check (result string error))
    "Vault's ciphertext text" (Ok "vault:v1:abc")
    (Vault.encrypt_dek kms session ~tenant_id:"a/b" (Key.of_string "0123"));
  Alcotest.(check (list string))
    "the key is made first; the address lost its slash, the token goes along"
    [
      {|POST http://vault:8200/v1/transit/keys/a%2Fb {"type":"aes256-gcm96","derived":false}|};
      {|POST http://vault:8200/v1/transit/encrypt/a%2Fb {"plaintext":"MDEyMw=="}|};
    ]
    (List.map shown (Scripted.asked client));
  Alcotest.(check (list string))
    "the token" [ "the-token"; "the-token" ]
    (List.map (fun (r : Transport.request) -> r.token) (Scripted.asked client))

let test_a_dek_is_unwrapped env =
  with_service env [ answer 200 ~body:{|{"data":{"plaintext":"MDEyMw=="}}|} ]
  @@ fun kms session client ->
  Alcotest.(check (result a_key error))
    "decoded"
    (Ok (Key.of_string "0123"))
    (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc");
  Alcotest.(check (list string))
    "asked"
    [ {|POST http://vault:8200/v1/transit/decrypt/t1 {"ciphertext":"vault:v1:abc"}|} ]
    (List.map shown (Scripted.asked client))

let test_a_dek_is_drawn_from_vault env =
  with_service env
    [
      answer 204;
      answer 200 ~body:{|{"data":{"plaintext":"MDEyMw==","ciphertext":"vault:v1:abc"}}|};
    ]
  @@ fun kms session client ->
  (match Vault.generate_dek kms session ~tenant_id:"t1" with
  | Ok (dek, wrapped) ->
      Alcotest.check a_key "in the clear" (Key.of_string "0123") dek;
      Alcotest.(check string) "and wrapped" "vault:v1:abc" wrapped
  | Error e -> Alcotest.failf "generate_dek: %a" Kms_error.pp e);
  Alcotest.(check string)
    "asked for 256 bits"
    {|POST http://vault:8200/v1/transit/datakey/plaintext/t1 {"bits":256}|}
    (shown (List.nth (Scripted.asked client) 1))

(* A rotation asks Transit to rotate and nothing else: Transit refuses one
   of a key it has not got, and the adapter makes no first version. *)
let test_a_rotation_never_makes_the_first_version env =
  with_service env
    [
      answer 400 ~body:{|{"errors":["key not found"]}|};
      answer 404 ~body:{|{"errors":[]}|};
    ]
  @@ fun kms session client ->
  Alcotest.(check (result (Alcotest.testable Key_version.pp Key_version.equal) error))
    "not found"
    (Error (Kms_error.Kek_not_found { tenant_id = "t1"; key_version = None }))
    (Vault.rotate_kek kms session ~tenant_id:"t1");
  Alcotest.(check (list string))
    "rotated, read once, nothing made"
    [
      "POST http://vault:8200/v1/transit/keys/t1/rotate {}";
      "GET http://vault:8200/v1/transit/keys/t1";
    ]
    (List.map shown (Scripted.asked client))

(* The new version is the answer's; where Vault says nothing, one read. *)
let test_a_rotation_takes_the_version_from_the_answer_or_reads_it env =
  with_service env [ answer 200 ~body:(key_description ~latest:3 ~oldest:1 ()) ]
  @@ fun kms session client ->
  Alcotest.(check bool)
    "version three" true
    (Vault.rotate_kek kms session ~tenant_id:"t1" = Ok (Key_version.of_int_exn 3));
  Alcotest.(check (list string))
    "the answer's"
    [ "POST http://vault:8200/v1/transit/keys/t1/rotate {}" ]
    (List.map shown (Scripted.asked client));
  with_service env
    [ answer 204; answer 200 ~body:(key_description ~latest:3 ~oldest:1 ()) ]
  @@ fun kms session client ->
  Alcotest.(check bool)
    "version three" true
    (Vault.rotate_kek kms session ~tenant_id:"t1" = Ok (Key_version.of_int_exn 3));
  Alcotest.(check (list string))
    "read once"
    [
      "POST http://vault:8200/v1/transit/keys/t1/rotate {}";
      "GET http://vault:8200/v1/transit/keys/t1";
    ]
    (List.map shown (Scripted.asked client))

let test_a_rewrap_is_vault_s env =
  with_service env [ answer 200 ~body:{|{"data":{"ciphertext":"vault:v2:def"}}|} ]
  @@ fun kms session client ->
  Alcotest.(check (result string error))
    "rewrapped" (Ok "vault:v2:def")
    (Vault.rewrap_dek kms session ~tenant_id:"t1" "vault:v1:abc");
  Alcotest.(check (list string))
    "asked"
    [ {|POST http://vault:8200/v1/transit/rewrap/t1 {"ciphertext":"vault:v1:abc"}|} ]
    (List.map shown (Scripted.asked client))

let test_deleting_allows_the_deletion_first env =
  with_service env [ answer 200 ~body:{|{"data":{}}|}; answer 204; answer 204 ]
  @@ fun kms session client ->
  Alcotest.(check (result unit error))
    "deleted" (Ok ())
    (Vault.delete_kek kms session ~tenant_id:"t1");
  Alcotest.(check (list string))
    "asked"
    [
      "GET http://vault:8200/v1/transit/keys/t1";
      {|POST http://vault:8200/v1/transit/keys/t1/config {"deletion_allowed":true}|};
      "DELETE http://vault:8200/v1/transit/keys/t1";
    ]
    (List.map shown (Scripted.asked client))

let test_nothing_to_delete_is_not_an_error env =
  with_service env [ answer 404 ] @@ fun kms session client ->
  Alcotest.(check (result unit error))
    "nothing done" (Ok ())
    (Vault.delete_kek kms session ~tenant_id:"t1");
  Alcotest.(check int) "one call" 1 (List.length (Scripted.asked client))

let test_vault_s_errors_are_joined env =
  with_service env [ answer 400 ~body:{|{"errors":["one","two"]}|} ]
  @@ fun kms session _ ->
  Alcotest.(check (result unit error))
    "refused"
    (Error
       (Kms_error.Vault
          { status = 400; meth = "GET"; path = "/keys/t1"; message = "one; two" }))
    (Vault.delete_kek kms session ~tenant_id:"t1")

let test_a_refusal_without_a_body_has_no_message env =
  with_service env [ answer 503 ] @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "refused"
    (Error
       (Kms_error.Vault
          { status = 503; meth = "POST"; path = "/decrypt/t1"; message = "" }))
    (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc")

let test_a_missing_key_is_reported_as_such env =
  with_service env [ answer 404 ] @@ fun kms session _ ->
  Alcotest.(check (result string error))
    "no key"
    (Error (Kms_error.Kek_not_found { tenant_id = "t1"; key_version = None }))
    (Vault.rewrap_dek kms session ~tenant_id:"t1" "vault:v1:abc")

let test_vault_out_of_reach_is_a_transport_failure env =
  with_service env [ Error "connection refused" ] @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "unreachable" (Error (Kms_error.Transport "connection refused"))
    (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc")

let test_an_answer_of_another_shape_is_malformed env =
  let is_malformed = function
    | Error (Kms_error.Malformed _) -> true
    | _ -> false
  in
  with_service env
    [
      answer 200 ~body:{|{"data":{}}|};
      answer 200 ~body:{|{"data":{"plaintext":"not base64!"}}|};
    ]
  @@ fun kms session _ ->
  Alcotest.(check bool)
    "no plaintext" true
    (is_malformed (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc"));
  Alcotest.(check bool)
    "not base64" true
    (is_malformed (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc"));
  Alcotest.(check bool)
    "a ciphertext that is not text is refused before Vault is asked" true
    (is_malformed (Vault.decrypt_dek kms session ~tenant_id:"t1" "\xff\xfe"))

let test_every_call_goes_through_the_session env =
  let seen = ref [] in
  let observer =
    {
      Rest_observer.none with
      on_request_ended =
        (fun request ~elapsed:_ ~failed ->
          seen := Printf.sprintf "%s %s %b" request.meth request.url failed :: !seen);
    }
  in
  with_service ~observer env [ answer 404; Error "connection refused" ]
  @@ fun kms session _ ->
  ignore (Vault.delete_kek kms session ~tenant_id:"t1");
  ignore (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc");
  Alcotest.(check (list string))
    "a refusal is an answer; not reaching Vault is a failure"
    [
      "GET http://vault:8200/v1/transit/keys/t1 false";
      "POST http://vault:8200/v1/transit/decrypt/t1 true";
    ]
    (List.rev !seen)

let test_another_mount_and_key_type env =
  let client =
    Scripted.create
      [ answer 200 ~body:(key_of_type ~latest:1 ~oldest:1 "chacha20-poly1305") ]
  in
  let session = Rest.create ~clock:(Eio.Stdenv.mono_clock env) client in
  let kms =
    Vault.create ~mount:"keys" ~key_type:"chacha20-poly1305" ~addr:"http://vault:8200"
      ~token:"t" ()
  in
  Alcotest.(check bool)
    "made" true
    (Vault.create_kek kms session ~tenant_id:"t1" Kek_options.default
    = Ok Key_version.first);
  Alcotest.(check (list string))
    "asked"
    [
      {|POST http://vault:8200/v1/keys/keys/t1 {"type":"chacha20-poly1305","derived":false}|};
    ]
    (List.map shown (Scripted.asked client))

(* The signing and MAC ports (ADR-0023). *)

let ed25519 = Signature_algorithm.Ed25519
let v = Key_version.of_int_exn
let a_signature = Alcotest.testable Signature.pp Signature.equal
let verified = Alcotest.(result unit error)
let zeroes version =
  Signature.make ~algorithm:ed25519 ~key_version:(v version) (String.make 64 '\000')
let bodies client = List.map shown (Scripted.asked client)

let test_a_signature_is_asked_in_jws_marshaling_and_parsed_from_its_versioned_text env =
  with_service env
    [
      answer 200 ~body:"{}";
      answer 200 ~body:{|{"data":{"key_version":3,"signature":"vault:v3:AQID"}}|};
    ]
  @@ fun kms session client ->
  Alcotest.(check (result a_signature error))
    "parsed"
    (Ok (Signature.make ~algorithm:ed25519 ~key_version:(v 3) "\001\002\003"))
    (Vault.sign kms session ~tenant_id:"t" ed25519 "hi");
  Alcotest.(check (list string))
    "the key is made of its type, the input signed in JWS marshaling"
    [
      {|POST http://vault:8200/v1/transit/keys/t.sign.ed25519 {"type":"ed25519"}|};
      {|POST http://vault:8200/v1/transit/sign/t.sign.ed25519 {"input":"aGk=","marshaling_algorithm":"jws"}|};
    ]
    (bodies client)

let test_an_ecdsa_signature_names_its_hash_and_marshaling_on_both_sides env =
  with_service env
    [
      answer 200 ~body:"{}";
      answer 200 ~body:{|{"data":{"signature":"vault:v1:AQID"}}|};
      answer 200 ~body:{|{"data":{"valid":true}}|};
    ]
  @@ fun kms session client ->
  let p256 = Signature_algorithm.Ecdsa_p256 in
  Alcotest.check verified "signed and verified" (Ok ())
    (Result.bind (Vault.sign kms session ~tenant_id:"t" p256 "hi") (fun signature ->
         Vault.verify kms session ~tenant_id:"t" "hi" signature));
  Alcotest.(check (list string))
    "asked"
    [
      {|POST http://vault:8200/v1/transit/keys/t.sign.ecdsa-p256 {"type":"ecdsa-p256"}|};
      {|POST http://vault:8200/v1/transit/sign/t.sign.ecdsa-p256 {"input":"aGk=","marshaling_algorithm":"jws","hash_algorithm":"sha2-256"}|};
      {|POST http://vault:8200/v1/transit/verify/t.sign.ecdsa-p256 {"input":"aGk=","signature":"vault:v1:AQID","marshaling_algorithm":"jws","hash_algorithm":"sha2-256"}|};
    ]
    (bodies client)

(* A refusal of a verification is read as a pinned signature's, of the
   versions that verify rather than sign (ADR-0035). *)
let test_a_refusal_of_verify_is_explained_by_one_read_of_the_key env =
  let signing ?signing_floor ~latest ~oldest () =
    key_of_type ?signing_floor ~latest ~oldest "ed25519"
  in
  let verify_under status ~version description =
    with_service env
      (answer status ~body:{|{"errors":["refused"]}|}
      :: Option.to_list (Option.map (fun body -> answer 200 ~body) description))
    @@ fun kms session _ -> Vault.verify kms session ~tenant_id:"t" "hi" (zeroes version)
  in
  Alcotest.check verified "the key is missing, on a 400"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    ( with_service env
        [
          answer 400 ~body:{|{"errors":["signature verification key not found"]}|};
          answer 404;
        ]
    @@ fun kms session _ -> Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1) );
  Alcotest.check verified "the key is missing, on a 403"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    ( with_service env
        [ answer 403 ~body:{|{"errors":["permission denied"]}|}; answer 404 ]
    @@ fun kms session _ -> Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1) );
  Alcotest.check verified "the version is retired" (Error (Kms_error.No_key_of_version 1))
    (verify_under 400 ~version:1 (Some (signing ~latest:3 ~oldest:2 ())));
  Alcotest.check verified "above the latest" (Error (Kms_error.No_key_of_version 4))
    (verify_under 400 ~version:4 (Some (signing ~latest:3 ~oldest:2 ())));
  (* A version below the signing floor serves a verification: the 400 is the
     request's own fault, not the version's. *)
  Alcotest.(check bool)
    "below the signing floor, the request's own" true
    (match
       verify_under 400 ~version:1
         (Some (signing ~signing_floor:2 ~latest:3 ~oldest:1 ()))
     with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false);
  Alcotest.(check bool)
    "an unexplained 400 is the request's own" true
    (match verify_under 400 ~version:1 (Some (signing ~latest:3 ~oldest:1 ())) with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false);
  Alcotest.check verified
    "a 403 with the key there: a holder that may sign holds no verify"
    (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "verify" }))
    (verify_under 403 ~version:1 (Some (signing ~latest:3 ~oldest:1 ())))

let test_a_signature_that_does_not_verify_is_said_so env =
  with_service env [ answer 200 ~body:{|{"data":{"valid":false}}|} ]
  @@ fun kms session _ ->
  Alcotest.check verified "invalid" (Error Kms_error.Invalid_signature)
    (Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1))

let test_a_mac_key_is_of_the_hmac_type_and_the_mac_is_parsed_from_its_versioned_text env =
  with_service env
    [ answer 200 ~body:"{}"; answer 200 ~body:{|{"data":{"hmac":"vault:v2:AQID"}}|} ]
  @@ fun kms session client ->
  let etag = Option.get (Result.to_option (Key_label.make "etag")) in
  Alcotest.(check (result (Alcotest.testable Mac.pp Mac.equal) error))
    "parsed"
    (Ok (Mac.make ~key_version:(v 2) "\001\002\003"))
    (Vault.mac kms session ~tenant_id:"t" etag "hi");
  Alcotest.(check (list string))
    "asked"
    [
      {|POST http://vault:8200/v1/transit/keys/t.mac.etag {"type":"hmac","key_size":32}|};
      {|POST http://vault:8200/v1/transit/hmac/t.mac.etag {"input":"aGk=","algorithm":"sha2-256"}|};
    ]
    (bodies client)

let test_a_tenant_id_that_is_not_a_key_part_is_refused_before_any_call env =
  with_service env [] @@ fun kms session client ->
  Alcotest.(check bool)
    "malformed" true
    (match Vault.sign kms session ~tenant_id:"a/b" ed25519 "hi" with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false);
  Alcotest.(check int) "no call" 0 (List.length (Scripted.asked client));
  Alcotest.(check (result string error))
    "a plain id makes a dotted name" (Ok "tenant-1.sign.ed25519")
    (Ascetic_kms_vault.Vault_transit.signing_key_name ~tenant_id:"tenant-1" ed25519)

(* The settings that serve the bus, and the reading of a refusal (ADR-0024). *)

let with_built ?derivation_context ?managed_keys env answers body =
  let client = Scripted.create answers in
  let session = Rest.create ~clock:(Eio.Stdenv.mono_clock env) client in
  let kms =
    Vault.create ?derivation_context ?managed_keys ~addr:"http://vault:8200" ~token:"t" ()
  in
  body kms session client

let test_managed_keys_are_made_by_nobody_here_and_a_missing_one_is_said_so env =
  with_built ~managed_keys:true env
    [
      answer 400 ~body:{|{"errors":["encryption key not found"]}|};
      answer 404 ~body:{|{"errors":[]}|};
    ]
  @@ fun kms session client ->
  Alcotest.(check (result unit error))
    "not found"
    (Error (Kms_error.Kek_not_found { tenant_id = "t"; key_version = None }))
    (Result.map ignore (Vault.generate_dek kms session ~tenant_id:"t"));
  Alcotest.(check (list string))
    "no key was made"
    [
      {|POST http://vault:8200/v1/transit/datakey/plaintext/t {"bits":256}|};
      "GET http://vault:8200/v1/transit/keys/t";
    ]
    (bodies client)

let test_the_context_travels_with_every_kek_operation_and_a_key_made_is_derived env =
  with_built ~derivation_context:"policy.lifecycle" env
    [
      answer 204;
      answer 200
        ~body:
          {|{"data":{"plaintext":"BwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwc=","ciphertext":"vault:v1:abc"}}|};
      answer 200
        ~body:{|{"data":{"plaintext":"BwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwc="}}|};
    ]
  @@ fun kms session client ->
  Alcotest.(check bool)
    "drawn and opened" true
    (Result.is_ok
       (Result.bind (Vault.generate_dek kms session ~tenant_id:"t") (fun (_, wrapped) ->
            Vault.decrypt_dek kms session ~tenant_id:"t" wrapped)));
  Alcotest.(check (list string))
    "the key made derived, the context with every operation"
    [
      {|POST http://vault:8200/v1/transit/keys/t {"type":"aes256-gcm96","derived":true}|};
      {|POST http://vault:8200/v1/transit/datakey/plaintext/t {"bits":256,"context":"cG9saWN5LmxpZmVjeWNsZQ=="}|};
      {|POST http://vault:8200/v1/transit/decrypt/t {"ciphertext":"vault:v1:abc","context":"cG9saWN5LmxpZmVjeWNsZQ=="}|};
    ]
    (bodies client)

let test_a_400_on_decrypt_is_explained_by_one_read_of_the_key env =
  let decrypt kms session = Vault.decrypt_dek kms session ~tenant_id:"t" "vault:v1:abc" in
  (* The key is gone. *)
  with_service env
    [ answer 400 ~body:{|{"errors":["encryption key not found"]}|}; answer 404 ]
  @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "gone"
    (Error (Kms_error.Kek_not_found { tenant_id = "t"; key_version = None }))
    (decrypt kms session);
  (* The version was retired. *)
  with_service env
    [
      answer 400 ~body:{|{"errors":["too old"]}|};
      answer 200 ~body:(key_description ~latest:3 ~oldest:2 ());
    ]
  @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "retired" (Error (Kms_error.No_key_of_version 1)) (decrypt kms session);
  (* The key is there and serves the version: the request's own fault, a
     wrong context, bytes changed, which no retry mends. *)
  with_service env
    [
      answer 400 ~body:{|{"errors":["cipher: message authentication failed"]}|};
      answer 200 ~body:(key_description ~derived:true ~latest:3 ~oldest:1 ());
    ]
  @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "the ciphertext's" (Error Kms_error.Decrypt) (decrypt kms session)

let test_a_403_on_decrypt_is_forbidden_when_the_key_is_there env =
  let decrypt kms session = Vault.decrypt_dek kms session ~tenant_id:"t" "vault:v1:abc" in
  let denied = answer 403 ~body:{|{"errors":["permission denied"]}|} in
  with_service env
    [ denied; answer 200 ~body:(key_description ~derived:true ~latest:1 ~oldest:1 ()) ]
  @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "forbidden"
    (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "decrypt" }))
    (decrypt kms session);
  (* The key is not there: not found, whatever the policy says. *)
  with_service env [ denied; answer 404 ] @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "not found"
    (Error (Kms_error.Kek_not_found { tenant_id = "t"; key_version = None }))
    (decrypt kms session);
  (* The read is refused too: the 403 as it came, for the operator. *)
  with_service env [ denied; denied ] @@ fun kms session _ ->
  Alcotest.(check (result a_key error))
    "as it came"
    (Error
       (Kms_error.Vault
          {
            status = 403;
            meth = "POST";
            path = "/decrypt/t";
            message = "permission denied";
          }))
    (decrypt kms session)

let test_custody_makes_a_derived_key_with_its_rotation_and_refuses_one_that_exists_otherwise
    env =
  let options = { Kek_options.derived = true; auto_rotate_period = Some 2_592_000.0 } in
  with_service env
    [
      answer 204;
      answer 200 ~body:(key_description ~derived:true ~latest:1 ~oldest:1 ());
      answer 204;
    ]
  @@ fun kms session client ->
  Alcotest.(check (result (Alcotest.testable Key_version.pp Key_version.equal) error))
    "made" (Ok Key_version.first)
    (Vault.create_kek kms session ~tenant_id:"t" options);
  Alcotest.(check (list string))
    "made derived with its period, read back, the period set"
    [
      {|POST http://vault:8200/v1/transit/keys/t {"type":"aes256-gcm96","derived":true,"auto_rotate_period":2592000}|};
      "GET http://vault:8200/v1/transit/keys/t";
      {|POST http://vault:8200/v1/transit/keys/t/config {"auto_rotate_period":2592000}|};
    ]
    (bodies client);
  (* Transit leaves a key that exists as it is and says nothing: the read
     tells that it is not derived, and that cannot be changed. *)
  with_service env
    [ answer 204; answer 200 ~body:(key_description ~latest:4 ~oldest:1 ()) ]
  @@ fun kms session _ ->
  Alcotest.(check bool)
    "a conflict" true
    (match Vault.create_kek kms session ~tenant_id:"t" options with
    | Error (Kms_error.Conflict { tenant_id = "t"; _ }) -> true
    | _ -> false)

(* A retirement reads the key once: a bound at or below the floor writes
   nothing, Transit would lower the floor when asked, one above the latest is
   refused, and a signing floor of the key's own below the bound is raised
   with it in the one write. *)
let test_a_retirement_reads_once_and_never_lowers_the_floor env =
  let retire kms session bound =
    Vault.retire_kek_versions_before kms session ~tenant_id:"t" (v bound)
  in
  List.iter
    (fun stale ->
      with_service env [ answer 200 ~body:(key_description ~latest:5 ~oldest:3 ()) ]
      @@ fun kms session client ->
      Alcotest.(check (result unit error))
        "nothing to do" (Ok ()) (retire kms session stale);
      Alcotest.(check (list string))
        "a read, and no write"
        [ "GET http://vault:8200/v1/transit/keys/t" ]
        (bodies client))
    [ 2; 3 ];
  ( with_service env
      [ answer 200 ~body:(key_description ~latest:5 ~oldest:3 ()); answer 204 ]
  @@ fun kms session client ->
    Alcotest.(check (result unit error)) "retired" (Ok ()) (retire kms session 4);
    Alcotest.(check (list string))
      "the floor raised"
      [
        "GET http://vault:8200/v1/transit/keys/t";
        {|POST http://vault:8200/v1/transit/keys/t/config {"min_decryption_version":4}|};
      ]
      (bodies client) );
  ( with_service env
      [
        answer 200 ~body:(key_description ~signing_floor:2 ~latest:5 ~oldest:3 ());
        answer 204;
      ]
  @@ fun kms session client ->
    Alcotest.(check (result unit error)) "retired" (Ok ()) (retire kms session 4);
    Alcotest.(check string)
      "the signing floor raised with it"
      {|POST http://vault:8200/v1/transit/keys/t/config {"min_decryption_version":4,"min_encryption_version":4}|}
      (List.nth (bodies client) 1) );
  ( with_service env [ answer 200 ~body:(key_description ~latest:5 ~oldest:3 ()) ]
  @@ fun kms session client ->
    Alcotest.(check (result unit error))
      "above the latest" (Error (Kms_error.No_key_of_version 6)) (retire kms session 6);
    Alcotest.(check int) "a read, and no write" 1 (List.length (bodies client)) );
  with_service env [ answer 404 ~body:{|{"errors":[]}|} ] @@ fun kms session _ ->
  Alcotest.(check (result unit error))
    "a tenant without a kek"
    (Error (Kms_error.Kek_not_found { tenant_id = "t"; key_version = None }))
    (retire kms session 2)

(* ---- Pinned signatures and the public keys (ADR-0035) ---- *)

let signed = Alcotest.(result a_signature error)
let a_version = Alcotest.testable Key_version.pp Key_version.equal
let hex text =
  String.init
    (String.length text / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * i) 2)))

let base64_url text =
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet text with
  | Ok bytes -> bytes
  | Error (`Msg reason) -> Alcotest.failf "not JWS base64: %s" reason

let pinned kms session ?(tenant_id = "t") version =
  Vault.sign_with_version kms session ~tenant_id ed25519 (v version) "hi"

let test_a_pinned_signature_asks_its_version_and_makes_no_key env =
  with_service env
    [ answer 200 ~body:{|{"data":{"key_version":2,"signature":"vault:v2:AQID"}}|} ]
  @@ fun kms session client ->
  Alcotest.check signed "signed at the version named"
    (Ok (Signature.make ~algorithm:ed25519 ~key_version:(v 2) "\001\002\003"))
    (pinned kms session 2);
  Alcotest.(check (list string))
    "the version in the body, and no POST keys"
    [
      {|POST http://vault:8200/v1/transit/sign/t.sign.ed25519 {"input":"aGk=","marshaling_algorithm":"jws","key_version":2}|};
    ]
    (bodies client);
  with_service env [] @@ fun kms session client ->
  Alcotest.(check bool)
    "version zero is refused before Vault is asked" true
    (match pinned kms session 0 with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false);
  Alcotest.(check int) "no call" 0 (List.length (bodies client))

let test_a_pinned_signature_by_another_version_than_asked_is_refused env =
  with_service env
    [ answer 200 ~body:{|{"data":{"key_version":3,"signature":"vault:v3:AQID"}}|} ]
  @@ fun kms session _ ->
  Alcotest.(check bool)
    "malformed" true
    (match pinned kms session 2 with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false)

(* The refusal of a pinned signature, by one read of the key: on a 400, a
   403 and a 500, never by Vault's text. *)
let test_a_refusal_of_a_pinned_signature_is_explained_by_one_read_of_the_key env =
  let signing ?signing_floor ~latest ~oldest () =
    key_of_type ?signing_floor ~latest ~oldest "ed25519"
  in
  let refused ~version status description =
    with_service env
      (answer status ~body:{|{"errors":["refused"]}|}
      :: Option.to_list (Option.map (fun body -> answer 200 ~body) description))
    @@ fun kms session _ -> pinned kms session version
  in
  Alcotest.check signed "no such key: Transit's 400"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    ( with_service env
        [ answer 400 ~body:{|{"errors":["signing key not found"]}|}; answer 404 ]
    @@ fun kms session _ -> pinned kms session 1 );
  Alcotest.check signed "above the latest: Transit's 500"
    (Error (Kms_error.No_key_of_version 3))
    (refused ~version:3 500 (Some (signing ~latest:2 ~oldest:1 ())));
  Alcotest.check signed "retired, the signing floor left at zero: a 500 too"
    (Error (Kms_error.No_key_of_version 1))
    (refused ~version:1 500 (Some (signing ~latest:3 ~oldest:2 ())));
  Alcotest.check signed "below the floor the key signs from, while it still verifies"
    (Error (Kms_error.No_key_of_version 2))
    (refused ~version:2 500 (Some (signing ~signing_floor:3 ~latest:3 ~oldest:1 ())));
  Alcotest.(check bool)
    "a 500 the read does not explain stays Vault's, of the moment" true
    (match refused ~version:2 500 (Some (signing ~latest:3 ~oldest:1 ())) with
    | Error (Kms_error.Vault { status = 500; _ }) -> true
    | _ -> false);
  Alcotest.(check bool)
    "a 400 the read does not explain is the request's own fault" true
    (match refused ~version:2 400 (Some (signing ~latest:3 ~oldest:1 ())) with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false);
  Alcotest.check signed "a 403 with the key there: no retry under this identity mends it"
    (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "sign" }))
    (refused ~version:2 403 (Some (signing ~latest:3 ~oldest:1 ())))

let test_a_403_on_the_read_too_and_any_other_status_are_left_as_they_came env =
  let denied = answer 403 ~body:{|{"errors":["permission denied"]}|} in
  ( with_service env [ denied; denied ] @@ fun kms session _ ->
    Alcotest.(check bool)
      "the 403 as it came" true
      (match pinned kms session 1 with
      | Error (Kms_error.Vault { status = 403; _ }) -> true
      | _ -> false) );
  with_service env [ answer 503 ~body:{|{"errors":["Vault is sealed"]}|} ]
  @@ fun kms session client ->
  Alcotest.(check bool)
    "a Vault unavailable" true
    (match pinned kms session 1 with
    | Error (Kms_error.Vault { status = 503; _ }) -> true
    | _ -> false);
  Alcotest.(check int) "not read again" 1 (List.length (bodies client))

(* A P-256 key Transit made, its public key as Transit published it, and a
   signature Transit made over [hello], verified by openssl independently of
   the code under test ([openssl dgst -sha256 -verify], the point it printed
   below); the same for Ed25519 ([openssl pkeyutl -verify -rawin]). *)
let p256_pem =
  "-----BEGIN PUBLIC KEY-----\n\
   MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE9WuXrrF/FhVQHsYXXTu0j6DbT5yz\n\
   9HglKvquboy6CfVASdzEUYdSCDev1ycaFZLIqJZMhEbJqhDNFObTr6vByg==\n\
   -----END PUBLIC KEY-----\n"

let p256_signature =
  "phAtQ5-wbgiRc1M0pycij5MXUyg6hI635jVUGDFKVgaaHU-8W9AS744l83OB9OVKU2UeuOPgZlj8gXqqOjHayQ"

let p256_point =
  "04f56b97aeb17f1615501ec6175d3bb48fa0db4f9cb3f478252afaae6e8cba09f54049dcc45187520837afd7271a1592c8a8964c8446c9aa10cd14e6d3afabc1ca"

let ed25519_public = "1sY8fbB3YT3rsSw7z4GJnLdGUPUlh8Yud6ocd687D/o="

let ed25519_signature =
  "LYNckKjG1Xm7xx-U9Rlm_zaBqXfCoINTI90TPfBvFOLOjaifTfzNK9NT-ThKj-6KMR3IQ2ySCW1fcv5HWXy1BA"

(* A key as Transit describes it with its versions' public keys. *)
let described kind ~oldest (keys : (string * Yojson.Safe.t) list) =
  Yojson.Safe.to_string
    (`Assoc
       [
         ( "data",
           `Assoc
             [
               ("type", `String kind);
               ("latest_version", `Int 3);
               ("min_decryption_version", `Int oldest);
               ("min_encryption_version", `Int 0);
               ("keys", `Assoc keys);
             ] );
       ])

let with_public_key text : Yojson.Safe.t = `Assoc [ ("public_key", `String text) ]

let test_the_public_keys_are_the_versions_served_read_into_the_model_s_form env =
  let p256 = Signature_algorithm.Ecdsa_p256 in
  let crlf = String.concat "\r\n" (String.split_on_char '\n' p256_pem) in
  ( with_service env
      [
        answer 200
          ~body:
            (* Version 1 is retired: Transit may still list it before a trim. *)
            (described "ecdsa-p256" ~oldest:2
               [
                 ("1", with_public_key p256_pem);
                 ("2", with_public_key p256_pem);
                 ("3", with_public_key crlf);
               ]);
      ]
  @@ fun kms session client ->
    let keyring = get "public_keys" (Vault.public_keys kms session ~tenant_id:"t" p256) in
    Alcotest.(check (list string))
      "one read"
      [ "GET http://vault:8200/v1/transit/keys/t.sign.ecdsa-p256" ]
      (bodies client);
    Alcotest.(check bool)
      "of the algorithm" true
      (Signature_algorithm.equal (Public_keyring.algorithm keyring) p256);
    Alcotest.(check (list int))
      "the versions served" [ 2; 3 ]
      (List.map Key_version.to_int (Public_keyring.versions keyring));
    match Public_keyring.get keyring (v 2) with
    | None -> Alcotest.fail "version two is not published"
    | Some key ->
        Alcotest.(check string)
          "the uncompressed point" (hex p256_point) (Public_key.to_string key);
        Alcotest.(check bool)
          "version three is the same key, read through other line ends" true
          (Option.equal Public_key.equal (Some key) (Public_keyring.get keyring (v 3)));
        let signature =
          Signature.make ~algorithm:p256 ~key_version:(v 2) (base64_url p256_signature)
        in
        Alcotest.check verified "verifies what Transit signed" (Ok ())
          (Public_key.verify key "hello" signature);
        Alcotest.check verified "and not another message"
          (Error Kms_error.Invalid_signature)
          (Public_key.verify key "hellp" signature) );
  with_service env
    [
      answer 200
        ~body:(described "ed25519" ~oldest:1 [ ("1", with_public_key ed25519_public) ]);
    ]
  @@ fun kms session _ ->
  let keyring =
    get "public_keys" (Vault.public_keys kms session ~tenant_id:"t" ed25519)
  in
  match Public_keyring.get keyring (v 1) with
  | None -> Alcotest.fail "version one is not published"
  | Some key ->
      Alcotest.check verified "verifies what Transit signed" (Ok ())
        (Public_key.verify key "hello"
           (Signature.make ~algorithm:ed25519 ~key_version:(v 1)
              (base64_url ed25519_signature)))

let test_the_public_keys_of_a_missing_key_of_another_type_or_another_armor_are_refused env
    =
  let p256 = Signature_algorithm.Ecdsa_p256 in
  ( with_service env [ answer 404 ~body:{|{"errors":[]}|} ] @@ fun kms session _ ->
    Alcotest.(check bool)
      "not found" true
      (match Vault.public_keys kms session ~tenant_id:"t" p256 with
      | Error (Kms_error.Signing_key_not_found { algorithm = Ecdsa_p256; _ }) -> true
      | _ -> false) );
  let certificate =
    Str.global_replace (Str.regexp_string "PUBLIC KEY") "CERTIFICATE" p256_pem
  in
  List.iter
    (fun (what, body) ->
      with_service env [ answer 200 ~body ] @@ fun kms session _ ->
      Alcotest.(check bool)
        what true
        (match Vault.public_keys kms session ~tenant_id:"t" p256 with
        | Error (Kms_error.Malformed _) -> true
        | _ -> false))
    [
      ( "another type",
        described "ecdsa-p384" ~oldest:1 [ ("1", with_public_key p256_pem) ] );
      ( "another armor",
        described "ecdsa-p256" ~oldest:1 [ ("1", with_public_key certificate) ] );
      ("no public key", described "ecdsa-p256" ~oldest:1 [ ("1", `Assoc []) ]);
    ]

(* ---- Custody of the signing and MAC keys (ADR-0036) ---- *)

let etag () = Option.get (Result.to_option (Key_label.make "etag"))

(* Under managed keys [sign] and [mac] ask Transit nothing but the
   operation, and a key that is not there is named by one read. *)
let test_managed_signing_and_macs_make_no_key_and_a_missing_one_is_said_so env =
  with_built ~managed_keys:true env
    [
      answer 400 ~body:{|{"errors":["signing key not found"]}|};
      answer 404 ~body:{|{"errors":[]}|};
      answer 400 ~body:{|{"errors":["encryption key not found"]}|};
      answer 404 ~body:{|{"errors":[]}|};
    ]
  @@ fun kms session client ->
  Alcotest.check signed "no signing key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    (Vault.sign kms session ~tenant_id:"t" ed25519 "hello");
  Alcotest.(check (result (Alcotest.testable Mac.pp Mac.equal) error))
    "no mac key"
    (Error (Kms_error.Mac_key_not_found { tenant_id = "t"; purpose = "etag" }))
    (Vault.mac kms session ~tenant_id:"t" (etag ()) "hello");
  Alcotest.(check (list string))
    "the operation, then one read; nothing made"
    [
      {|POST http://vault:8200/v1/transit/sign/t.sign.ed25519 {"input":"aGVsbG8=","marshaling_algorithm":"jws"}|};
      "GET http://vault:8200/v1/transit/keys/t.sign.ed25519";
      {|POST http://vault:8200/v1/transit/hmac/t.mac.etag {"input":"aGVsbG8=","algorithm":"sha2-256"}|};
      "GET http://vault:8200/v1/transit/keys/t.mac.etag";
    ]
    (bodies client)

let test_a_signing_rotation_never_makes_the_first_version env =
  ( with_service env
      [
        answer 400 ~body:{|{"errors":["key not found"]}|};
        answer 404 ~body:{|{"errors":[]}|};
      ]
  @@ fun kms session client ->
    Alcotest.(check (result a_version error))
      "not found"
      (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
      (Vault.rotate_signing_key kms session ~tenant_id:"t" ed25519);
    Alcotest.(check int)
      "rotated, read once, nothing made" 2
      (List.length (bodies client)) );
  with_service env [ answer 200 ~body:(key_of_type ~latest:3 ~oldest:1 "hmac") ]
  @@ fun kms session client ->
  Alcotest.(check (result a_version error))
    "the version is the answer's"
    (Ok (v 3))
    (Vault.rotate_mac_key kms session ~tenant_id:"t" (etag ()));
  Alcotest.(check (list string))
    "one call"
    [ "POST http://vault:8200/v1/transit/keys/t.mac.etag/rotate {}" ]
    (bodies client)

(* The making of a key reads the answer Transit gives, its description, and
   holds what is there to what was asked, with no read of its own. *)
let test_custody_makes_a_key_of_its_type_and_refuses_another env =
  let is_conflict = function
    | Error (Kms_error.Conflict { tenant_id = "t"; _ }) -> true
    | _ -> false
  in
  ( with_service env [ answer 200 ~body:(key_of_type ~latest:2 ~oldest:1 "ed25519") ]
  @@ fun kms session client ->
    Alcotest.(check (result a_version error))
      "the latest"
      (Ok (v 2))
      (Vault.create_signing_key kms session ~tenant_id:"t" ed25519 Key_options.default);
    Alcotest.(check (list string))
      "of its type"
      [ {|POST http://vault:8200/v1/transit/keys/t.sign.ed25519 {"type":"ed25519"}|} ]
      (bodies client) );
  ( with_service env [ answer 200 ~body:(key_of_type ~latest:1 ~oldest:1 "ecdsa-p256") ]
  @@ fun kms session _ ->
    Alcotest.(check bool)
      "a key of another type at the name" true
      (is_conflict
         (Vault.create_signing_key kms session ~tenant_id:"t" ed25519 Key_options.default))
  );
  ( with_service env
      [ answer 200 ~body:(key_of_type ~derived:true ~latest:1 ~oldest:1 "hmac") ]
  @@ fun kms session client ->
    Alcotest.(check bool)
      "a derived mac key" true
      (is_conflict
         (Vault.create_mac_key kms session ~tenant_id:"t" (etag ()) Key_options.default));
    Alcotest.(check (list string))
      "of its type and size"
      [
        {|POST http://vault:8200/v1/transit/keys/t.mac.etag {"type":"hmac","key_size":32}|};
      ]
      (bodies client) );
  (* A making refused under the identity is Forbidden, with no read. *)
  with_service env [ answer 403 ~body:{|{"errors":["permission denied"]}|} ]
  @@ fun kms session client ->
  Alcotest.(check (result a_version error))
    "forbidden"
    (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "create" }))
    (Vault.create_signing_key kms session ~tenant_id:"t" ed25519 Key_options.default);
  Alcotest.(check int) "no read" 1 (List.length (bodies client))

let test_a_custody_operation_refused_under_the_identity_is_forbidden env =
  let denied = answer 403 ~body:{|{"errors":["permission denied"]}|} in
  let there = answer 200 ~body:(key_of_type ~latest:2 ~oldest:1 "ed25519") in
  ( with_service env [ denied; there ] @@ fun kms session _ ->
    Alcotest.(check (result a_version error))
      "rotate"
      (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "rotate" }))
      (Vault.rotate_signing_key kms session ~tenant_id:"t" ed25519) );
  ( with_service env [ there; denied; there ] @@ fun kms session _ ->
    Alcotest.(check (result unit error))
      "retire"
      (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "retire" }))
      (Vault.retire_signing_versions_before kms session ~tenant_id:"t" ed25519 (v 2)) );
  with_service env [ there; denied ] @@ fun kms session _ ->
  Alcotest.(check (result unit error))
    "delete"
    (Error (Kms_error.Forbidden { tenant_id = "t"; operation = "delete" }))
    (Vault.delete_signing_key kms session ~tenant_id:"t" ed25519)

let () =
  Eio_main.run @@ fun env ->
  let case name test = Alcotest.test_case name `Quick (fun () -> test env) in
  Alcotest.run "Vault_transit"
    [
      ( "scripted",
        [
          case "a tenant id becomes a path segment"
            test_a_tenant_id_becomes_a_path_segment;
          case "a trailing slash in the address is dropped"
            test_a_trailing_slash_in_the_address_is_dropped;
          case "a dek is wrapped under the tenant's key"
            test_a_dek_is_wrapped_under_the_tenant_s_key;
          case "a dek is unwrapped" test_a_dek_is_unwrapped;
          case "a dek is drawn from vault" test_a_dek_is_drawn_from_vault;
          case "a rotation never makes the first version"
            test_a_rotation_never_makes_the_first_version;
          case "a rotation takes the version from the answer, or reads it"
            test_a_rotation_takes_the_version_from_the_answer_or_reads_it;
          case "a rewrap is vault's" test_a_rewrap_is_vault_s;
          case "deleting allows the deletion first"
            test_deleting_allows_the_deletion_first;
          case "nothing to delete is not an error" test_nothing_to_delete_is_not_an_error;
          case "vault's errors are joined" test_vault_s_errors_are_joined;
          case "a refusal without a body has no message"
            test_a_refusal_without_a_body_has_no_message;
          case "a missing key is reported as such" test_a_missing_key_is_reported_as_such;
          case "vault out of reach is a transport failure"
            test_vault_out_of_reach_is_a_transport_failure;
          case "an answer of another shape is malformed"
            test_an_answer_of_another_shape_is_malformed;
          case "every call goes through the session"
            test_every_call_goes_through_the_session;
          case "another mount and key type" test_another_mount_and_key_type;
        ] );
      ( "signing and MACs",
        [
          case "a signature is asked in JWS marshaling and parsed from its versioned text"
            test_a_signature_is_asked_in_jws_marshaling_and_parsed_from_its_versioned_text;
          case "an ecdsa signature names its hash and marshaling on both sides"
            test_an_ecdsa_signature_names_its_hash_and_marshaling_on_both_sides;
          case "a refusal of verify is explained by one read of the key"
            test_a_refusal_of_verify_is_explained_by_one_read_of_the_key;
          case "a signature that does not verify is said so"
            test_a_signature_that_does_not_verify_is_said_so;
          case
            "a mac key is of the hmac type and the mac is parsed from its versioned text"
            test_a_mac_key_is_of_the_hmac_type_and_the_mac_is_parsed_from_its_versioned_text;
          case "a tenant id that is not a key part is refused before any call"
            test_a_tenant_id_that_is_not_a_key_part_is_refused_before_any_call;
        ] );
      ( "the bus's key and custody",
        [
          case "managed keys are made by nobody here and a missing one is said so"
            test_managed_keys_are_made_by_nobody_here_and_a_missing_one_is_said_so;
          case "the context travels with every kek operation and a key made is derived"
            test_the_context_travels_with_every_kek_operation_and_a_key_made_is_derived;
          case "a 400 on decrypt is explained by one read of the key"
            test_a_400_on_decrypt_is_explained_by_one_read_of_the_key;
          case "a 403 on decrypt is forbidden when the key is there"
            test_a_403_on_decrypt_is_forbidden_when_the_key_is_there;
          case
            "custody makes a derived key with its rotation and refuses one that exists \
             otherwise"
            test_custody_makes_a_derived_key_with_its_rotation_and_refuses_one_that_exists_otherwise;
          case "a retirement reads once and never lowers the floor"
            test_a_retirement_reads_once_and_never_lowers_the_floor;
        ] );
      ( "pinned signatures and public keys",
        [
          case "a pinned signature asks its version and makes no key"
            test_a_pinned_signature_asks_its_version_and_makes_no_key;
          case "a pinned signature by another version than asked is refused"
            test_a_pinned_signature_by_another_version_than_asked_is_refused;
          case "a refusal of a pinned signature is explained by one read of the key"
            test_a_refusal_of_a_pinned_signature_is_explained_by_one_read_of_the_key;
          case "a 403 on the read too and any other status are left as they came"
            test_a_403_on_the_read_too_and_any_other_status_are_left_as_they_came;
          case "the public keys are the versions served, read into the model's form"
            test_the_public_keys_are_the_versions_served_read_into_the_model_s_form;
          case
            "the public keys of a missing key, of another type or another armor are \
             refused"
            test_the_public_keys_of_a_missing_key_of_another_type_or_another_armor_are_refused;
        ] );
      ( "custody of signing and mac keys",
        [
          case "managed signing and macs make no key and a missing one is said so"
            test_managed_signing_and_macs_make_no_key_and_a_missing_one_is_said_so;
          case "a signing rotation never makes the first version"
            test_a_signing_rotation_never_makes_the_first_version;
          case "custody makes a key of its type and refuses another"
            test_custody_makes_a_key_of_its_type_and_refuses_another;
          case "a custody operation refused under the identity is forbidden"
            test_a_custody_operation_refused_under_the_identity_is_forbidden;
        ] );
    ]
