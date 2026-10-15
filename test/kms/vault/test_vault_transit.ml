(** The Vault Transit adapter against a scripted transport: what it asks of Vault, and how
    it reads the answers, including how it tells Transit's [400]s and [403]s apart by one
    read of the key. No Vault needed; [test_vault] and [test_vault_signing] run the same
    service against a dev server. *)

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

let test_the_first_rotation_makes_the_key env =
  with_service env [ answer 404; answer 204 ] @@ fun kms session client ->
  Alcotest.(check bool)
    "version one" true
    (Vault.rotate_kek kms session ~tenant_id:"t1" = Ok Key_version.first);
  Alcotest.(check (list string))
    "asked"
    [
      "GET http://vault:8200/v1/transit/keys/t1";
      {|POST http://vault:8200/v1/transit/keys/t1 {"type":"aes256-gcm96","derived":false}|};
    ]
    (List.map shown (Scripted.asked client))

let test_a_later_rotation_reads_the_version_back env =
  with_service env
    [
      answer 200 ~body:{|{"data":{"latest_version":2}}|};
      answer 204;
      answer 200 ~body:{|{"data":{"latest_version":3}}|};
    ]
  @@ fun kms session client ->
  Alcotest.(check bool)
    "version three" true
    (Vault.rotate_kek kms session ~tenant_id:"t1" = Ok (Key_version.of_int_exn 3));
  Alcotest.(check (list string))
    "asked"
    [
      "GET http://vault:8200/v1/transit/keys/t1";
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
  let client = Scripted.create [ answer 404; answer 204 ] in
  let session = Rest.create ~clock:(Eio.Stdenv.mono_clock env) client in
  let kms =
    Vault.create ~mount:"keys" ~key_type:"chacha20-poly1305" ~addr:"http://vault:8200"
      ~token:"t" ()
  in
  ignore (Vault.rotate_kek kms session ~tenant_id:"t1");
  Alcotest.(check (list string))
    "asked"
    [
      "GET http://vault:8200/v1/keys/keys/t1";
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

let test_a_400_on_verify_is_explained_by_one_read_of_the_key env =
  (* No such key. *)
  with_service env
    [
      answer 400 ~body:{|{"errors":["signature verification key not found"]}|};
      answer 404 ~body:{|{"errors":[]}|};
    ]
  @@ fun kms session _ ->
  Alcotest.check verified "the key is missing"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    (Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1));
  (* A version the key no longer serves. *)
  with_service env
    [
      answer 400
        ~body:
          {|{"errors":["ciphertext or signature version is disallowed by policy (too old)"]}|};
      answer 200 ~body:{|{"data":{"latest_version":3,"min_decryption_version":2}}|};
    ]
  @@ fun kms session _ ->
  Alcotest.check verified "the version is retired" (Error (Kms_error.No_key_of_version 1))
    (Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1));
  (* Any other 400 stays Vault's. *)
  with_service env
    [
      answer 400 ~body:{|{"errors":["something else"]}|};
      answer 200 ~body:{|{"data":{"latest_version":3,"min_decryption_version":1}}|};
    ]
  @@ fun kms session _ ->
  Alcotest.check verified "left as it was"
    (Error
       (Kms_error.Vault
          {
            status = 400;
            meth = "POST";
            path = "/verify/t.sign.ed25519";
            message = "something else";
          }))
    (Vault.verify kms session ~tenant_id:"t" "hi" (zeroes 1))

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

let key_description ?(derived = false) ~latest ~oldest () =
  Printf.sprintf
    {|{"data":{"latest_version":%d,"min_decryption_version":%d,"derived":%b}}|} latest
    oldest derived

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

let test_retiring_versions_raises_the_key_s_minimum_decryption_version env =
  with_service env [ answer 204 ] @@ fun kms session client ->
  Alcotest.(check (result unit error))
    "retired" (Ok ())
    (Vault.retire_kek_versions_before kms session ~tenant_id:"t" (v 3));
  Alcotest.(check (list string))
    "asked"
    [ {|POST http://vault:8200/v1/transit/keys/t/config {"min_decryption_version":3}|} ]
    (bodies client)

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
          case "the first rotation makes the key" test_the_first_rotation_makes_the_key;
          case "a later rotation reads the version back"
            test_a_later_rotation_reads_the_version_back;
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
          case "a 400 on verify is explained by one read of the key"
            test_a_400_on_verify_is_explained_by_one_read_of_the_key;
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
          case "retiring versions raises the key's minimum decryption version"
            test_retiring_versions_raises_the_key_s_minimum_decryption_version;
        ] );
    ]
