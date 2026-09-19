(** The Vault Transit adapter against a scripted transport: what it asks of Vault, and how
    it reads the answers. No Vault needed; [test_vault] runs the same service against a
    dev server. *)

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
    (match request.body with Some body -> " " ^ Yojson.Safe.to_string body | None -> "")

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
      {|POST http://vault:8200/v1/transit/keys/a%2Fb {"type":"aes256-gcm96"}|};
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
      {|POST http://vault:8200/v1/transit/keys/t1 {"type":"aes256-gcm96"}|};
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
  Alcotest.(check (result a_key error))
    "refused"
    (Error
       (Kms_error.Vault
          { status = 400; meth = "POST"; path = "/decrypt/t1"; message = "one; two" }))
    (Vault.decrypt_dek kms session ~tenant_id:"t1" "vault:v1:abc")

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
  let is_malformed = function Error (Kms_error.Malformed _) -> true | _ -> false in
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
      {|POST http://vault:8200/v1/keys/keys/t1 {"type":"chacha20-poly1305"}|};
    ]
    (List.map shown (Scripted.asked client))

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
    ]
