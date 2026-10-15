(** The signing and MAC ports on Vault Transit, against a dev server with the [transit]
    engine enabled, as [test_vault.ml]; the same properties [test_pg_signing.ml] holds on
    PostgreSQL. Skipped without [TEST_VAULT_ADDR]. *)

open Ascetic_kms
module Transport = Ascetic_kms_vault_cohttp.Cohttp_transport
module Vault = Ascetic_kms_vault.Vault_transit.Make (Transport)
module Rest = Ascetic_session_rest.Rest_session
module Rest_pool = Ascetic_session_rest.Rest_session_pool

let lift error = Kms_error.Session error
let ( let* ) = Result.bind

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let ed25519 = Signature_algorithm.Ed25519
let p256 = Signature_algorithm.Ecdsa_p256
let v = Key_version.of_int_exn
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_version = Alcotest.testable Key_version.pp Key_version.equal
let a_mac = Alcotest.testable Mac.pp Mac.equal
let verified = Alcotest.(result unit error)
let zeroes version =
  Signature.make ~algorithm:ed25519 ~key_version:(v version) (String.make 64 '\000')
let etag () = get "label" (Key_label.make "etag")

type fixture = {
  sessions : Cohttp_eio.Client.t Rest_pool.t;
  kms : Vault.t;
  tenant : string;
  addr : string;
  token : string;
}

let atomic f body =
  Rest_pool.session f.sessions ~lift (fun session ->
      Rest.atomic session ~lift (fun tx -> body tx f.kms))

(* A tenant named after the test; its keys are deleted when the test is over. *)
let with_fixture ~name env ~addr ~token body =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  let f =
    {
      sessions = Rest_pool.create ~clock:(Eio.Stdenv.mono_clock env) client;
      kms = Vault.create ~addr ~token ();
      tenant = "test-sign-" ^ name;
      addr;
      token;
    }
  in
  let cleanup () =
    get "cleanup"
      (atomic f (fun tx kms ->
           let* () = Vault.delete_signing_key kms tx ~tenant_id:f.tenant ed25519 in
           let* () = Vault.delete_signing_key kms tx ~tenant_id:f.tenant p256 in
           Vault.delete_mac_key kms tx ~tenant_id:f.tenant (etag ())))
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> body f)

(* A dev server starts without the engine; one that has it says so with 400. *)
let enable_transit env ~addr ~token =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  match
    Transport.send client
      {
        meth = "POST";
        url = addr ^ "/v1/sys/mounts/transit";
        token;
        body = Some (`Assoc [ ("type", `String "transit") ]);
      }
  with
  | Ok { status = 204 | 200 | 400; _ } -> ()
  | Ok { status; _ } -> Alcotest.failf "enabling transit: status %d" status
  | Error reason -> Alcotest.failf "enabling transit: %s" reason

let sign f ?(algorithm = ed25519) message =
  get "sign"
    (atomic f (fun tx kms -> Vault.sign kms tx ~tenant_id:f.tenant algorithm message))

let verify f message signature =
  atomic f (fun tx kms -> Vault.verify kms tx ~tenant_id:f.tenant message signature)

let test_a_signature_verifies_and_a_changed_message_does_not f =
  let signature = sign f "hello" in
  Alcotest.(check bool)
    "of the algorithm" true
    (Signature_algorithm.equal (Signature.algorithm signature) ed25519);
  Alcotest.check a_version "of version one" (v 1) (Signature.key_version signature);
  Alcotest.(check int)
    "sixty-four bytes" 64
    (String.length (Signature.to_string signature));
  Alcotest.check verified "verifies" (Ok ()) (verify f "hello" signature);
  Alcotest.check verified "a changed message does not" (Error Kms_error.Invalid_signature)
    (verify f "hellp" signature)

let test_an_ecdsa_signature_is_raw_r_and_s f =
  let signature = sign f ~algorithm:p256 "hello" in
  Alcotest.(check int) "not DER" 64 (String.length (Signature.to_string signature));
  Alcotest.check verified "verifies" (Ok ()) (verify f "hello" signature)

let test_what_an_earlier_version_signed_still_verifies_after_a_rotation f =
  let first = sign f "hello" in
  Alcotest.check a_version "two" (v 2)
    (get "rotate"
       (atomic f (fun tx kms ->
            Vault.rotate_signing_key kms tx ~tenant_id:f.tenant ed25519)));
  let second = sign f "hello" in
  Alcotest.check a_version "signed by two" (v 2) (Signature.key_version second);
  Alcotest.check verified "the first still verifies" (Ok ()) (verify f "hello" first)

let test_a_signature_by_a_key_the_tenant_has_not_got_is_told_apart_from_a_bad_one f =
  Alcotest.check verified "no key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = f.tenant; algorithm = ed25519 }))
    (verify f "hello" (zeroes 1));
  (* A version the key has not got, once the key exists. *)
  ignore (sign f "hello");
  Alcotest.check verified "no such version" (Error (Kms_error.No_key_of_version 9))
    (verify f "hello" (zeroes 9))

let test_a_mac_is_the_same_twice_and_another_message_s_differs f =
  let mac message =
    get "mac"
      (atomic f (fun tx kms -> Vault.mac kms tx ~tenant_id:f.tenant (etag ()) message))
  in
  let first = mac "hello" in
  Alcotest.check a_mac "the same again" first (mac "hello");
  Alcotest.check a_version "of version one" (v 1) (Mac.key_version first);
  Alcotest.(check int) "thirty-two bytes" 32 (String.length (Mac.to_string first));
  Alcotest.(check bool) "another message" false (Mac.equal first (mac "hellp"))

let test_deleting_what_is_not_there_is_nothing _f = ()

(* The public key of the tenant's key of the algorithm, as Transit publishes
   it, read as the model's: the raw bytes in base64 for Ed25519, a PEM
   SubjectPublicKeyInfo for P-256. *)
let public_key_transit_publishes f algorithm version =
  let name =
    get "name"
      (Ascetic_kms_vault.Vault_transit.signing_key_name ~tenant_id:f.tenant algorithm)
  in
  let text =
    match
      Transport.send (Rest_pool.client f.sessions)
        {
          meth = "GET";
          url = Printf.sprintf "%s/v1/transit/keys/%s" f.addr name;
          token = f.token;
          body = None;
        }
    with
    | Ok { status = 200; body = Some json } ->
        Yojson.Safe.Util.(
          json |> member "data" |> member "keys"
          |> member (string_of_int (Key_version.to_int version))
          |> member "public_key" |> to_string)
    | Ok { status; _ } -> Alcotest.failf "reading the key: status %d" status
    | Error reason -> Alcotest.failf "reading the key: %s" reason
  in
  let bytes =
    match algorithm with
    | Signature_algorithm.Ed25519 -> (
        match Base64.decode text with
        | Ok bytes -> bytes
        | Error (`Msg reason) -> Alcotest.failf "the public key is not base64: %s" reason)
    | Signature_algorithm.Ecdsa_p256 -> (
        match X509.Public_key.decode_pem text with
        | Ok (`P256 key) -> Mirage_crypto_ec.P256.Dsa.pub_to_octets key
        | Ok _ -> Alcotest.fail "the public key is not a P-256 point"
        | Error (`Msg reason) -> Alcotest.failf "the public key is not PEM: %s" reason)
  in
  get "public key" (Public_key.make algorithm bytes)

(* A signature Vault made, in the byte form the port fixes, verifies in this
   process under the public key Transit publishes, read as the model's: the
   two adapters' forms are one, for both algorithms. *)
let test_a_signature_vault_made_verifies_under_the_public_key_it_publishes f =
  List.iter
    (fun algorithm ->
      let name = Signature_algorithm.to_string algorithm in
      let signature = sign f ~algorithm "hello" in
      let public =
        public_key_transit_publishes f algorithm (Signature.key_version signature)
      in
      Alcotest.check verified
        (name ^ ": verifies in this process")
        (Ok ())
        (Public_key.verify public "hello" signature);
      Alcotest.check verified
        (name ^ ": another message does not")
        (Error Kms_error.Invalid_signature)
        (Public_key.verify public "hellp" signature))
    Signature_algorithm.all

let () =
  match Sys.getenv_opt "TEST_VAULT_ADDR" with
  | None ->
      print_endline "[skip] vault signing integration tests: TEST_VAULT_ADDR is not set";
      exit 0
  | Some addr ->
      let token =
        Option.value (Sys.getenv_opt "TEST_VAULT_TOKEN") ~default:"test-root-token"
      in
      Eio_main.run @@ fun env ->
      enable_transit env ~addr ~token;
      let case name test =
        Alcotest.test_case name `Quick (fun () ->
            let slug =
              String.map
                (function
                  | ' ' | '\'' -> '-'
                  | c -> c)
                name
            in
            with_fixture ~name:slug env ~addr ~token test)
      in
      Alcotest.run "Vault_transit signing over cohttp"
        [
          ( "integration",
            [
              case "a signature verifies and a changed message does not"
                test_a_signature_verifies_and_a_changed_message_does_not;
              case "an ecdsa signature is raw r and s"
                test_an_ecdsa_signature_is_raw_r_and_s;
              case "what an earlier version signed still verifies after a rotation"
                test_what_an_earlier_version_signed_still_verifies_after_a_rotation;
              case
                "a signature by a key the tenant has not got is told apart from a bad one"
                test_a_signature_by_a_key_the_tenant_has_not_got_is_told_apart_from_a_bad_one;
              case "a mac is the same twice and another message's differs"
                test_a_mac_is_the_same_twice_and_another_message_s_differs;
              case "deleting what is not there is nothing"
                test_deleting_what_is_not_there_is_nothing;
              case "a signature vault made verifies under the public key it publishes"
                test_a_signature_vault_made_verifies_under_the_public_key_it_publishes;
            ] );
        ]
