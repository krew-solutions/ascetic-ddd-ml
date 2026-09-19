(** Integration tests for the Vault Transit adapter over [cohttp-eio]. They need a Vault
    dev server, named by [TEST_VAULT_ADDR] with the token in [TEST_VAULT_TOKEN], and are
    skipped without one; the [vault] service of [docker-compose.yml] is one. The tests
    enable the [transit] engine themselves. *)

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

let key () = get "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_key = Alcotest.testable Key.pp Key.equal
let unwrapped = Alcotest.(result a_key error)
let starts_with prefix text = String.starts_with ~prefix text

type fixture = {
  sessions : Cohttp_eio.Client.t Rest_pool.t;
  kms : Vault.t;
  addr : string;
  tenants : string * string;
}

(* Runs the body in one scope of a session. *)
let atomic_with f kms body =
  Rest_pool.session f.sessions ~lift (fun session ->
      Rest.atomic session ~lift (fun tx -> body tx kms))

let atomic f body = atomic_with f f.kms body

(* Tenants named after the test; their keys are deleted when the test is over. *)
let with_fixture ~name env ~addr ~token body =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  let f =
    {
      sessions = Rest_pool.create ~clock:(Eio.Stdenv.mono_clock env) client;
      kms = Vault.create ~addr ~token ();
      addr;
      tenants = (Printf.sprintf "test-%s-1" name, Printf.sprintf "test-%s-2" name);
    }
  in
  let cleanup () =
    List.iter
      (fun tenant_id ->
        get "cleanup" (atomic f (fun tx kms -> Vault.delete_kek kms tx ~tenant_id)))
      [ fst f.tenants; snd f.tenants ]
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

let is_refused_with status = function
  | Error (Kms_error.Vault { status = found; _ }) -> found = status
  | _ -> false

let test_a_dek_wrapped_after_a_rotation_unwraps f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let dek = key () in
         let* wrapped = Vault.encrypt_dek kms tx ~tenant_id dek in
         Alcotest.(check bool) "Vault's text" true (starts_with "vault:v1:" wrapped);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id wrapped);
         Ok ()))

let test_a_generated_dek_is_thirty_two_bytes_and_unwraps f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let* dek, wrapped = Vault.generate_dek kms tx ~tenant_id in
         Alcotest.(check int) "thirty-two bytes" 32 (Key.length dek);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id wrapped);
         Ok ()))

let test_each_rotation_is_the_next_version f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* first = Vault.rotate_kek kms tx ~tenant_id in
         let* second = Vault.rotate_kek kms tx ~tenant_id in
         Alcotest.(check (list int))
           "one, then two" [ 1; 2 ]
           (List.map Key_version.to_int [ first; second ]);
         Ok ()))

let test_what_an_earlier_version_wrapped_still_unwraps f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let dek = key () in
         let* wrapped_v1 = Vault.encrypt_dek kms tx ~tenant_id dek in
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id wrapped_v1);
         Ok ()))

let test_a_rewrap_moves_a_dek_to_the_current_version f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let dek = key () in
         let* wrapped_v1 = Vault.encrypt_dek kms tx ~tenant_id dek in
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let* wrapped_v2 = Vault.rewrap_dek kms tx ~tenant_id wrapped_v1 in
         Alcotest.(check bool) "another wrapped form" false (wrapped_v1 = wrapped_v2);
         Alcotest.(check bool) "was under one" true (starts_with "vault:v1:" wrapped_v1);
         Alcotest.(check bool) "is under two" true (starts_with "vault:v2:" wrapped_v2);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id wrapped_v2);
         Ok ()))

let test_deleting_the_key_shreds_what_it_wrapped f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id in
         let* wrapped = Vault.encrypt_dek kms tx ~tenant_id (key ()) in
         let* () = Vault.delete_kek kms tx ~tenant_id in
         Alcotest.(check bool)
           "refused with 400" true
           (is_refused_with 400 (Vault.decrypt_dek kms tx ~tenant_id wrapped));
         (* Deleting again is not an error. *)
         Vault.delete_kek kms tx ~tenant_id))

let test_the_first_contact_makes_the_key f =
  let tenant_id = fst f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let dek = key () in
         let* wrapped = Vault.encrypt_dek kms tx ~tenant_id dek in
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id wrapped);
         Ok ()))

let test_one_tenant_s_key_does_not_unwrap_another_s_dek f =
  let one, other = f.tenants in
  get "atomic"
    (atomic f (fun tx kms ->
         let* _ = Vault.rotate_kek kms tx ~tenant_id:one in
         let* _ = Vault.rotate_kek kms tx ~tenant_id:other in
         let dek = key () in
         let* wrapped = Vault.encrypt_dek kms tx ~tenant_id:one dek in
         Alcotest.check unwrapped "its own tenant" (Ok dek)
           (Vault.decrypt_dek kms tx ~tenant_id:one wrapped);
         Alcotest.(check bool)
           "another tenant is refused with 400" true
           (is_refused_with 400 (Vault.decrypt_dek kms tx ~tenant_id:other wrapped));
         Ok ()))

let test_a_wrong_token_is_refused_by_vault f =
  let kms = Vault.create ~addr:f.addr ~token:"not-the-token" () in
  let outcome =
    atomic_with f kms (fun tx kms -> Vault.rotate_kek kms tx ~tenant_id:(fst f.tenants))
  in
  Alcotest.(check bool) "refused with 403" true (is_refused_with 403 outcome)

let test_a_vault_out_of_reach_is_a_transport_failure f =
  let kms = Vault.create ~addr:"http://127.0.0.1:1" ~token:"t" () in
  let outcome =
    atomic_with f kms (fun tx kms -> Vault.rotate_kek kms tx ~tenant_id:(fst f.tenants))
  in
  Alcotest.(check bool)
    "unreachable" true
    (match outcome with Error (Kms_error.Transport _) -> true | _ -> false)

let () =
  match Sys.getenv_opt "TEST_VAULT_ADDR" with
  | None ->
      print_endline "[skip] vault integration tests: TEST_VAULT_ADDR is not set";
      exit 0
  | Some addr ->
      let token =
        Option.value (Sys.getenv_opt "TEST_VAULT_TOKEN") ~default:"test-root-token"
      in
      Eio_main.run @@ fun env ->
      enable_transit env ~addr ~token;
      let case name test =
        Alcotest.test_case name `Quick (fun () ->
            let slug = String.map (function ' ' | '\'' -> '_' | c -> c) name in
            with_fixture ~name:slug env ~addr ~token test)
      in
      Alcotest.run "Vault_transit over cohttp"
        [
          ( "integration",
            [
              case "a dek wrapped after a rotation unwraps"
                test_a_dek_wrapped_after_a_rotation_unwraps;
              case "a generated dek is thirty two bytes and unwraps"
                test_a_generated_dek_is_thirty_two_bytes_and_unwraps;
              case "each rotation is the next version"
                test_each_rotation_is_the_next_version;
              case "what an earlier version wrapped still unwraps"
                test_what_an_earlier_version_wrapped_still_unwraps;
              case "a rewrap moves a dek to the current version"
                test_a_rewrap_moves_a_dek_to_the_current_version;
              case "deleting the key shreds what it wrapped"
                test_deleting_the_key_shreds_what_it_wrapped;
              case "the first contact makes the key" test_the_first_contact_makes_the_key;
              case "one tenant's key does not unwrap another's dek"
                test_one_tenant_s_key_does_not_unwrap_another_s_dek;
              case "a wrong token is refused by vault"
                test_a_wrong_token_is_refused_by_vault;
              case "a vault out of reach is a transport failure"
                test_a_vault_out_of_reach_is_a_transport_failure;
            ] );
        ]
