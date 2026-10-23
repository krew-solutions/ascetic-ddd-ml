(** The signing and MAC ports on the PostgreSQL adapter, against a live database, as
    [test_pg.ml]; the same properties [test_vault_signing.ml] holds on Vault, so that the
    two adapters do not drift apart: the custody rules of ADR-0036 in the one body every
    adapter runs, pinned signatures and the keyring (ADR-0035). Skipped without
    [TEST_DATABASE_URL]. *)

open Ascetic_kms
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Rules = Ascetic_kms_test_custody.Custody_rules.Make (Pg_kms)
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

let lift error = Kms_error.Session error
let ( let* ) = Result.bind

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let master_key () = get "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)
let ed25519 = Signature_algorithm.Ed25519
let p256 = Signature_algorithm.Ecdsa_p256
let v = Key_version.of_int_exn
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_version = Alcotest.testable Key_version.pp Key_version.equal
let a_signature = Alcotest.testable Signature.pp Signature.equal
let a_mac = Alcotest.testable Mac.pp Mac.equal
let verified = Alcotest.(result unit error)
let zeroes ?(algorithm = ed25519) version =
  Signature.make ~algorithm ~key_version:(v version) (String.make 64 '\000')

(* ------------------------------------------------------------------------ *)
(* The fixture                                                                *)

type fixture = {
  sessions : Pool.t;
  kms : Pg_kms.t;
  table : string;
  master : Key.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

let master_of f = f.master

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let with_session f body = get "session" (Pool.session f.sessions ~lift body)
let atomic f body =
  Pool.session f.sessions ~lift (fun session -> Session.atomic session ~lift body)

let service ?managed_keys ~table master_key =
  let id = Identifier.of_string_exn in
  Pg_kms.create ~table:(id table)
    ~signing_table:(id (table ^ "_sign"))
    ~mac_table:(id (table ^ "_mac"))
    ?managed_keys master_key

(* Tables named after the test, dropped and made afresh, so tests start
   empty; a fresh master key. *)
let with_fixture ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let sessions =
    match
      Caqti_eio_unix.connect_pool
        ~pool_config:(Caqti_pool_config.create ~max_size:8 ())
        ~sw
        ~stdenv:(env :> Caqti_eio.stdenv)
        uri
    with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let table = "ks_" ^ name in
  let master = master_key () in
  let kms = service ~table master in
  let f =
    {
      sessions;
      kms;
      table;
      master;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
    }
  in
  with_session f (fun session ->
      exec session
        (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_sign, %s_mac" table table table);
      Pg_kms.setup kms session);
  body f

let signing_versions_of f tenant_id =
  with_session f (fun session ->
      let module C = (val Session.connection session) in
      let open Caqti_request.Infix in
      let request =
        (Caqti_type.string ->* Caqti_type.int)
          ~oneshot:true
          (Printf.sprintf
             "SELECT key_version FROM %s_sign WHERE tenant_id = $1 ORDER BY key_version"
             f.table)
      in
      match C.collect_list request tenant_id with
      | Ok versions -> Ok versions
      | Error err -> Alcotest.failf "signing_versions_of: %a" Caqti_error.pp err)

(* The fixture's service built with managed keys, over the same tables. *)
let managed f = service ~managed_keys:true ~table:f.table (master_of f)

let sign f tx ?(algorithm = ed25519) tenant_id message =
  Pg_kms.sign f.kms tx ~tenant_id algorithm message

let verify f tx tenant_id message signature =
  Pg_kms.verify f.kms tx ~tenant_id message signature

let sign_with_version f tx ?(algorithm = ed25519) tenant_id version message =
  Pg_kms.sign_with_version f.kms tx ~tenant_id algorithm (v version) message

let versions_published f tx ?(algorithm = ed25519) tenant_id =
  Result.map
    (fun keyring -> List.map Key_version.to_int (Public_keyring.versions keyring))
    (Pg_kms.public_keys f.kms tx ~tenant_id algorithm)

let rotate f tx ?(algorithm = ed25519) tenant_id =
  Pg_kms.rotate_signing_key f.kms tx ~tenant_id algorithm

(* ------------------------------------------------------------------------ *)
(* The tests                                                                  *)

let test_a_signature_verifies_and_a_changed_message_does_not env uri =
  with_fixture ~name:"roundtrip" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* signature = sign f tx "1" "hello" in
         Alcotest.(check bool)
           "of the algorithm" true
           (Signature_algorithm.equal (Signature.algorithm signature) ed25519);
         Alcotest.check a_version "of version one" (v 1) (Signature.key_version signature);
         Alcotest.(check int)
           "sixty-four bytes" 64
           (String.length (Signature.to_string signature));
         Alcotest.check verified "verifies" (Ok ()) (verify f tx "1" "hello" signature);
         Alcotest.check verified "a changed message does not"
           (Error Kms_error.Invalid_signature)
           (verify f tx "1" "hellp" signature);
         Ok ()))

let test_an_ecdsa_signature_is_raw_r_and_s env uri =
  with_fixture ~name:"p256" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* signature = sign f tx ~algorithm:p256 "1" "hello" in
         Alcotest.(check int) "not DER" 64 (String.length (Signature.to_string signature));
         verify f tx "1" "hello" signature))

let test_what_an_earlier_version_signed_still_verifies_after_a_rotation env uri =
  with_fixture ~name:"rotation" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* first = sign f tx "1" "hello" in
         let* rotated = rotate f tx "1" in
         Alcotest.check a_version "two" (v 2) rotated;
         let* second = sign f tx "1" "hello" in
         Alcotest.check a_version "signed by two" (v 2) (Signature.key_version second);
         Alcotest.(check bool)
           "another signature" false
           (String.equal (Signature.to_string first) (Signature.to_string second));
         Alcotest.check verified "the first still verifies" (Ok ())
           (verify f tx "1" "hello" first);
         verify f tx "1" "hello" second));
  Alcotest.(check (list int)) "both rows" [ 1; 2 ] (signing_versions_of f "1")

let test_a_signature_by_a_key_the_tenant_has_not_got_is_told_apart_from_a_bad_one env uri
    =
  with_fixture ~name:"missing" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         Alcotest.check verified "no key"
           (Error
              (Kms_error.Signing_key_not_found { tenant_id = "1"; algorithm = ed25519 }))
           (verify f tx "1" "hello" (zeroes 1));
         let* _ = sign f tx "1" "hello" in
         Alcotest.check verified "no such version" (Error (Kms_error.No_key_of_version 9))
           (verify f tx "1" "hello" (zeroes 9));
         (* A key of another algorithm is another key. *)
         Alcotest.check verified "no key of the other algorithm"
           (Error (Kms_error.Signing_key_not_found { tenant_id = "1"; algorithm = p256 }))
           (verify f tx "1" "hello" (zeroes ~algorithm:p256 1));
         Ok ()))

let test_one_tenant_s_key_does_not_verify_another_s_signature env uri =
  with_fixture ~name:"tenants" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* signature = sign f tx "1" "hello" in
         let* _ = sign f tx "2" "hello" in
         Alcotest.check verified "the other tenant's key"
           (Error Kms_error.Invalid_signature)
           (verify f tx "2" "hello" signature);
         Ok ()))

let test_a_key_wrapped_under_one_master_key_does_not_open_under_another env uri =
  with_fixture ~name:"master" env uri @@ fun f ->
  get "atomic" (atomic f (fun tx -> Result.map ignore (sign f tx "1" "hello")));
  let other = service ~table:f.table (master_key ()) in
  Alcotest.(check (result unit error))
    "refused" (Error Kms_error.Decrypt)
    (Result.map ignore
       (atomic f (fun tx -> Pg_kms.sign other tx ~tenant_id:"1" ed25519 "hello")))

let test_deleting_the_key_ends_what_it_signed env uri =
  with_fixture ~name:"delete" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* signature = sign f tx "1" "hello" in
         let* () = Pg_kms.delete_signing_key f.kms tx ~tenant_id:"1" ed25519 in
         Alcotest.check verified "gone with the key"
           (Error
              (Kms_error.Signing_key_not_found { tenant_id = "1"; algorithm = ed25519 }))
           (verify f tx "1" "hello" signature);
         (* Nothing to delete is nothing. *)
         let* () = Pg_kms.delete_signing_key f.kms tx ~tenant_id:"1" ed25519 in
         Pg_kms.delete_mac_key f.kms tx ~tenant_id:"1"
           (get "label" (Key_label.make "etag"))))

let test_a_mac_is_the_same_twice_and_another_s_differs env uri =
  with_fixture ~name:"mac" env uri @@ fun f ->
  let etag = get "label" (Key_label.make "etag") in
  let other = get "label" (Key_label.make "other") in
  get "atomic"
    (atomic f (fun tx ->
         let mac tenant_id purpose message =
           Pg_kms.mac f.kms tx ~tenant_id purpose message
         in
         let* first = mac "1" etag "hello" in
         let* again = mac "1" etag "hello" in
         Alcotest.check a_mac "the same again" first again;
         Alcotest.check a_version "of version one" (v 1) (Mac.key_version first);
         Alcotest.(check int) "thirty-two bytes" 32 (String.length (Mac.to_string first));
         let* by_message = mac "1" etag "hellp" in
         let* by_tenant = mac "2" etag "hello" in
         let* by_purpose = mac "1" other "hello" in
         List.iter
           (fun (what, mac) -> Alcotest.(check bool) what false (Mac.equal first mac))
           [
             ("another message", by_message);
             ("another tenant", by_tenant);
             ("another purpose", by_purpose);
           ];
         let* () = Pg_kms.delete_mac_key f.kms tx ~tenant_id:"1" etag in
         let* fresh = mac "1" etag "hello" in
         Alcotest.(check bool) "a new key after deletion" false (Mac.equal first fresh);
         Ok ()))

(* Two first signings at once: the first makes the key and holds its
   transaction open; the second waits on the keyring's lock, and, once the
   first commits, signs with the same key. *)
let test_two_first_signings_at_once_share_one_key env uri =
  with_fixture ~name:"first_at_once" env uri @@ fun f ->
  let made, set_made = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  let first, second =
    Eio.Switch.run @@ fun sw ->
    let first =
      Eio.Fiber.fork_promise ~sw (fun () ->
          atomic f (fun tx ->
              let* signature = sign f tx "1" "hello" in
              Eio.Promise.resolve set_made ();
              Eio.Promise.await release;
              Ok signature))
    in
    Eio.Promise.await made;
    let second =
      Eio.Fiber.fork_promise ~sw (fun () -> atomic f (fun tx -> sign f tx "1" "hello"))
    in
    Eio.Time.Mono.sleep f.clock 0.3;
    Alcotest.(check bool) "the second waits" false (Eio.Promise.is_resolved second);
    Eio.Promise.resolve set_release ();
    ( get "first" (Eio.Promise.await_exn first),
      get "second" (Eio.Promise.await_exn second) )
  in
  Alcotest.check a_version "the first under version one" (v 1)
    (Signature.key_version first);
  Alcotest.check a_version "the second too" (v 1) (Signature.key_version second);
  Alcotest.check a_signature "one key, one deterministic signature" first second;
  Alcotest.(check (list int)) "one row" [ 1 ] (signing_versions_of f "1")

(* ------------------------------------------------------------------------ *)
(* Custody (ADR-0036)                                                         *)

(* The scope the custody rules run in: one transaction per call, over the
   fixture's service built with managed keys. *)
let scope f kms = { Rules.atomic = (fun body -> atomic f (fun tx -> body tx kms)) }

let test_the_custody_of_signing_keys_keeps_the_rules env uri =
  with_fixture ~name:"custody_signing" env uri @@ fun f ->
  Rules.signing_custody (scope f (managed f)) ~tenant:"1"

let test_the_custody_of_mac_keys_keeps_the_rules env uri =
  with_fixture ~name:"custody_mac" env uri @@ fun f ->
  Rules.mac_custody (scope f (managed f)) ~tenant:"1"

(* Two makings at once: the second waits on the keyring's lock, and both
   answer the one version made. *)
let test_two_makings_at_once_make_one_key env uri =
  with_fixture ~name:"custody_race" env uri @@ fun f ->
  let kms = managed f in
  let make () =
    atomic f (fun tx ->
        Pg_kms.create_signing_key kms tx ~tenant_id:"1" ed25519 Key_options.default)
  in
  let a, b = Eio.Fiber.pair make make in
  Alcotest.(check (list int))
    "one version, answered twice" [ 1; 1 ]
    (List.map Key_version.to_int [ get "the first making" a; get "the second making" b ]);
  Alcotest.(check (list int)) "one row" [ 1 ] (signing_versions_of f "1")

let test_a_rotation_period_is_refused_by_a_table env uri =
  with_fixture ~name:"custody_period" env uri @@ fun f ->
  let options = { Key_options.auto_rotate_period = Some 3600.0 } in
  let is_unsupported = function
    | Error (Kms_error.Unsupported _) -> true
    | _ -> false
  in
  Alcotest.(check bool)
    "a signing key" true
    (is_unsupported
       (atomic f (fun tx ->
            Pg_kms.create_signing_key f.kms tx ~tenant_id:"1" ed25519 options)));
  Alcotest.(check bool)
    "a mac key" true
    (is_unsupported
       (atomic f (fun tx ->
            Pg_kms.create_mac_key f.kms tx ~tenant_id:"1"
              (get "label" (Key_label.make "etag"))
              options)));
  Alcotest.(check (list int)) "nothing made" [] (signing_versions_of f "1")

(* ------------------------------------------------------------------------ *)
(* Pinned signatures and the keyring (ADR-0035)                               *)

let test_the_earlier_version_signs_when_it_is_named env uri =
  with_fixture ~name:"pinned" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* first = sign f tx "1" "hello" in
         let* _ = rotate f tx "1" in
         let* pinned = sign_with_version f tx "1" 1 "hello" in
         Alcotest.check a_signature "the same signature, Ed25519 being deterministic"
           first pinned;
         let* latest = sign f tx "1" "hello" in
         Alcotest.check a_version "the latest signs otherwise" (v 2)
           (Signature.key_version latest);
         Ok ()))

let test_a_pinned_signature_makes_nothing env uri =
  with_fixture ~name:"pinned_nothing" env uri @@ fun f ->
  Alcotest.(check (result a_signature error))
    "no key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "1"; algorithm = p256 }))
    (atomic f (fun tx -> sign_with_version f tx ~algorithm:p256 "1" 1 "hello"));
  Alcotest.(check (list int)) "none made" [] (signing_versions_of f "1");
  ignore (get "sign" (atomic f (fun tx -> sign f tx ~algorithm:p256 "1" "hello")));
  (* A refusal rolls back what its transaction did; the key was made in one
     of its own, and the refusal adds nothing to it. *)
  Alcotest.(check (result a_signature error))
    "no such version" (Error (Kms_error.No_key_of_version 5))
    (atomic f (fun tx -> sign_with_version f tx ~algorithm:p256 "1" 5 "hello"));
  Alcotest.(check (list int)) "the one version" [ 1 ] (signing_versions_of f "1");
  Alcotest.(check bool)
    "version zero is no version, and is refused before the table is asked" true
    (match atomic f (fun tx -> sign_with_version f tx ~algorithm:p256 "1" 0 "hello") with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false)

(* The keyring is the versions there are, the public half of each derived
   from its private half; a version an operator deleted neither signs nor
   verifies. *)
let test_the_keyring_is_the_versions_there_are env uri =
  with_fixture ~name:"keyring" env uri @@ fun f ->
  let first =
    get "atomic"
      (atomic f (fun tx ->
           let* first = sign f tx ~algorithm:p256 "1" "hello" in
           let* _ = rotate f tx ~algorithm:p256 "1" in
           let* _ = rotate f tx ~algorithm:p256 "1" in
           let* keyring = Pg_kms.public_keys f.kms tx ~tenant_id:"1" p256 in
           Alcotest.(check (list int))
             "three versions" [ 1; 2; 3 ]
             (List.map Key_version.to_int (Public_keyring.versions keyring));
           (match Public_keyring.get keyring (v 1) with
           | Some key ->
               Alcotest.check verified "version one verifies what it signed" (Ok ())
                 (Public_key.verify key "hello" first)
           | None -> Alcotest.fail "version one is not published");
           Ok first))
  in
  with_session f (fun session ->
      exec session
        (Printf.sprintf "DELETE FROM %s_sign WHERE tenant_id = '1' AND key_version < 2"
           f.table);
      Ok ());
  get "atomic"
    (atomic f (fun tx ->
         let* versions = versions_published f tx ~algorithm:p256 "1" in
         Alcotest.(check (list int)) "two left" [ 2; 3 ] versions;
         Alcotest.(check (result a_signature error))
           "the deleted version signs nothing" (Error (Kms_error.No_key_of_version 1))
           (sign_with_version f tx ~algorithm:p256 "1" 1 "hello");
         Alcotest.check verified "nor verifies" (Error (Kms_error.No_key_of_version 1))
           (verify f tx "1" "hello" first);
         Ok ()))

(* The keyring and a verification unwrap the private halves: under another
   master key they fail as a signature does, integrity over availability. *)
let test_the_keyring_does_not_open_under_another_master_key env uri =
  with_fixture ~name:"keyring_master" env uri @@ fun f ->
  let signature = get "sign" (atomic f (fun tx -> sign f tx "1" "hello")) in
  let other = service ~table:f.table (master_key ()) in
  Alcotest.(check (result unit error))
    "the keyring" (Error Kms_error.Decrypt)
    (Result.map ignore
       (atomic f (fun tx -> Pg_kms.public_keys other tx ~tenant_id:"1" ed25519)));
  Alcotest.check verified "a verification" (Error Kms_error.Decrypt)
    (atomic f (fun tx -> Pg_kms.verify other tx ~tenant_id:"1" "hello" signature))

(* A writer of the table without the master key can change the public half
   and never the wrapped private half: the mismatch is refused on the read
   and on a verification, under either the stored or the forged key. *)
let test_a_public_half_replaced_in_the_table_is_refused env uri =
  with_fixture ~name:"tampered" env uri @@ fun f ->
  let signed =
    get "atomic"
      (atomic f (fun tx ->
           let* _ = sign f tx "1" "hello" in
           let* _ = rotate f tx "1" in
           sign f tx "1" "hello"))
  in
  let forged =
    get "generate" (Signing_key.generate ~tenant_id:"1" ~algorithm:ed25519 ~version:(v 2))
  in
  let forged_signature = Signing_key.sign forged "hello" in
  with_session f (fun session ->
      let module C = (val Session.connection session) in
      let open Caqti_request.Infix in
      let update =
        (Caqti_type.octets ->. Caqti_type.unit)
          ~oneshot:true
          (Printf.sprintf
             "UPDATE %s_sign SET public_key = $1 WHERE tenant_id = '1' AND key_version = \
              2"
             f.table)
      in
      match C.exec update (Public_key.to_string (Signing_key.public forged)) with
      | Ok () -> Ok ()
      | Error err -> Alcotest.failf "update: %a" Caqti_error.pp err);
  let is_malformed = function
    | Error (Kms_error.Malformed _) -> true
    | _ -> false
  in
  get "atomic"
    (atomic f (fun tx ->
         Alcotest.(check bool)
           "the keyring is refused" true
           (is_malformed (Pg_kms.public_keys f.kms tx ~tenant_id:"1" ed25519));
         List.iter
           (fun (what, signature) ->
             Alcotest.(check bool)
               what true
               (is_malformed (verify f tx "1" "hello" signature)))
           [
             ("the stored key's signature", signed); ("the forged key's", forged_signature);
           ];
         Ok ()))

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (fun () -> test env uri) in
  [
    case "the custody of signing keys keeps the rules"
      test_the_custody_of_signing_keys_keeps_the_rules;
    case "the custody of mac keys keeps the rules"
      test_the_custody_of_mac_keys_keeps_the_rules;
    case "two makings at once make one key" test_two_makings_at_once_make_one_key;
    case "a rotation period is refused by a table"
      test_a_rotation_period_is_refused_by_a_table;
    case "the earlier version signs when it is named"
      test_the_earlier_version_signs_when_it_is_named;
    case "a pinned signature makes nothing" test_a_pinned_signature_makes_nothing;
    case "the keyring is the versions there are"
      test_the_keyring_is_the_versions_there_are;
    case "the keyring does not open under another master key"
      test_the_keyring_does_not_open_under_another_master_key;
    case "a public half replaced in the table is refused"
      test_a_public_half_replaced_in_the_table_is_refused;
    case "a signature verifies and a changed message does not"
      test_a_signature_verifies_and_a_changed_message_does_not;
    case "an ecdsa signature is raw r and s" test_an_ecdsa_signature_is_raw_r_and_s;
    case "what an earlier version signed still verifies after a rotation"
      test_what_an_earlier_version_signed_still_verifies_after_a_rotation;
    case "a signature by a key the tenant has not got is told apart from a bad one"
      test_a_signature_by_a_key_the_tenant_has_not_got_is_told_apart_from_a_bad_one;
    case "one tenant's key does not verify another's signature"
      test_one_tenant_s_key_does_not_verify_another_s_signature;
    case "a key wrapped under one master key does not open under another"
      test_a_key_wrapped_under_one_master_key_does_not_open_under_another;
    case "deleting the key ends what it signed" test_deleting_the_key_ends_what_it_signed;
    case "a mac is the same twice and another's differs"
      test_a_mac_is_the_same_twice_and_another_s_differs;
    case "two first signings at once share one key"
      test_two_first_signings_at_once_share_one_key;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] kms signing integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Pg_kms signing" [ ("integration", cases env uri) ]
