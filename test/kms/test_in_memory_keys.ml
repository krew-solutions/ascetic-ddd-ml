(** The in-memory keys behave as the adapters do, where the tests of a consumer would
    notice a difference: the custody rules of ADR-0036 in the one body every adapter
    runs, and what signs, pins, retires and publishes. *)

open Ascetic_kms

(* The session is whatever the consumer's is; the keys ignore it. *)
module Keys = In_memory_keys.Make (struct
  type t = unit
end)

module Rules = Ascetic_kms_test_custody.Custody_rules.Make (Keys)

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let v = Key_version.of_int_exn
let ed25519 = Signature_algorithm.Ed25519
let p256 = Signature_algorithm.Ecdsa_p256
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_signature = Alcotest.testable Signature.pp Signature.equal
let a_mac = Alcotest.testable Mac.pp Mac.equal
let a_version = Alcotest.testable Key_version.pp Key_version.equal
let verified = Alcotest.(result unit error)
let signed = Alcotest.(result a_signature error)
let seed byte = String.make 32 byte
let zeroes = Signature.make ~algorithm:ed25519 ~key_version:(v 1) (String.make 64 '\000')
let sign keys ?(algorithm = ed25519) tenant_id message =
  get "sign" (Keys.sign keys () ~tenant_id algorithm message)
let verify keys tenant_id message signature =
  Keys.verify keys () ~tenant_id message signature
let versions_published keys tenant_id =
  Public_keyring.versions
    (get "public_keys" (Keys.public_keys keys () ~tenant_id ed25519))

(* The scope the custody rules run in: the keys, managed, and no session. *)
let managed () =
  let keys = Keys.create ~managed_keys:true () in
  ({ Rules.atomic = (fun f -> f () keys) }, keys)

let test_the_custody_of_signing_keys_keeps_the_rules () =
  Rules.signing_custody (fst (managed ())) ~tenant:"t"

let test_the_custody_of_mac_keys_keeps_the_rules () =
  Rules.mac_custody (fst (managed ())) ~tenant:"t"

let test_a_rotation_period_is_refused_here () =
  let _, keys = managed () in
  let options = { Key_options.auto_rotate_period = Some 3600.0 } in
  let is_unsupported = function
    | Error (Kms_error.Unsupported _) -> true
    | _ -> false
  in
  Alcotest.(check bool)
    "a signing key" true
    (is_unsupported (Keys.create_signing_key keys () ~tenant_id:"t" ed25519 options));
  Alcotest.(check bool)
    "a mac key" true
    (is_unsupported
       (Keys.create_mac_key keys () ~tenant_id:"t"
          (get "label" (Key_label.make "etag"))
          options))

let test_managed_keys_still_sign_with_a_registered_seed () =
  let _, keys = managed () in
  Alcotest.(check bool)
    "nothing to sign with" true
    (match Keys.sign keys () ~tenant_id:"t" ed25519 "hello" with
    | Error (Kms_error.Signing_key_not_found _) -> true
    | _ -> false);
  ignore (get "register" (Keys.register keys ~tenant_id:"t" ~seed:(seed '\007')));
  Alcotest.check a_version "signs under the registered version" (v 1)
    (Signature.key_version (sign keys "t" "hello"))

let test_a_registered_seed_signs_the_same_from_run_to_run () =
  let keys = Keys.create () in
  Alcotest.check a_version "the first version" (v 1)
    (get "register" (Keys.register keys ~tenant_id:"t" ~seed:(seed '\007')));
  let first = sign keys "t" "hello" in
  let other = Keys.create () in
  ignore (get "register" (Keys.register other ~tenant_id:"t" ~seed:(seed '\007')));
  Alcotest.check a_signature "the same from the same seed" first (sign other "t" "hello");
  Alcotest.check a_version "of version one" (v 1) (Signature.key_version first);
  Alcotest.check verified "verifies" (Ok ()) (verify keys "t" "hello" first)

let test_an_unregistered_tenant_gets_a_key_on_first_signing () =
  let keys = Keys.create () in
  let signature = sign keys "t" "hello" in
  Alcotest.check verified "verifies" (Ok ()) (verify keys "t" "hello" signature);
  Alcotest.check verified "another message" (Error Kms_error.Invalid_signature)
    (verify keys "t" "hellp" signature)

let test_a_missing_key_and_a_missing_version_are_named () =
  let keys = Keys.create () in
  Alcotest.check verified "no key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    (verify keys "t" "hello" zeroes);
  ignore (get "register" (Keys.register keys ~tenant_id:"t" ~seed:(seed '\001')));
  Alcotest.check verified "no such version" (Error (Kms_error.No_key_of_version 2))
    (verify keys "t" "hello"
       (Signature.make ~algorithm:ed25519 ~key_version:(v 2) (String.make 64 '\000')))

let test_a_rotation_is_the_next_version_and_the_earlier_one_still_verifies () =
  let keys = Keys.create () in
  let first = sign keys "t" "hello" in
  Alcotest.check a_version "two" (v 2)
    (get "rotate" (Keys.rotate_signing_key keys () ~tenant_id:"t" ed25519));
  let second = sign keys "t" "hello" in
  Alcotest.check a_version "signed by two" (v 2) (Signature.key_version second);
  Alcotest.check verified "the first still verifies" (Ok ())
    (verify keys "t" "hello" first);
  get "delete" (Keys.delete_signing_key keys () ~tenant_id:"t" ed25519);
  Alcotest.check verified "gone with the key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    (verify keys "t" "hello" first)

let test_an_ecdsa_signature_is_raw_r_and_s_under_a_key_of_its_own () =
  let keys = Keys.create () in
  ignore (get "register" (Keys.register keys ~tenant_id:"t" ~seed:(seed '\001')));
  let signature = sign keys ~algorithm:p256 "t" "hello" in
  Alcotest.(check bool)
    "of the algorithm" true
    (Signature_algorithm.equal (Signature.algorithm signature) p256);
  Alcotest.check a_version "the Ed25519 ring is another key" (v 1)
    (Signature.key_version signature);
  Alcotest.(check int) "not DER" 64 (String.length (Signature.to_string signature));
  Alcotest.check verified "verifies" (Ok ()) (verify keys "t" "hello" signature)

let test_a_mac_repeats_and_differs_by_message_tenant_and_purpose () =
  let keys = Keys.create () in
  let etag = get "label" (Key_label.make "etag") in
  let other = get "label" (Key_label.make "other") in
  let mac tenant_id purpose message =
    get "mac" (Keys.mac keys () ~tenant_id purpose message)
  in
  let first = mac "t" etag "hello" in
  Alcotest.check a_mac "the same again" first (mac "t" etag "hello");
  Alcotest.check a_version "of version one" (v 1) (Mac.key_version first);
  Alcotest.(check int) "thirty-two bytes" 32 (String.length (Mac.to_string first));
  List.iter
    (fun (what, mac) -> Alcotest.(check bool) what false (Mac.equal first mac))
    [
      ("another message", mac "t" etag "hellp");
      ("another tenant", mac "u" etag "hello");
      ("another purpose", mac "t" other "hello");
    ];
  get "delete" (Keys.delete_mac_key keys () ~tenant_id:"t" etag);
  Alcotest.(check bool)
    "a new key after deletion" false
    (Mac.equal first (mac "t" etag "hello"))

let test_a_seed_that_is_no_key_is_refused () =
  let keys = Keys.create () in
  Alcotest.(check bool)
    "thirty-one bytes" true
    (match Keys.register keys ~tenant_id:"t" ~seed:(String.make 31 '\001') with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false)

(* A pinned signature of a tenant without a key, or at a version the key
   has not got, is refused and makes nothing: the key is not there after,
   and the keyring has what it had. *)
let test_a_refused_pinned_signature_leaves_no_key_behind () =
  let keys = Keys.create () in
  let pinned version =
    Keys.sign_with_version keys () ~tenant_id:"t" ed25519 (v version) "hello"
  in
  Alcotest.check signed "no key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = "t"; algorithm = ed25519 }))
    (pinned 1);
  Alcotest.(check bool)
    "none made" true
    (match Keys.public_keys keys () ~tenant_id:"t" ed25519 with
    | Error (Kms_error.Signing_key_not_found _) -> true
    | _ -> false);
  let first = sign keys "t" "hello" in
  Alcotest.check signed "no such version" (Error (Kms_error.No_key_of_version 5))
    (pinned 5);
  Alcotest.(check (list int))
    "the one version" [ 1 ]
    (List.map Key_version.to_int (versions_published keys "t"));
  Alcotest.check signed "the version there is signs the same" (Ok first) (pinned 1);
  Alcotest.(check bool)
    "version zero is no version" true
    (match pinned 0 with
    | Error (Kms_error.Malformed _) -> true
    | _ -> false)

let test_a_retired_version_neither_signs_verifies_nor_is_published () =
  let keys = Keys.create () in
  let first = sign keys "t" "hello" in
  let rotate () = get "rotate" (Keys.rotate_signing_key keys () ~tenant_id:"t" ed25519) in
  let retire ?(tenant_id = "t") bound =
    Keys.retire_signing_versions_before keys () ~tenant_id ed25519 (v bound)
  in
  let pinned version =
    Keys.sign_with_version keys () ~tenant_id:"t" ed25519 (v version) "hello"
  in
  ignore (rotate ());
  ignore (rotate ());
  Alcotest.check signed "the earlier version signs when it is named" (Ok first) (pinned 1);
  get "retire" (retire 2);
  Alcotest.check signed "retired: signs nothing" (Error (Kms_error.No_key_of_version 1))
    (pinned 1);
  Alcotest.check verified "retired: verifies nothing"
    (Error (Kms_error.No_key_of_version 1))
    (verify keys "t" "hello" first);
  Alcotest.(check (list int))
    "retired: not published" [ 2; 3 ]
    (List.map Key_version.to_int (versions_published keys "t"));
  Alcotest.check a_version "a retired number is not used again" (v 4) (rotate ());
  Alcotest.(check (result unit error))
    "above the latest" (Error (Kms_error.No_key_of_version 9)) (retire 9);
  Alcotest.(check bool)
    "a tenant without the key" true
    (match retire ~tenant_id:"u" 1 with
    | Error (Kms_error.Signing_key_not_found _) -> true
    | _ -> false)

let test_a_registered_seed_publishes_its_key () =
  let keys = Keys.create () in
  ignore (get "register" (Keys.register keys ~tenant_id:"t" ~seed:(seed '\007')));
  let signature = sign keys "t" "hello" in
  let keyring = get "public_keys" (Keys.public_keys keys () ~tenant_id:"t" ed25519) in
  Alcotest.(check bool)
    "of the algorithm" true
    (Signature_algorithm.equal (Public_keyring.algorithm keyring) ed25519);
  match Public_keyring.get keyring (v 1) with
  | None -> Alcotest.fail "version one is not published"
  | Some key ->
      Alcotest.check verified "the published half verifies what the key signed" (Ok ())
        (Public_key.verify key "hello" signature);
      Alcotest.(check bool)
        "no other version" true
        (Public_keyring.get keyring (v 2) = None)

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "In_memory_keys"
    [
      ( "custody",
        [
          case "the custody of signing keys keeps the rules"
            test_the_custody_of_signing_keys_keeps_the_rules;
          case "the custody of mac keys keeps the rules"
            test_the_custody_of_mac_keys_keeps_the_rules;
          case "a rotation period is refused here" test_a_rotation_period_is_refused_here;
          case "managed keys still sign with a registered seed"
            test_managed_keys_still_sign_with_a_registered_seed;
        ] );
      ( "keys in memory",
        [
          case "a registered seed signs the same from run to run"
            test_a_registered_seed_signs_the_same_from_run_to_run;
          case "an unregistered tenant gets a key on first signing"
            test_an_unregistered_tenant_gets_a_key_on_first_signing;
          case "a missing key and a missing version are named"
            test_a_missing_key_and_a_missing_version_are_named;
          case "a rotation is the next version and the earlier one still verifies"
            test_a_rotation_is_the_next_version_and_the_earlier_one_still_verifies;
          case "an ecdsa signature is raw r and s under a key of its own"
            test_an_ecdsa_signature_is_raw_r_and_s_under_a_key_of_its_own;
          case "a mac repeats and differs by message, tenant and purpose"
            test_a_mac_repeats_and_differs_by_message_tenant_and_purpose;
          case "a seed that is no key is refused" test_a_seed_that_is_no_key_is_refused;
        ] );
      ( "pinned signatures and public keys",
        [
          case "a refused pinned signature leaves no key behind"
            test_a_refused_pinned_signature_leaves_no_key_behind;
          case "a retired version neither signs, verifies nor is published"
            test_a_retired_version_neither_signs_verifies_nor_is_published;
          case "a registered seed publishes its key"
            test_a_registered_seed_publishes_its_key;
        ] );
    ]
