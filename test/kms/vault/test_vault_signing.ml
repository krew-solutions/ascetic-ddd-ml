(** The signing and MAC ports on Vault Transit, against a dev server with the [transit]
    engine enabled, as [test_vault.ml]; the same properties [test_pg_signing.ml] holds on
    PostgreSQL: the custody rules of ADR-0036 in the one body every adapter runs, pinned
    signatures and the keyring (ADR-0035), and a holder's token that signs and MACs and
    holds no custody. Skipped without [TEST_VAULT_ADDR]. *)

open Ascetic_kms
module Transport = Ascetic_kms_vault_cohttp.Cohttp_transport
module Vault = Ascetic_kms_vault.Vault_transit.Make (Transport)
module Rules = Ascetic_kms_test_custody.Custody_rules.Make (Vault)
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

let atomic_with f kms body =
  Rest_pool.session f.sessions ~lift (fun session ->
      Rest.atomic session ~lift (fun tx -> body tx kms))

let atomic f body = atomic_with f f.kms body

(* One call of Vault's own API under the root token, for what the tests set
   up around the adapter: policies, tokens, keys of another type. *)
let operator f ~meth ~path body =
  match
    Transport.send (Rest_pool.client f.sessions)
      { meth; url = Printf.sprintf "%s/v1/%s" f.addr path; token = f.token; body }
  with
  | Ok { status; body } when status >= 200 && status < 300 -> body
  | Ok { status; _ } -> Alcotest.failf "%s %s: status %d" meth path status
  | Error reason -> Alcotest.failf "%s %s: %s" meth path reason

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

(* ---- Pinned signatures and the keyring (ADR-0035) ---- *)

let a_signature = Alcotest.testable Signature.pp Signature.equal
let signed = Alcotest.(result a_signature error)

let pinned f ?(algorithm = ed25519) version message =
  atomic f (fun tx kms ->
      Vault.sign_with_version kms tx ~tenant_id:f.tenant algorithm (v version) message)

let rotate f ?(algorithm = ed25519) () =
  get "rotate"
    (atomic f (fun tx kms ->
         Vault.rotate_signing_key kms tx ~tenant_id:f.tenant algorithm))

let retire f bound =
  atomic f (fun tx kms ->
      Vault.retire_signing_versions_before kms tx ~tenant_id:f.tenant ed25519 (v bound))

let keyring f algorithm =
  get "public_keys"
    (atomic f (fun tx kms -> Vault.public_keys kms tx ~tenant_id:f.tenant algorithm))

let test_after_a_rotation_the_earlier_version_signs_when_it_is_named f =
  let first = sign f "hello" in
  ignore (rotate f ());
  Alcotest.check signed "the same signature, Ed25519 being deterministic" (Ok first)
    (pinned f 1 "hello");
  Alcotest.check a_version "the latest signs otherwise" (v 2)
    (Signature.key_version (sign f "hello"))

let test_a_pinned_signature_makes_no_key f =
  Alcotest.check signed "no key"
    (Error (Kms_error.Signing_key_not_found { tenant_id = f.tenant; algorithm = ed25519 }))
    (pinned f 1 "hello");
  Alcotest.(check bool)
    "and none made" true
    (match
       atomic f (fun tx kms -> Vault.public_keys kms tx ~tenant_id:f.tenant ed25519)
     with
    | Error (Kms_error.Signing_key_not_found _) -> true
    | _ -> false)

let test_a_version_the_key_does_not_sign_with_is_named f =
  let first = sign f "hello" in
  ignore (rotate f ());
  Alcotest.check signed "above the latest" (Error (Kms_error.No_key_of_version 3))
    (pinned f 3 "hello");
  get "retire" (retire f 2);
  Alcotest.check signed "retired" (Error (Kms_error.No_key_of_version 1))
    (pinned f 1 "hello");
  Alcotest.check verified "and verifies nothing" (Error (Kms_error.No_key_of_version 1))
    (verify f "hello" first);
  (* A signing floor of the key's own, [min_encryption_version], set outside
     the port, below which a version verifies and no longer signs. *)
  ignore (rotate f ());
  let second = get "sign at two, before the floor rose" (pinned f 2 "hello") in
  ignore
    (operator f ~meth:"POST"
       ~path:(Printf.sprintf "transit/keys/%s.sign.ed25519/config" f.tenant)
       (Some (`Assoc [ ("min_encryption_version", `Int 3) ])));
  Alcotest.check signed "below the signing floor" (Error (Kms_error.No_key_of_version 2))
    (pinned f 2 "hello");
  Alcotest.check verified "while it still verifies" (Ok ()) (verify f "hello" second)

let test_the_keyring_is_what_transit_publishes_for_both_algorithms f =
  List.iter
    (fun algorithm ->
      let name = Signature_algorithm.to_string algorithm in
      let signature = sign f ~algorithm "hello" in
      ignore (rotate f ~algorithm ());
      let keyring = keyring f algorithm in
      Alcotest.(check (list int))
        (name ^ ": both versions") [ 1; 2 ]
        (List.map Key_version.to_int (Public_keyring.versions keyring));
      List.iter
        (fun version ->
          Alcotest.(check bool)
            (Printf.sprintf "%s v%d: what Transit publishes" name version)
            true
            (Option.equal Public_key.equal
               (Public_keyring.get keyring (v version))
               (Some (public_key_transit_publishes f algorithm (v version)))))
        [ 1; 2 ];
      match Public_keyring.get keyring (v 1) with
      | Some key ->
          Alcotest.check verified
            (name ^ ": verifies what version one signed")
            (Ok ())
            (Public_key.verify key "hello" signature)
      | None -> Alcotest.fail (name ^ ": version one is not published"))
    Signature_algorithm.all

(* ---- Custody (ADR-0036) ---- *)

(* The scope the custody rules run in, over a service built with managed
   keys; the tenant is the fixture's, cleaned up after. *)
let managed f = Vault.create ~managed_keys:true ~addr:f.addr ~token:f.token ()
let scope f kms =
  { Rules.atomic = (fun body -> atomic_with f kms (fun tx kms -> body tx kms)) }

let test_the_custody_of_signing_keys_keeps_the_rules f =
  Rules.signing_custody (scope f (managed f)) ~tenant:f.tenant

let test_the_custody_of_mac_keys_keeps_the_rules f =
  Rules.mac_custody (scope f (managed f)) ~tenant:f.tenant

(* Transit says nothing of a key that exists with another type: the adapter
   reads the description and refuses. *)
let test_a_key_of_another_type_at_the_name_is_a_conflict f =
  ignore
    (operator f ~meth:"POST"
       ~path:(Printf.sprintf "transit/keys/%s.sign.ed25519" f.tenant)
       (Some (`Assoc [ ("type", `String "ecdsa-p256") ])));
  Alcotest.(check bool)
    "a conflict" true
    (match
       atomic f (fun tx kms ->
           Vault.create_signing_key kms tx ~tenant_id:f.tenant ed25519 Key_options.default)
     with
    | Error (Kms_error.Conflict _) -> true
    | _ -> false)

let test_a_rotation_period_asked_is_the_key_s f =
  let period = 7200.0 in
  Alcotest.check a_version "made" (v 1)
    (get "create"
       (atomic f (fun tx kms ->
            Vault.create_signing_key kms tx ~tenant_id:f.tenant ed25519
              { Key_options.auto_rotate_period = Some period })));
  let found =
    match
      operator f ~meth:"GET"
        ~path:(Printf.sprintf "transit/keys/%s.sign.ed25519" f.tenant)
        None
    with
    | Some json ->
        Yojson.Safe.Util.(json |> member "data" |> member "auto_rotate_period" |> to_int)
    | None -> Alcotest.fail "no description"
  in
  Alcotest.(check int) "the key's" 7200 found;
  (* Asked again with the same period: left as it is; with none: kept. *)
  Alcotest.check a_version "left as it is" (v 1)
    (get "create again"
       (atomic f (fun tx kms ->
            Vault.create_signing_key kms tx ~tenant_id:f.tenant ed25519
              Key_options.default)))

(* A holder's grant, [read] on the keys, [update] on [sign] and [hmac]:
   built with managed keys, it signs and MACs with the keys custody made,
   at the latest or at a version named; a tenant without them is named, not
   a 403; and every custody operation, and [verify], is [Forbidden]. *)
let test_a_holder_s_token_signs_and_macs_and_holds_no_custody f =
  let tenant = f.tenant in
  get "custody"
    (atomic f (fun tx kms ->
         let* _ =
           Vault.create_signing_key kms tx ~tenant_id:tenant ed25519 Key_options.default
         in
         let* _ = Vault.rotate_signing_key kms tx ~tenant_id:tenant ed25519 in
         let* _ =
           Vault.create_mac_key kms tx ~tenant_id:tenant (etag ()) Key_options.default
         in
         Ok ()));
  let policy = tenant ^ "-holder" in
  ignore
    (operator f ~meth:"PUT"
       ~path:("sys/policies/acl/" ^ policy)
       (Some
          (`Assoc
             [
               ( "policy",
                 `String
                   "path \"transit/keys/+\" { capabilities = [\"read\"] }\n\
                    path \"transit/sign/+\" { capabilities = [\"update\"] }\n\
                    path \"transit/hmac/+\" { capabilities = [\"update\"] }\n" );
             ])));
  let token =
    match
      operator f ~meth:"POST" ~path:"auth/token/create"
        (Some
           (`Assoc
              [
                ("policies", `List [ `String policy ]);
                ("ttl", `String "10m");
                ("no_default_policy", `Bool true);
              ]))
    with
    | Some json ->
        Yojson.Safe.Util.(json |> member "auth" |> member "client_token" |> to_string)
    | None -> Alcotest.fail "no token"
  in
  let holder = Vault.create ~managed_keys:true ~addr:f.addr ~token () in
  let as_holder body = atomic_with f holder body in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (operator f ~meth:"POST" ~path:"auth/token/revoke"
           (Some (`Assoc [ ("token", `String token) ])));
      ignore (operator f ~meth:"DELETE" ~path:("sys/policies/acl/" ^ policy) None))
    (fun () ->
      let signature, mac =
        get "the holder signs and macs"
          (as_holder (fun tx kms ->
               let* signature = Vault.sign kms tx ~tenant_id:tenant ed25519 "hello" in
               let* mac = Vault.mac kms tx ~tenant_id:tenant (etag ()) "hello" in
               Ok (signature, mac)))
      in
      Alcotest.(check (list int))
        "with the keys custody made" [ 2; 1 ]
        (List.map Key_version.to_int
           [ Signature.key_version signature; Mac.key_version mac ]);
      Alcotest.check a_version "at the version named" (v 1)
        (Signature.key_version
           (get "pinned"
              (as_holder (fun tx kms ->
                   Vault.sign_with_version kms tx ~tenant_id:tenant ed25519 (v 1) "hello"))));
      let nobody = tenant ^ "-nobody" in
      Alcotest.(check bool)
        "a tenant without a signing key is named, not a 403" true
        (match
           as_holder (fun tx kms -> Vault.sign kms tx ~tenant_id:nobody ed25519 "hello")
         with
        | Error (Kms_error.Signing_key_not_found _) -> true
        | _ -> false);
      Alcotest.(check bool)
        "a tenant without a mac key likewise" true
        (match
           as_holder (fun tx kms -> Vault.mac kms tx ~tenant_id:nobody (etag ()) "hello")
         with
        | Error (Kms_error.Mac_key_not_found _) -> true
        | _ -> false);
      List.iter
        (fun (what, outcome) ->
          Alcotest.(check bool)
            (what ^ " under the holder's token")
            true
            (match outcome with
            | Error (Kms_error.Forbidden { operation; _ }) -> String.equal operation what
            | _ -> false))
        [
          (* Verifying through the KMS is [verify], which this policy leaves
             out: a holder that signs holds no verify by that. *)
          ( "verify",
            as_holder (fun tx kms ->
                Vault.verify kms tx ~tenant_id:tenant "hello" signature) );
          ( "create",
            Result.map ignore
              (as_holder (fun tx kms ->
                   Vault.create_signing_key kms tx ~tenant_id:(tenant ^ "-new") ed25519
                     Key_options.default)) );
          ( "rotate",
            Result.map ignore
              (as_holder (fun tx kms ->
                   Vault.rotate_signing_key kms tx ~tenant_id:tenant ed25519)) );
          ( "retire",
            as_holder (fun tx kms ->
                Vault.retire_signing_versions_before kms tx ~tenant_id:tenant ed25519
                  (v 2)) );
          ( "delete",
            as_holder (fun tx kms ->
                Vault.delete_signing_key kms tx ~tenant_id:tenant ed25519) );
          ( "rotate",
            Result.map ignore
              (as_holder (fun tx kms ->
                   Vault.rotate_mac_key kms tx ~tenant_id:tenant (etag ()))) );
          ( "delete",
            as_holder (fun tx kms ->
                Vault.delete_mac_key kms tx ~tenant_id:tenant (etag ())) );
        ])

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
          ( "pinned signatures and the keyring",
            [
              case "after a rotation the earlier version signs when it is named"
                test_after_a_rotation_the_earlier_version_signs_when_it_is_named;
              case "a pinned signature makes no key" test_a_pinned_signature_makes_no_key;
              case "a version the key does not sign with is named"
                test_a_version_the_key_does_not_sign_with_is_named;
              case "the keyring is what transit publishes for both algorithms"
                test_the_keyring_is_what_transit_publishes_for_both_algorithms;
            ] );
          ( "custody",
            [
              case "the custody of signing keys keeps the rules"
                test_the_custody_of_signing_keys_keeps_the_rules;
              case "the custody of mac keys keeps the rules"
                test_the_custody_of_mac_keys_keeps_the_rules;
              case "a key of another type at the name is a conflict"
                test_a_key_of_another_type_at_the_name_is_a_conflict;
              case "a rotation period asked is the key's"
                test_a_rotation_period_asked_is_the_key_s;
              case "a holder's token signs and macs and holds no custody"
                test_a_holder_s_token_signs_and_macs_and_holds_no_custody;
            ] );
        ]
