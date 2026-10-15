(** The model, without a service: the master key over KEKs, a KEK over DEKs, the wire form
    of a wrapped key, the signing and MAC keys beside the KEK, a KEK serving a derivation
    context, and what the Python port wrapped. *)

open Ascetic_kms

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let key () = get "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)
let master key = get "master" (Master_key.make key Algorithm.Aes_256_gcm)
let v = Key_version.of_int_exn

let hex text =
  String.init
    (String.length text / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * i) 2)))

let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_key = Alcotest.testable Key.pp Key.equal
let a_wrapped = Alcotest.testable Wrapped_key.pp Wrapped_key.equal
let a_version = Alcotest.testable Key_version.pp Key_version.equal
let unwrapped = Alcotest.(result a_key error)
let failure result = Result.map (fun _ -> ()) result

(* The master key wraps for a tenant. *)

let test_a_wrapped_key_unwraps_under_the_key_that_wrapped_it () =
  let master = master (key ()) and dek = key () in
  let wrapped = get "wrap" (Master_key.wrap master ~tenant_id:"t1" dek) in
  Alcotest.check a_version "names the master key's version" Master_key.version
    (Wrapped_key.key_version wrapped);
  Alcotest.check unwrapped "unwraps" (Ok dek)
    (Master_key.unwrap master ~tenant_id:"t1" wrapped)

let test_wrapping_twice_gives_two_wrapped_forms () =
  let master = master (key ()) and dek = key () in
  let one = get "wrap" (Master_key.wrap master ~tenant_id:"t1" dek) in
  let other = get "wrap" (Master_key.wrap master ~tenant_id:"t1" dek) in
  Alcotest.(check bool) "different" false (Wrapped_key.equal one other)

let test_another_tenant_does_not_unwrap_it () =
  let master = master (key ()) in
  let wrapped = get "wrap" (Master_key.wrap master ~tenant_id:"t1" (key ())) in
  Alcotest.check unwrapped "refused" (Error Kms_error.Decrypt)
    (Master_key.unwrap master ~tenant_id:"t2" wrapped)

(* The master key over KEKs. *)

let test_the_first_kek_is_version_one () =
  let master = master (key ()) in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  Alcotest.(check string) "tenant" "tenant-1" (Kek.tenant_id kek);
  Alcotest.check a_version "version" (v 1) (Kek.version kek);
  Alcotest.(check bool)
    "of the master key's algorithm" true
    (Algorithm.equal (Kek.algorithm kek) (Master_key.algorithm master));
  Alcotest.check a_version "wrapped by the master key" Master_key.version
    (Wrapped_key.key_version (Kek.wrapped kek));
  Alcotest.(check int)
    "thirty-two bytes under the wrapping" 32
    (Key.length
       (get "unwrap" (Master_key.unwrap master ~tenant_id:"tenant-1" (Kek.wrapped kek))))

let test_a_kek_loads_back_from_its_wrapped_form () =
  let master = master (key ()) in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  let dek = key () in
  let wrapped = get "wrap" (Kek.wrap kek dek) in
  let loaded =
    get "load_kek"
      (Master_key.load_kek master ~tenant_id:"tenant-1" ~version:(Kek.version kek)
         ~algorithm:(Kek.algorithm kek) (Kek.wrapped kek))
  in
  Alcotest.(check string) "tenant" (Kek.tenant_id kek) (Kek.tenant_id loaded);
  Alcotest.check a_version "version" (Kek.version kek) (Kek.version loaded);
  Alcotest.check a_wrapped "wrapped form" (Kek.wrapped kek) (Kek.wrapped loaded);
  Alcotest.check unwrapped "unwraps what the first wrapped" (Ok dek)
    (Kek.unwrap loaded wrapped)

let test_a_kek_does_not_load_under_another_tenant () =
  let master = master (key ()) in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"t1") in
  Alcotest.(check (result unit error))
    "refused" (Error Kms_error.Decrypt)
    (failure
       (Master_key.load_kek master ~tenant_id:"t2" ~version:(v 1)
          ~algorithm:Algorithm.Aes_256_gcm (Kek.wrapped kek)))

let test_a_kek_does_not_load_under_another_master_key () =
  let kek =
    get "generate_kek" (Master_key.generate_kek (master (key ())) ~tenant_id:"t1")
  in
  Alcotest.(check (result unit error))
    "refused" (Error Kms_error.Decrypt)
    (failure
       (Master_key.load_kek
          (master (key ()))
          ~tenant_id:"t1" ~version:(v 1) ~algorithm:Algorithm.Aes_256_gcm
          (Kek.wrapped kek)))

let test_rotation_makes_the_next_version () =
  let master = master (key ()) in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  let rotated = get "rotate_kek" (Master_key.rotate_kek master kek) in
  Alcotest.check a_version "one up" (v 2) (Kek.version rotated);
  Alcotest.(check string) "same tenant" (Kek.tenant_id kek) (Kek.tenant_id rotated);
  Alcotest.(check bool)
    "same algorithm" true
    (Algorithm.equal (Kek.algorithm kek) (Kek.algorithm rotated));
  Alcotest.(check bool)
    "another key" false
    (Wrapped_key.equal (Kek.wrapped kek) (Kek.wrapped rotated))

(* The KEK over DEKs. *)

let test_a_kek_wraps_a_dek_and_unwraps_it () =
  let kek =
    get "generate_kek" (Master_key.generate_kek (master (key ())) ~tenant_id:"tenant-1")
  in
  let dek = key () in
  Alcotest.check unwrapped "round trip" (Ok dek)
    (Result.bind (Kek.wrap kek dek) (Kek.unwrap kek))

let test_a_kek_makes_a_dek_of_its_kind_and_wraps_it () =
  let kek =
    get "generate_kek" (Master_key.generate_kek (master (key ())) ~tenant_id:"tenant-1")
  in
  let dek, wrapped = get "generate_dek" (Kek.generate_dek kek) in
  Alcotest.(check int) "thirty-two bytes" 32 (Key.length dek);
  Alcotest.check a_version "names the KEK's version" (Kek.version kek)
    (Wrapped_key.key_version wrapped);
  Alcotest.check unwrapped "unwraps" (Ok dek) (Kek.unwrap kek wrapped)

let test_the_wrapped_key_names_the_kek_version_on_the_wire () =
  let master = master (key ()) in
  let kek =
    get "rotate_kek"
      (Master_key.rotate_kek master
         (get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1")))
  in
  let wrapped = get "wrap" (Kek.wrap kek (key ())) in
  let bytes = Wrapped_key.to_bytes wrapped in
  Alcotest.(check string)
    "four big-endian bytes" "\000\000\000\002" (String.sub bytes 0 4);
  Alcotest.(check (result a_wrapped error))
    "parses back" (Ok wrapped) (Wrapped_key.parse bytes)

let test_a_rotated_kek_refuses_what_the_old_one_wrapped_and_rewraps_it () =
  let master = master (key ()) in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  let dek = key () in
  let wrapped = get "wrap" (Kek.wrap kek dek) in
  let rotated = get "rotate_kek" (Master_key.rotate_kek master kek) in
  Alcotest.check unwrapped "the old one still unwraps" (Ok dek) (Kek.unwrap kek wrapped);
  Alcotest.check unwrapped "the new one refuses before trying"
    (Error (Kms_error.Wrong_key_version { expected = 2; found = 1 }))
    (Kek.unwrap rotated wrapped);
  let rewrapped = get "rewrap" (Kek.rewrap rotated ~from:kek wrapped) in
  Alcotest.check a_version "under version two" (v 2) (Wrapped_key.key_version rewrapped);
  Alcotest.check unwrapped "unwraps" (Ok dek) (Kek.unwrap rotated rewrapped)

let test_keks_of_two_tenants_do_not_unwrap_each_other_s () =
  let master = master (key ()) in
  let kek1 = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  let kek2 = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"t2") in
  let wrapped = get "wrap" (Kek.wrap kek1 (key ())) in
  Alcotest.check unwrapped "refused" (Error Kms_error.Decrypt) (Kek.unwrap kek2 wrapped)

(* The edges. *)

let is_malformed = function
  | Error (Kms_error.Malformed _) -> true
  | _ -> false

let test_a_master_key_of_the_wrong_length_is_refused () =
  Alcotest.(check bool)
    "sixteen bytes" true
    (is_malformed
       (Master_key.make (Key.of_string (String.make 16 '\000')) Algorithm.Aes_256_gcm))

let test_bytes_too_short_to_name_a_version_are_refused () =
  Alcotest.(check bool)
    "three bytes" true
    (is_malformed (Wrapped_key.parse "\000\000\007"));
  Alcotest.(check (result a_wrapped error))
    "four bytes are a version and nothing sealed"
    (Ok (Wrapped_key.make ~key_version:(v 7) ~sealed:""))
    (Wrapped_key.parse "\000\000\000\007")

let test_a_version_is_what_four_bytes_hold () =
  Alcotest.(check bool) "minus one" true (is_malformed (Key_version.of_int (-1)));
  Alcotest.(check bool)
    "two to the thirty-second" true
    (is_malformed (Key_version.of_int (Key_version.max + 1)));
  let last = v Key_version.max in
  Alcotest.(check bool)
    "nothing after the last" true
    (is_malformed (Key_version.next last));
  Alcotest.(check string)
    "the last on the wire" "\255\255\255\255rest"
    (Key_version.stamp last "rest");
  Alcotest.(check bool)
    "and read back unsigned" true
    (Key_version.read ~what:"versioned" "\255\255\255\255rest" = Ok (last, "rest"))

let test_a_kek_never_shows_its_key () =
  let key = key () in
  let master = master key in
  let kek = get "generate_kek" (Master_key.generate_kek master ~tenant_id:"tenant-1") in
  Alcotest.(check string)
    "the KEK" "Kek(tenant `tenant-1`, version 1, AES-256-GCM)"
    (Format.asprintf "%a" Kek.pp kek);
  Alcotest.(check string)
    "the master key" "Master_key(AES-256-GCM)"
    (Format.asprintf "%a" Master_key.pp master)

(* The signing and MAC keys, beside the KEK. *)

let a_signature = Alcotest.testable Signature.pp Signature.equal
let a_mac = Alcotest.testable Mac.pp Mac.equal
let a_public = Alcotest.testable Public_key.pp Public_key.equal
let verified = Alcotest.(result unit error)
let ed25519 = Signature_algorithm.Ed25519
let p256 = Signature_algorithm.Ecdsa_p256

let test_an_algorithm_reads_back_from_its_id () =
  List.iter
    (fun algorithm ->
      Alcotest.(check bool)
        (Signature_algorithm.to_string algorithm)
        true
        (Signature_algorithm.of_string (Signature_algorithm.to_string algorithm)
        = Some algorithm))
    Signature_algorithm.all;
  Alcotest.(check bool) "rsa-2048" true (Signature_algorithm.of_string "rsa-2048" = None)

let test_a_label_is_one_key_part () =
  Alcotest.(check (result string error))
    "draft-etag" (Ok "draft-etag")
    (Result.map Key_label.to_string (Key_label.make "draft-etag"));
  List.iter
    (fun bad ->
      Alcotest.(check bool)
        (Printf.sprintf "%S" bad) true
        (is_malformed (Key_label.make bad)))
    [ ""; "a.b"; "a/b"; "a b"; String.make 65 'x' ]

let test_each_algorithm_signs_in_its_form_and_verifies () =
  List.iter
    (fun (algorithm, public_length) ->
      let name = Signature_algorithm.to_string algorithm in
      let key =
        get "generate" (Signing_key.generate ~tenant_id:"t" ~algorithm ~version:(v 3))
      in
      Alcotest.(check int)
        (name ^ ": the public half's length")
        public_length
        (String.length (Public_key.to_string (Signing_key.public key)));
      let signature = Signing_key.sign key "hello" in
      Alcotest.(check bool)
        "of the algorithm" true
        (Signature_algorithm.equal (Signature.algorithm signature) algorithm);
      Alcotest.check a_version "of the version" (v 3) (Signature.key_version signature);
      Alcotest.(check int)
        (name ^ ": sixty-four bytes") 64
        (String.length (Signature.to_string signature));
      Alcotest.check verified "verifies" (Ok ())
        (Public_key.verify (Signing_key.public key) "hello" signature);
      Alcotest.check verified "another message" (Error Kms_error.Invalid_signature)
        (Public_key.verify (Signing_key.public key) "hellp" signature);
      let other =
        get "generate" (Signing_key.generate ~tenant_id:"t" ~algorithm ~version:(v 3))
      in
      Alcotest.check verified "another key" (Error Kms_error.Invalid_signature)
        (Public_key.verify (Signing_key.public other) "hello" signature))
    [ (ed25519, 32); (p256, 65) ]

let test_a_seed_makes_the_same_key_and_a_loaded_half_derives_the_same_public () =
  List.iter
    (fun algorithm ->
      let seed = String.make 32 '\007' in
      let a =
        get "of_seed" (Signing_key.of_seed ~tenant_id:"t" ~algorithm ~version:(v 1) seed)
      in
      let b =
        get "of_seed" (Signing_key.of_seed ~tenant_id:"t" ~algorithm ~version:(v 1) seed)
      in
      Alcotest.check a_public "the same public half" (Signing_key.public a)
        (Signing_key.public b);
      Alcotest.check a_signature "the same signature" (Signing_key.sign a "x")
        (Signing_key.sign b "x");
      let loaded =
        get "load"
          (Signing_key.load ~tenant_id:"t" ~algorithm ~version:(v 1)
             (Signing_key.private_key a))
      in
      Alcotest.check a_public "derived again" (Signing_key.public a)
        (Signing_key.public loaded);
      Alcotest.(check string)
        "the name"
        ("t.sign." ^ Signature_algorithm.to_string algorithm)
        (Signing_key.name a))
    Signature_algorithm.all

let test_a_public_key_is_checked_and_a_signature_of_another_algorithm_is_not_its () =
  Alcotest.(check bool)
    "thirty-one bytes are no Ed25519 key" true
    (is_malformed (Public_key.make ed25519 (String.make 31 '\000')));
  Alcotest.(check bool)
    "sixty-five zero bytes are no P-256 point" true
    (is_malformed (Public_key.make p256 (String.make 65 '\000')));
  let ed =
    get "generate" (Signing_key.generate ~tenant_id:"t" ~algorithm:ed25519 ~version:(v 1))
  in
  let p =
    get "generate" (Signing_key.generate ~tenant_id:"t" ~algorithm:p256 ~version:(v 1))
  in
  let signature = Signing_key.sign p "hello" in
  Alcotest.check verified "another algorithm" (Error Kms_error.Invalid_signature)
    (Public_key.verify (Signing_key.public ed) "hello" signature);
  let again =
    get "make" (Public_key.make p256 (Public_key.to_string (Signing_key.public p)))
  in
  Alcotest.check verified "the public bytes read back verify" (Ok ())
    (Public_key.verify again "hello" signature)

let test_a_private_half_that_is_no_key_of_the_algorithm_is_refused () =
  Alcotest.(check bool)
    "a P-256 scalar of zero" true
    (is_malformed
       (Signing_key.of_seed ~tenant_id:"t" ~algorithm:p256 ~version:(v 1)
          (String.make 32 '\000')));
  Alcotest.(check bool)
    "thirty-one bytes for Ed25519" true
    (is_malformed
       (Signing_key.of_seed ~tenant_id:"t" ~algorithm:ed25519 ~version:(v 1)
          (String.make 31 '\001')))

let test_a_mac_is_thirty_two_bytes_of_this_version_and_a_function_of_key_and_message () =
  let purpose = get "label" (Key_label.make "etag") in
  let key = get "generate" (Mac_key.generate ~tenant_id:"t" ~purpose ~version:(v 2)) in
  let first = Mac_key.mac key "hello" in
  Alcotest.check a_version "of the version" (v 2) (Mac.key_version first);
  Alcotest.(check int) "thirty-two bytes" 32 (String.length (Mac.to_string first));
  Alcotest.check a_mac "the same again" first (Mac_key.mac key "hello");
  Alcotest.(check bool)
    "another message" false
    (Mac.equal first (Mac_key.mac key "hellp"));
  let other = get "generate" (Mac_key.generate ~tenant_id:"t" ~purpose ~version:(v 2)) in
  Alcotest.(check bool) "another key" false (Mac.equal first (Mac_key.mac other "hello"));
  let loaded = Mac_key.load ~tenant_id:"t" ~purpose ~version:(v 2) (Mac_key.key key) in
  Alcotest.check a_mac "loaded" first (Mac_key.mac loaded "hello");
  Alcotest.(check string) "the name" "t.mac.etag" (Mac_key.name key)

(* A KEK serving a derivation context. *)

let test_a_kek_serving_a_context_binds_what_it_wraps_to_it () =
  let master = master (key ()) in
  let lifecycle =
    get "generate_kek"
      (Master_key.generate_kek ~context:"policy.lifecycle" master ~tenant_id:"tenant-1")
  in
  Alcotest.(check (option string))
    "the context" (Some "policy.lifecycle") (Kek.context lifecycle);
  let dek = key () in
  let wrapped = get "wrap" (Kek.wrap lifecycle dek) in
  Alcotest.check unwrapped "its own" (Ok dek) (Kek.unwrap lifecycle wrapped);
  (* The same key bytes, loaded for another context or for none, do not open
     it: the binding is the wrap's associated data. *)
  let load ?context () =
    get "load_kek"
      (Master_key.load_kek ?context master ~tenant_id:"tenant-1" ~version:(v 1)
         ~algorithm:Algorithm.Aes_256_gcm (Kek.wrapped lifecycle))
  in
  Alcotest.check unwrapped "another context" (Error Kms_error.Decrypt)
    (Kek.unwrap (load ~context:"integration.command" ()) wrapped);
  Alcotest.check unwrapped "no context" (Error Kms_error.Decrypt)
    (Kek.unwrap (load ()) wrapped);
  let rotated = get "rotate_kek" (Master_key.rotate_kek master lifecycle) in
  Alcotest.(check (option string))
    "a rotation keeps the context" (Some "policy.lifecycle") (Kek.context rotated);
  Alcotest.(check string)
    "and says so"
    "Kek(tenant `tenant-1`, context `policy.lifecycle`, version 2, AES-256-GCM)"
    (Format.asprintf "%a" Kek.pp rotated)

let test_the_binding_is_the_tenant_length_prefixed_then_the_context () =
  Alcotest.(check string) "no context" "tenant-1" (Kek.binding ~tenant_id:"tenant-1" None);
  Alcotest.(check string)
    "a context" "\000\000\000\008tenant-1policy.lifecycle"
    (Kek.binding ~tenant_id:"tenant-1" (Some "policy.lifecycle"))

(* Made by the Python port's [MasterKey] and [Kek] with the master key [00 01 .. 1f] for
   [tenant-1]: the first KEK, its rotation, and one DEK, [40 41 .. 5f], wrapped under
   each. *)
module Wrapped_by_the_python_port = struct
  let kek_v1 =
    "00000001a81366893e77f9851f6b13b9f2e4181704bcc43283f37b81741bc0fd0137c70d02cf28faaa22ac0a7b228367770147acc3f14d68debe95405cac4fc7"

  let kek_v2 =
    "000000010e09c2809b2663cdd8af35a7b9d86e4303611a2067f005639a47b3a44aa5e985ef4a3cadca09a598b91648fb4fe440760e0c84d7f4a8cfc151eed8af"

  let dek_under_v1 =
    "00000001b3d9d1393913407b952682e4e96b8c8c21d18b59211e65daf7a6060c07a8d24fed1899f548955ca16f07d75ce27ba7d225f03e83ae932f8c348a3628"

  let dek_under_v2 =
    "000000026a4bace0f5c2c7047cd2041010aab91c913a5a389c0158660ac23518007f2e612c8bb3a84209ee4f2f1f2eb58c04acb27fb358f6cc54fd16e82da757"

  let python_master () = master (Key.of_string (String.init 32 Char.chr))
  let wrapped text = get "parse" (Wrapped_key.parse (hex text))

  let load version text =
    get "load_kek"
      (Master_key.load_kek (python_master ()) ~tenant_id:"tenant-1" ~version:(v version)
         ~algorithm:Algorithm.Aes_256_gcm (wrapped text))

  let test_its_keks_load_here_and_unwrap_its_deks () =
    let dek = Key.of_string (String.init 32 (fun i -> Char.chr (0x40 + i))) in
    List.iter
      (fun (version, kek, dek_wrapped) ->
        Alcotest.check a_version "the DEK names the version" (v version)
          (Wrapped_key.key_version (wrapped dek_wrapped));
        Alcotest.check unwrapped
          (Printf.sprintf "version %d" version)
          (Ok dek)
          (Kek.unwrap (load version kek) (wrapped dek_wrapped)))
      [ (1, kek_v1, dek_under_v1); (2, kek_v2, dek_under_v2) ]

  let test_its_second_kek_refuses_what_its_first_wrapped () =
    Alcotest.check unwrapped "refused before trying"
      (Error (Kms_error.Wrong_key_version { expected = 2; found = 1 }))
      (Kek.unwrap (load 2 kek_v2) (wrapped dek_under_v1))
end

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Kms domain"
    [
      ( "the master key wraps for a tenant",
        [
          case "a wrapped key unwraps under the key that wrapped it"
            test_a_wrapped_key_unwraps_under_the_key_that_wrapped_it;
          case "wrapping twice gives two wrapped forms"
            test_wrapping_twice_gives_two_wrapped_forms;
          case "another tenant does not unwrap it" test_another_tenant_does_not_unwrap_it;
        ] );
      ( "the master key over KEKs",
        [
          case "the first kek is version one" test_the_first_kek_is_version_one;
          case "a kek loads back from its wrapped form"
            test_a_kek_loads_back_from_its_wrapped_form;
          case "a kek does not load under another tenant"
            test_a_kek_does_not_load_under_another_tenant;
          case "a kek does not load under another master key"
            test_a_kek_does_not_load_under_another_master_key;
          case "rotation makes the next version" test_rotation_makes_the_next_version;
        ] );
      ( "the KEK over DEKs",
        [
          case "a kek wraps a dek and unwraps it" test_a_kek_wraps_a_dek_and_unwraps_it;
          case "a kek makes a dek of its kind and wraps it"
            test_a_kek_makes_a_dek_of_its_kind_and_wraps_it;
          case "the wrapped key names the kek version on the wire"
            test_the_wrapped_key_names_the_kek_version_on_the_wire;
          case "a rotated kek refuses what the old one wrapped and rewraps it"
            test_a_rotated_kek_refuses_what_the_old_one_wrapped_and_rewraps_it;
          case "keks of two tenants do not unwrap each other's"
            test_keks_of_two_tenants_do_not_unwrap_each_other_s;
        ] );
      ( "the edges",
        [
          case "a master key of the wrong length is refused"
            test_a_master_key_of_the_wrong_length_is_refused;
          case "bytes too short to name a version are refused"
            test_bytes_too_short_to_name_a_version_are_refused;
          case "a version is what four bytes hold" test_a_version_is_what_four_bytes_hold;
          case "a kek never shows its key" test_a_kek_never_shows_its_key;
        ] );
      ( "the signing and MAC keys",
        [
          case "an algorithm reads back from its id"
            test_an_algorithm_reads_back_from_its_id;
          case "a label is one key part" test_a_label_is_one_key_part;
          case "each algorithm signs in its form and verifies"
            test_each_algorithm_signs_in_its_form_and_verifies;
          case "a seed makes the same key and a loaded half derives the same public"
            test_a_seed_makes_the_same_key_and_a_loaded_half_derives_the_same_public;
          case "a public key is checked and a signature of another algorithm is not its"
            test_a_public_key_is_checked_and_a_signature_of_another_algorithm_is_not_its;
          case "a private half that is no key of the algorithm is refused"
            test_a_private_half_that_is_no_key_of_the_algorithm_is_refused;
          case
            "a mac is thirty-two bytes of this version and a function of key and message"
            test_a_mac_is_thirty_two_bytes_of_this_version_and_a_function_of_key_and_message;
        ] );
      ( "a kek serving a context",
        [
          case "a kek serving a context binds what it wraps to it"
            test_a_kek_serving_a_context_binds_what_it_wraps_to_it;
          case "the binding is the tenant length-prefixed then the context"
            test_the_binding_is_the_tenant_length_prefixed_then_the_context;
        ] );
      ( "wrapped by the python port",
        [
          case "its keks load here and unwrap its deks"
            Wrapped_by_the_python_port.test_its_keks_load_here_and_unwrap_its_deks;
          case "its second kek refuses what its first wrapped"
            Wrapped_by_the_python_port.test_its_second_kek_refuses_what_its_first_wrapped;
        ] );
    ]
