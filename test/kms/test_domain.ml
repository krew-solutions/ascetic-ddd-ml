(** The model, without a service: the master key over KEKs, a KEK over DEKs, the wire form
    of a wrapped key, and what the Python port wrapped. *)

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

let is_malformed = function Error (Kms_error.Malformed _) -> true | _ -> false

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
      ( "wrapped by the python port",
        [
          case "its keks load here and unwrap its deks"
            Wrapped_by_the_python_port.test_its_keks_load_here_and_unwrap_its_deks;
          case "its second kek refuses what its first wrapped"
            Wrapped_by_the_python_port.test_its_second_kek_refuses_what_its_first_wrapped;
        ] );
    ]
