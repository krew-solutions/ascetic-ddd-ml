(** The model, without a store: the resource's canonical text, and the ciphers that carry
    the version along. *)

open Ascetic_dek
module Kms_error = Ascetic_kms.Kms_error
module Algorithm = Ascetic_kms.Algorithm
module Key = Ascetic_kms.Key
module Key_version = Ascetic_kms.Key_version

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let key () = get "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)
let cipher key = get "cipher" (Algorithm.cipher Algorithm.Aes_256_gcm key ~aad:"aad")
let v = Key_version.of_int_exn
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let bytes = Alcotest.(result string error)
let is_malformed = function Error (Kms_error.Malformed _) -> true | _ -> false

let sealed_by version key plaintext =
  get "encrypt"
    (Versioned_cipher.encrypt (Versioned_cipher.make (v version) (cipher key)) plaintext)

(* The resource. *)

let test_a_resource_has_one_text_however_its_id_was_built () =
  let one =
    Resource.make ~tenant_id:"tenant-1" ~kind:"Order"
      (`Assoc [ ("shop", `Int 7); ("number", `String "A-1") ])
  and other =
    Resource.make ~tenant_id:"tenant-1" ~kind:"Order"
      (`Assoc [ ("number", `String "A-1"); ("shop", `Int 7) ])
  in
  Alcotest.(check string) "one text" (Resource.canonical one) (Resource.canonical other);
  Alcotest.(check bool) "one resource" true (Resource.equal one other);
  Alcotest.(check string)
    "the text" {|["tenant-1","Order",{"number":"A-1","shop":7}]|} (Resource.canonical one);
  Alcotest.(check string)
    "the id alone" {|{"number":"A-1","shop":7}|} (Resource.id_text one)

let test_a_string_id_and_a_number_id_are_two_resources () =
  let text = Resource.make ~tenant_id:"t" ~kind:"Order" (`String "1")
  and number = Resource.make ~tenant_id:"t" ~kind:"Order" (`Int 1) in
  Alcotest.(check bool) "two resources" false (Resource.equal text number);
  Alcotest.(check string)
    "text" {|["t","Order","1"]|}
    (Format.asprintf "%a" Resource.pp text);
  Alcotest.(check string)
    "number" {|["t","Order",1]|}
    (Format.asprintf "%a" Resource.pp number)

let test_the_parts_cannot_run_into_each_other () =
  let one = Resource.make ~tenant_id:"a:b" ~kind:"c" (`String "d")
  and other = Resource.make ~tenant_id:"a" ~kind:"b:c" (`String "d") in
  Alcotest.(check bool) "two resources" false (Resource.equal one other)

(* What the reference port's JSON library writes, taken from a run of it, for a string of
   every byte below U+0020, then a quote, a backslash, DEL, a slash and two letters that
   are not ASCII. The escapes are its text as it stands; the last few bytes it leaves as
   they are, and they are spelled here as OCaml spells them. *)
let test_the_text_is_the_reference_port_s_byte_for_byte () =
  let as_the_reference_writes_it =
    {|"\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\b\t\n\u000b\f\r\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f\"\\|}
    ^ "\x7f/\xd0\xb6\xd1\x91\""
  in
  Alcotest.(check string)
    "a string" as_the_reference_writes_it
    (Canonical.to_string
       (`String (String.init 0x20 Char.chr ^ "\"\\\x7f/\xd0\xb6\xd1\x91")));
  Alcotest.(check string)
    "scalars" "[-5,0,true,null]"
    (Canonical.to_string (`List [ `Int (-5); `Int 0; `Bool true; `Null ]))

let test_a_field_given_twice_counts_once () =
  Alcotest.(check string)
    "the last one" {|{"a":2,"b":1}|}
    (Canonical.to_string (`Assoc [ ("a", `Int 1); ("b", `Int 1); ("a", `Int 2) ]))

let test_an_id_built_for_the_json_library_is_accepted_as_it_is () =
  (* An id is a subtype of the JSON library's value: it goes there by coercion. *)
  let id : Resource.id = `Assoc [ ("shop", `Int 7) ] in
  Alcotest.(check string)
    "the same text either way" (Canonical.to_string id)
    (Yojson.Safe.to_string (id :> Yojson.Safe.t))

(* One version. *)

let test_a_versioned_cipher_names_its_version_and_refuses_another () =
  let key = key () in
  let v3 = Versioned_cipher.make (v 3) (cipher key) in
  let sealed = get "encrypt" (Versioned_cipher.encrypt v3 "plain") in
  Alcotest.(check string)
    "four big-endian bytes" "\000\000\000\003"
    (String.sub sealed 0 Key_version.size);
  Alcotest.check bytes "opens its own" (Ok "plain") (Versioned_cipher.decrypt v3 sealed);
  let v4 = Versioned_cipher.make (v 4) (cipher key) in
  Alcotest.check bytes "refuses another version's before trying"
    (Error (Kms_error.Wrong_key_version { expected = 4; found = 3 }))
    (Versioned_cipher.decrypt v4 sealed)

let test_bytes_too_short_to_be_versioned_are_refused () =
  let v1 = Versioned_cipher.make (v 1) (cipher (key ())) in
  Alcotest.(check bool)
    "three bytes" true
    (is_malformed (Versioned_cipher.decrypt v1 "\000\000\001"))

let test_a_versioned_cipher_makes_keys_of_its_kind () =
  let v1 = Versioned_cipher.make (v 1) (cipher (key ())) in
  Alcotest.(check int)
    "thirty-two bytes" 32
    (Key.length (get "generate_key" (Versioned_cipher.generate_key v1)))

let test_a_versioned_cipher_is_a_cipher_to_a_codec () =
  let v2 = Versioned_cipher.make (v 2) (cipher (key ())) in
  let as_cipher = Versioned_cipher.cipher v2 in
  let sealed = get "encrypt" (as_cipher.encrypt "plain") in
  Alcotest.(check string)
    "the version goes along" "\000\000\000\002"
    (String.sub sealed 0 Key_version.size);
  Alcotest.check bytes "and opens" (Ok "plain") (as_cipher.decrypt sealed)

(* Every version. *)

let test_a_keyring_seals_under_the_newest_and_opens_every_version () =
  let k1, k2, k3 = (key (), key (), key ()) in
  let ring =
    Keyring.add
      (Keyring.add (Keyring.make (v 2) (cipher k2)) (v 1) (cipher k1))
      (v 3) (cipher k3)
  in
  Alcotest.(check int) "the newest" 3 (Key_version.to_int (Keyring.newest ring));
  Alcotest.(check (list int))
    "oldest first" [ 1; 2; 3 ]
    (List.map Key_version.to_int (Keyring.versions ring));
  Alcotest.(check string)
    "seals under the newest" "\000\000\000\003"
    (String.sub (get "encrypt" (Keyring.encrypt ring "new")) 0 Key_version.size);
  List.iter
    (fun (version, key) ->
      Alcotest.check bytes
        (Printf.sprintf "version %d" version)
        (Ok "old")
        (Keyring.decrypt ring (sealed_by version key "old")))
    [ (1, k1); (2, k2); (3, k3) ];
  Alcotest.check bytes "a version it has not" (Error (Kms_error.No_key_of_version 9))
    (Keyring.decrypt ring (sealed_by 9 k1 "?"))

let test_a_version_given_again_replaces_the_one_there () =
  let old_key, new_key = (key (), key ()) in
  let sealed_by_old = sealed_by 1 old_key "x" in
  let ring = Keyring.add (Keyring.make (v 1) (cipher old_key)) (v 1) (cipher new_key) in
  Alcotest.(check (list int))
    "one version" [ 1 ]
    (List.map Key_version.to_int (Keyring.versions ring));
  Alcotest.check bytes "the old key is gone" (Error Kms_error.Decrypt)
    (Keyring.decrypt ring sealed_by_old);
  (* The same among the older ones. *)
  let ring =
    Keyring.add
      (Keyring.add (Keyring.make (v 2) (cipher (key ()))) (v 1) (cipher old_key))
      (v 1) (cipher new_key)
  in
  Alcotest.(check (list int))
    "two versions" [ 1; 2 ]
    (List.map Key_version.to_int (Keyring.versions ring));
  Alcotest.check bytes "the old key is gone there too" (Error Kms_error.Decrypt)
    (Keyring.decrypt ring sealed_by_old)

let test_what_a_keyring_seals_its_newest_version_opens () =
  let k = key () in
  let ring = Keyring.add (Keyring.make (v 1) (cipher k)) (v 2) (cipher (key ())) in
  let sealed = get "encrypt" ((Keyring.cipher ring).encrypt "plain") in
  Alcotest.check bytes "version one refuses it"
    (Error (Kms_error.Wrong_key_version { expected = 1; found = 2 }))
    (Versioned_cipher.decrypt (Versioned_cipher.make (v 1) (cipher k)) sealed);
  Alcotest.check bytes "the keyring opens it" (Ok "plain") (Keyring.decrypt ring sealed)

let test_a_keyring_never_shows_a_key () =
  let ring = Keyring.add (Keyring.make (v 1) (cipher (key ()))) (v 2) (cipher (key ())) in
  Alcotest.(check string)
    "the versions alone" "Keyring(versions 1, 2)"
    (Format.asprintf "%a" Keyring.pp ring)

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Dek domain"
    [
      ( "the resource",
        [
          case "a resource has one text however its id was built"
            test_a_resource_has_one_text_however_its_id_was_built;
          case "a string id and a number id are two resources"
            test_a_string_id_and_a_number_id_are_two_resources;
          case "the parts cannot run into each other"
            test_the_parts_cannot_run_into_each_other;
          case "the text is the reference port's, byte for byte"
            test_the_text_is_the_reference_port_s_byte_for_byte;
          case "a field given twice counts once" test_a_field_given_twice_counts_once;
          case "an id built for the JSON library is accepted as it is"
            test_an_id_built_for_the_json_library_is_accepted_as_it_is;
        ] );
      ( "one version",
        [
          case "a versioned cipher names its version and refuses another"
            test_a_versioned_cipher_names_its_version_and_refuses_another;
          case "bytes too short to be versioned are refused"
            test_bytes_too_short_to_be_versioned_are_refused;
          case "a versioned cipher makes keys of its kind"
            test_a_versioned_cipher_makes_keys_of_its_kind;
          case "a versioned cipher is a cipher to a codec"
            test_a_versioned_cipher_is_a_cipher_to_a_codec;
        ] );
      ( "every version",
        [
          case "a keyring seals under the newest and opens every version"
            test_a_keyring_seals_under_the_newest_and_opens_every_version;
          case "a version given again replaces the one there"
            test_a_version_given_again_replaces_the_one_there;
          case "what a keyring seals its newest version opens"
            test_what_a_keyring_seals_its_newest_version_opens;
          case "a keyring never shows a key" test_a_keyring_never_shows_a_key;
        ] );
    ]
