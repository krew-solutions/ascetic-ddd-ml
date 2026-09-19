(** The primitive, checked against what the Python port sealed. *)

open Ascetic_kms

(* Made with the Python port's [cryptography.hazmat] AES-GCM: key [00 01 .. 1f], nonce
   [a0 a1 .. ab], associated data [tenant-1], plaintext [the quick brown fox]. *)
let sealed_by_python =
  "a0a1a2a3a4a5a6a7a8a9aaab9270190d34be6bdc0945e5a1680daefe16c321dd95c879131f961ff6e0cb3098c8aa0a"

let hex text =
  String.init
    (String.length text / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * i) 2)))

let known_key () = Key.of_string (String.init 32 Char.chr)
let known_nonce = String.init 12 (fun i -> Char.chr (0xa0 + i))
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let bytes = Alcotest.(result string error)

let cipher ~aad =
  match Aes256gcm.make (known_key ()) ~aad with
  | Ok cipher -> cipher
  | Error e -> Alcotest.failf "cipher: %a" Kms_error.pp e

let is_malformed = function Error (Kms_error.Malformed _) -> true | _ -> false

let test_seals_what_the_python_port_sealed () =
  Alcotest.check bytes "the same bytes"
    (Ok (hex sealed_by_python))
    (Aes256gcm.Known_answer.seal (cipher ~aad:"tenant-1") ~nonce:known_nonce
       "the quick brown fox")

let test_opens_what_the_python_port_sealed () =
  Alcotest.check bytes "the plaintext" (Ok "the quick brown fox")
    (Aes256gcm.decrypt (cipher ~aad:"tenant-1") (hex sealed_by_python))

let test_another_tenant_does_not_open_it () =
  Alcotest.check bytes "refused" (Error Kms_error.Decrypt)
    (Aes256gcm.decrypt (cipher ~aad:"tenant-2") (hex sealed_by_python))

let test_a_changed_byte_does_not_open () =
  let sealed = Bytes.of_string (hex sealed_by_python) in
  let at = Aes256gcm.nonce_size in
  Bytes.set sealed at (Char.chr (Char.code (Bytes.get sealed at) lxor 1));
  Alcotest.check bytes "refused" (Error Kms_error.Decrypt)
    (Aes256gcm.decrypt (cipher ~aad:"tenant-1") (Bytes.to_string sealed))

let test_bytes_too_short_to_be_sealed_are_malformed () =
  let cipher = cipher ~aad:"tenant-1" in
  List.iter
    (fun length ->
      Alcotest.(check bool)
        (Printf.sprintf "%d bytes" length)
        true
        (is_malformed (Aes256gcm.decrypt cipher (String.make length '\000'))))
    [ 0; Aes256gcm.nonce_size; Aes256gcm.nonce_size + Aes256gcm.tag_size - 1 ]

let test_a_key_of_the_wrong_length_is_refused () =
  Alcotest.(check bool)
    "sixteen bytes, which the library would take as AES-128" true
    (is_malformed
       (Result.map ignore
          (Aes256gcm.make (Key.of_string (String.make 16 '\000')) ~aad:"tenant-1")));
  Alcotest.(check bool)
    "thirty-one bytes" true
    (is_malformed
       (Result.map ignore
          (Algorithm.cipher Algorithm.Aes_256_gcm
             (Key.of_string (String.make 31 '\000'))
             ~aad:"tenant-1")))

let test_a_nonce_of_the_wrong_length_is_refused () =
  Alcotest.(check bool)
    "eleven bytes" true
    (is_malformed
       (Aes256gcm.Known_answer.seal (cipher ~aad:"tenant-1")
          ~nonce:(String.make 11 '\000') "x"))

let test_the_algorithm_name_round_trips_through_storage () =
  Alcotest.(check bool)
    "round trip" true
    (Algorithm.of_string (Algorithm.to_string Algorithm.Aes_256_gcm)
    = Ok Algorithm.Aes_256_gcm);
  Alcotest.(check bool)
    "another name" true
    (Algorithm.of_string "AES-128-GCM"
    = Error (Kms_error.Unsupported_algorithm "AES-128-GCM"))

let test_the_sizes_are_those_of_aes_256_gcm () =
  Alcotest.(check (list int))
    "key, nonce, tag" [ 32; 12; 16 ]
    [ Aes256gcm.key_size; Aes256gcm.nonce_size; Aes256gcm.tag_size ];
  Alcotest.(check bool)
    "the library takes a key of that size" true
    (Array.mem Aes256gcm.key_size Mirage_crypto.AES.GCM.key_sizes)

let test_a_cipher_makes_keys_of_its_own_kind () =
  let cipher = Aes256gcm.cipher (cipher ~aad:"tenant-1") in
  match cipher.generate_key () with
  | Error e -> Alcotest.failf "generate_key: %a" Kms_error.pp e
  | Ok key ->
      Alcotest.(check int) "its size" Aes256gcm.key_size (Key.length key);
      Alcotest.(check bool) "not the cipher's own" false (Key.equal key (known_key ()));
      Alcotest.(check bool)
        "fit for a cipher of the kind" true
        (Result.is_ok (Aes256gcm.make key ~aad:"tenant-1"))

let test_sealing_twice_gives_two_sealed_forms_that_both_open () =
  let cipher = cipher ~aad:"tenant-1" in
  let one = Aes256gcm.encrypt cipher "plain"
  and other = Aes256gcm.encrypt cipher "plain" in
  Alcotest.(check bool) "a fresh nonce each time" false (one = other);
  List.iter
    (fun sealed ->
      Alcotest.check bytes "opens" (Ok "plain")
        (Result.bind sealed (Aes256gcm.decrypt cipher)))
    [ one; other ]

let test_a_key_never_shows_its_bytes () =
  Alcotest.(check string)
    "the length alone" "Key(32 bytes)"
    (Format.asprintf "%a" Key.pp (known_key ()))

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Aes256gcm"
    [
      ( "cipher",
        [
          case "seals what the python port sealed" test_seals_what_the_python_port_sealed;
          case "opens what the python port sealed" test_opens_what_the_python_port_sealed;
          case "another tenant does not open it" test_another_tenant_does_not_open_it;
          case "a changed byte does not open" test_a_changed_byte_does_not_open;
          case "bytes too short to be sealed are malformed"
            test_bytes_too_short_to_be_sealed_are_malformed;
          case "a key of the wrong length is refused"
            test_a_key_of_the_wrong_length_is_refused;
          case "a nonce of the wrong length is refused"
            test_a_nonce_of_the_wrong_length_is_refused;
          case "the algorithm name round trips through storage"
            test_the_algorithm_name_round_trips_through_storage;
          case "the sizes are those of AES-256-GCM"
            test_the_sizes_are_those_of_aes_256_gcm;
          case "a cipher makes keys of its own kind"
            test_a_cipher_makes_keys_of_its_own_kind;
          case "sealing twice gives two sealed forms that both open"
            test_sealing_twice_gives_two_sealed_forms_that_both_open;
          case "a key never shows its bytes" test_a_key_never_shows_its_bytes;
        ] );
    ]
