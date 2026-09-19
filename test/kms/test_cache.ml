(** The cache in front of a key management service: what it asks the service, and what it
    answers from memory. *)

open Ascetic_kms
module Memory = Ascetic_session_memory.Memory_session
module Memory_pool = Ascetic_session_memory.Memory_session_pool

(* Counts what it is asked; "unwraps" by taking the wrapped bytes as the key, and "wraps"
   by taking the key's bytes. *)
module Counting = struct
  type t = { mutable unwraps : int; mutable deletes : int }
  type session = Memory.t

  let create () = { unwraps = 0; deletes = 0 }
  let unwraps t = t.unwraps
  let encrypt_dek _ _ ~tenant_id:_ dek = Ok (Key.to_string dek)

  let decrypt_dek t _ ~tenant_id encrypted_dek =
    t.unwraps <- t.unwraps + 1;
    if String.equal tenant_id "gone" then
      Error (Kms_error.Kek_not_found { tenant_id; key_version = None })
    else Ok (Key.of_string encrypted_dek)

  let generate_dek _ _ ~tenant_id:_ =
    Ok (Key.of_string (String.make 32 '\007'), String.make 32 '\007')

  let rotate_kek _ _ ~tenant_id:_ = Ok Key_version.first
  let rewrap_dek _ _ ~tenant_id:_ encrypted_dek = Ok encrypted_dek

  let delete_kek t _ ~tenant_id:_ =
    t.deletes <- t.deletes + 1;
    Ok ()
end

module Cached_counting = Cached.Make (Counting)

let lift error = Kms_error.Session error

let with_session body =
  match
    Memory_pool.session (Memory_pool.create ()) ~lift (fun session -> Ok (body session))
  with
  | Ok value -> value
  | Error e -> Alcotest.failf "session: %a" Kms_error.pp e

let unwrap cached session ~tenant_id wrapped =
  match Cached_counting.decrypt_dek cached session ~tenant_id wrapped with
  | Ok key -> key
  | Error e -> Alcotest.failf "decrypt_dek: %a" Kms_error.pp e

let clock () =
  let clock = Eio_mock.Clock.Mono.make () in
  Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 1L);
  clock

let seconds clock s =
  Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns (Int64.of_float (s *. 1e9)))

let test_the_same_wrapped_key_is_unwrapped_once () =
  let cached = Cached_counting.create ~clock:(clock ()) (Counting.create ()) in
  with_session (fun session ->
      let first = unwrap cached session ~tenant_id:"t1" "wrapped-1" in
      let again = unwrap cached session ~tenant_id:"t1" "wrapped-1" in
      Alcotest.(check bool) "the same key" true (Key.equal first again);
      Alcotest.(check int)
        "asked once" 1
        (Counting.unwraps (Cached_counting.inner cached));
      (* Another wrapped key, and the same bytes for another tenant, are asked. *)
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-2");
      ignore (unwrap cached session ~tenant_id:"t2" "wrapped-1");
      Alcotest.(check int)
        "asked for each" 3
        (Counting.unwraps (Cached_counting.inner cached));
      Alcotest.(check int) "held" 3 (Cached_counting.length cached))

let test_a_zero_time_to_live_keeps_nothing () =
  let cached = Cached_counting.create ~ttl:0.0 ~clock:(clock ()) (Counting.create ()) in
  with_session (fun session ->
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      Alcotest.(check int)
        "asked twice" 2
        (Counting.unwraps (Cached_counting.inner cached));
      Alcotest.(check bool) "nothing held" true (Cached_counting.is_empty cached))

let test_the_oldest_key_goes_when_the_cache_is_full () =
  let cached =
    Cached_counting.create ~capacity:2 ~clock:(clock ()) (Counting.create ())
  in
  let unwraps () = Counting.unwraps (Cached_counting.inner cached) in
  with_session (fun session ->
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-2");
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-3");
      Alcotest.(check int) "two held" 2 (Cached_counting.length cached);
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-3");
      Alcotest.(check int) "the newest is still held" 3 (unwraps ());
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      Alcotest.(check int) "the oldest was let go" 4 (unwraps ()))

let test_a_key_is_asked_for_again_after_its_time_to_live () =
  let clock = clock () in
  let cached = Cached_counting.create ~ttl:60.0 ~clock (Counting.create ()) in
  let unwraps () = Counting.unwraps (Cached_counting.inner cached) in
  with_session (fun session ->
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      seconds clock 59.0;
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      Alcotest.(check int) "still held a minute short" 1 (unwraps ());
      seconds clock 61.0;
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      Alcotest.(check int) "asked again" 2 (unwraps ());
      Alcotest.(check int) "the expired one made room" 1 (Cached_counting.length cached))

let test_deleting_a_tenant_s_kek_forgets_its_keys_here () =
  let cached = Cached_counting.create ~clock:(clock ()) (Counting.create ()) in
  with_session (fun session ->
      ignore (unwrap cached session ~tenant_id:"t1" "wrapped-1");
      ignore (unwrap cached session ~tenant_id:"t2" "wrapped-1");
      Alcotest.(check bool)
        "deleted" true
        (Cached_counting.delete_kek cached session ~tenant_id:"t1" = Ok ());
      Alcotest.(check int) "one held" 1 (Cached_counting.length cached);
      ignore (unwrap cached session ~tenant_id:"t2" "wrapped-1");
      Alcotest.(check int)
        "the other tenant's key is still held" 2
        (Counting.unwraps (Cached_counting.inner cached)))

let test_a_refusal_is_not_kept () =
  let cached = Cached_counting.create ~clock:(clock ()) (Counting.create ()) in
  with_session (fun session ->
      let refused () =
        Result.is_error
          (Cached_counting.decrypt_dek cached session ~tenant_id:"gone" "wrapped-1")
      in
      Alcotest.(check bool) "refused" true (refused ());
      Alcotest.(check bool) "refused again" true (refused ());
      Alcotest.(check int)
        "asked twice" 2
        (Counting.unwraps (Cached_counting.inner cached));
      Alcotest.(check bool) "nothing held" true (Cached_counting.is_empty cached))

let test_everything_else_passes_through () =
  let cached = Cached_counting.create ~clock:(clock ()) (Counting.create ()) in
  with_session (fun session ->
      let dek = Key.of_string "a key" in
      Alcotest.(check bool)
        "encrypt_dek" true
        (Cached_counting.encrypt_dek cached session ~tenant_id:"t1" dek = Ok "a key");
      Alcotest.(check bool)
        "rewrap_dek" true
        (Cached_counting.rewrap_dek cached session ~tenant_id:"t1" "w" = Ok "w");
      Alcotest.(check bool)
        "rotate_kek" true
        (Cached_counting.rotate_kek cached session ~tenant_id:"t1" = Ok Key_version.first);
      Alcotest.(check bool)
        "generate_dek" true
        (Result.is_ok (Cached_counting.generate_dek cached session ~tenant_id:"t1"));
      Alcotest.(check bool) "nothing held" true (Cached_counting.is_empty cached))

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Cached"
    [
      ( "cache",
        [
          case "the same wrapped key is unwrapped once"
            test_the_same_wrapped_key_is_unwrapped_once;
          case "a zero time to live keeps nothing" test_a_zero_time_to_live_keeps_nothing;
          case "the oldest key goes when the cache is full"
            test_the_oldest_key_goes_when_the_cache_is_full;
          case "a key is asked for again after its time to live"
            test_a_key_is_asked_for_again_after_its_time_to_live;
          case "deleting a tenant's kek forgets its keys here"
            test_deleting_a_tenant_s_kek_forgets_its_keys_here;
          case "a refusal is not kept" test_a_refusal_is_not_kept;
          case "everything else passes through" test_everything_else_passes_through;
        ] );
    ]
