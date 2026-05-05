(** Tests for [Work_result]. *)

module Res = Ascetic_saga.Work_result

let test_create_empty () =
  let r = Res.empty in
  Alcotest.(check int) "empty length" 0 (List.length (Res.to_list r))

let test_create_with_data () =
  let r = Res.of_list [
    "reservationId", `Int 12345;
    "status", `String "confirmed";
  ] in
  Alcotest.(check (option string))
    "reservationId"
    (Some "12345")
    (Option.map Yojson.Safe.to_string (Res.find r "reservationId"));
  Alcotest.(check (option string))
    "status"
    (Some "\"confirmed\"")
    (Option.map Yojson.Safe.to_string (Res.find r "status"))

let test_add_and_find () =
  let r = Res.add Res.empty "key" (`String "value") in
  Alcotest.(check (option string))
    "key"
    (Some "\"value\"")
    (Option.map Yojson.Safe.to_string (Res.find r "key"))

let test_find_exn_missing_raises () =
  let r = Res.of_list [ "a", `Int 1 ] in
  Alcotest.check_raises
    "find_exn raises Invalid_argument"
    (Invalid_argument "Work_result.find_exn: missing key \"missing\"")
    (fun () -> ignore (Res.find_exn r "missing"))

let test_concat_results () =
  let r = Res.of_list [ "a", `Int 1 ] in
  let r = Res.add r "b" (`Int 2) in
  let r = Res.add r "c" (`Int 3) in
  Alcotest.(check int) "size" 3 (List.length (Res.to_list r));
  Alcotest.(check (option string))
    "a preserved"
    (Some "1")
    (Option.map Yojson.Safe.to_string (Res.find r "a"));
  Alcotest.(check (option string))
    "c added"
    (Some "3")
    (Option.map Yojson.Safe.to_string (Res.find r "c"))

let () =
  Alcotest.run "Work_result"
    [
      ( "construction",
        [
          Alcotest.test_case "empty" `Quick test_create_empty;
          Alcotest.test_case "with data" `Quick test_create_with_data;
        ] );
      ( "lookup",
        [
          Alcotest.test_case "add and find" `Quick test_add_and_find;
          Alcotest.test_case "find_exn missing raises" `Quick
            test_find_exn_missing_raises;
          Alcotest.test_case "accumulate" `Quick test_concat_results;
        ] );
    ]
