(** Tests for [Work_item_arguments]. *)

module Args = Ascetic_saga.Work_item_arguments

let test_create_empty () =
  let args = Args.empty in
  Alcotest.(check int) "empty length" 0 (List.length (Args.to_list args))

let test_create_with_data () =
  let args = Args.of_list [
    "vehicleType", `String "Compact";
    "days", `Int 5;
  ] in
  Alcotest.(check (option string))
    "vehicleType"
    (Some "\"Compact\"")
    (Option.map Yojson.Safe.to_string (Args.find args "vehicleType"));
  Alcotest.(check (option string))
    "days"
    (Some "5")
    (Option.map Yojson.Safe.to_string (Args.find args "days"))

let test_add_and_find () =
  let args = Args.add Args.empty "destination" (`String "Paris") in
  Alcotest.(check (option string))
    "destination"
    (Some "\"Paris\"")
    (Option.map Yojson.Safe.to_string (Args.find args "destination"))

let test_find_missing_key () =
  let args = Args.of_list [ "a", `Int 1 ] in
  Alcotest.(check bool)
    "missing key returns None"
    true
    (Args.find args "missing" = None)

let test_find_exn_missing_key_raises () =
  let args = Args.of_list [ "a", `Int 1 ] in
  Alcotest.check_raises
    "find_exn raises Invalid_argument"
    (Invalid_argument "Work_item_arguments.find_exn: missing key \"missing\"")
    (fun () -> ignore (Args.find_exn args "missing"))

let () =
  Alcotest.run "Work_item_arguments"
    [
      ( "construction",
        [
          Alcotest.test_case "empty" `Quick test_create_empty;
          Alcotest.test_case "with data" `Quick test_create_with_data;
        ] );
      ( "lookup",
        [
          Alcotest.test_case "add and find" `Quick test_add_and_find;
          Alcotest.test_case "find missing returns None" `Quick test_find_missing_key;
          Alcotest.test_case "find_exn missing raises" `Quick
            test_find_exn_missing_key_raises;
        ] );
    ]
