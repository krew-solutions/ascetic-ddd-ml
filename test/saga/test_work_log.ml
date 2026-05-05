(** Tests for [Work_log]. *)

open Test_helpers

let test_create_work_log () =
  let counters = make_counters () in
  let activity = make_stub_activity ~counters ~name:"Stub" () in
  let result = Res.of_list [ "reservationId", `Int 12345 ] in
  let log = WL.create ~activity ~result in
  Alcotest.(check (option string))
    "reservationId"
    (Some "12345")
    (Option.map Yojson.Safe.to_string (Res.find (WL.result log) "reservationId"));
  Alcotest.(check string)
    "activity name"
    "Stub"
    (S.Activity.name (WL.activity log))

let test_create_with_factory () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Stub" () in
  let result = Res.of_list [ "id", `Int 1 ] in
  let log = WL.create_with_factory ~factory ~result in
  Alcotest.(check string) "name" "Stub" (S.Activity.name (WL.activity log));
  Alcotest.(check (option string))
    "id"
    (Some "1")
    (Option.map Yojson.Safe.to_string (Res.find (WL.result log) "id"))

let test_result_is_accessible () =
  let counters = make_counters () in
  let activity = make_stub_activity ~counters ~name:"Stub" () in
  let result = Res.of_list [ "key", `String "value"; "count", `Int 42 ] in
  let log = WL.create ~activity ~result in
  Alcotest.(check (option string))
    "key"
    (Some "\"value\"")
    (Option.map Yojson.Safe.to_string (Res.find (WL.result log) "key"));
  Alcotest.(check (option string))
    "count"
    (Some "42")
    (Option.map Yojson.Safe.to_string (Res.find (WL.result log) "count"))

let () =
  Alcotest.run "Work_log"
    [
      ( "construction",
        [
          Alcotest.test_case "from activity" `Quick test_create_work_log;
          Alcotest.test_case "from factory" `Quick test_create_with_factory;
          Alcotest.test_case "result accessible" `Quick
            test_result_is_accessible;
        ] );
    ]
