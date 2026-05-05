(** Tests for [Work_item]. *)

open Test_helpers

let test_create_work_item () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Stub" () in
  let args = Args.of_list [ "vehicleType", `String "SUV" ] in
  let wi = WI.create ~factory ~arguments:args in
  Alcotest.(check (option string))
    "vehicleType"
    (Some "\"SUV\"")
    (Option.map Yojson.Safe.to_string (Args.find (WI.arguments wi) "vehicleType"));
  let act = WI.activity wi in
  Alcotest.(check string) "activity name" "Stub" (S.Activity.name act)

let test_arguments_are_accessible () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Stub" () in
  let args = Args.of_list [ "a", `Int 1; "b", `Int 2; "c", `Int 3 ] in
  let wi = WI.create ~factory ~arguments:args in
  Alcotest.(check (option string))
    "a"
    (Some "1")
    (Option.map Yojson.Safe.to_string (Args.find (WI.arguments wi) "a"));
  Alcotest.(check (option string))
    "b"
    (Some "2")
    (Option.map Yojson.Safe.to_string (Args.find (WI.arguments wi) "b"));
  Alcotest.(check (option string))
    "c"
    (Some "3")
    (Option.map Yojson.Safe.to_string (Args.find (WI.arguments wi) "c"))

let test_factory_returns_consistent_activity () =
  (* Each invocation of stub_factory creates a fresh activity, but the
     name and queue addresses are stable. *)
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Stub" () in
  let wi = WI.create ~factory ~arguments:Args.empty in
  let a = WI.activity wi in
  let b = WI.activity wi in
  Alcotest.(check string) "name a" "Stub" (S.Activity.name a);
  Alcotest.(check string) "name b" "Stub" (S.Activity.name b);
  Alcotest.(check string)
    "work queue stable"
    (S.Activity.work_item_queue_address a)
    (S.Activity.work_item_queue_address b)

let () =
  Alcotest.run "Work_item"
    [
      ( "construction",
        [
          Alcotest.test_case "stores factory and arguments" `Quick
            test_create_work_item;
          Alcotest.test_case "arguments accessible" `Quick
            test_arguments_are_accessible;
          Alcotest.test_case "factory yields consistent activity name" `Quick
            test_factory_returns_consistent_activity;
        ] );
    ]
