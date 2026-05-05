(** Tests for [Routing_slip]. *)

open Test_helpers

(* --- Creation ----------------------------------------------------------- *)

let test_create_empty () =
  let rs = RS.create () in
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "not in progress" false (RS.is_in_progress rs)

let test_create_with_work_items () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory ~arguments:(Args.of_list [ "a", `Int 1 ]);
        WI.create ~factory ~arguments:(Args.of_list [ "b", `Int 2 ]);
      ]
      ()
  in
  Alcotest.(check bool) "not completed" false (RS.is_completed rs);
  Alcotest.(check bool) "not in progress yet" false (RS.is_in_progress rs)

(* --- process_next ------------------------------------------------------- *)

let test_process_next_success () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check bool) "process_next true" true (RS.process_next rs);
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "in progress" true (RS.is_in_progress rs)

let test_process_next_failure () =
  let counters = make_counters () in
  let factory = stub_factory ~should_succeed:false ~counters ~name:"Fail" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check bool) "process_next false" false (RS.process_next rs);
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "no logs" false (RS.is_in_progress rs)

let test_process_next_on_empty_raises () =
  let rs = RS.create () in
  Alcotest.check_raises
    "Invalid_operation on empty"
    (RS.Invalid_operation "No more work items to process")
    (fun () -> ignore (RS.process_next rs))

let test_process_multiple_items () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  Alcotest.(check int)
    "1 log after first" 1 (List.length (RS.completed_work_logs rs));
  let _ = RS.process_next rs in
  Alcotest.(check int)
    "2 logs after second" 2 (List.length (RS.completed_work_logs rs));
  let _ = RS.process_next rs in
  Alcotest.(check int)
    "3 logs after third" 3 (List.length (RS.completed_work_logs rs));
  Alcotest.(check bool) "completed" true (RS.is_completed rs)

(* --- undo_last ---------------------------------------------------------- *)

let test_undo_last_success () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  let _ = RS.process_next rs in
  Alcotest.(check bool) "undo_last true" true (RS.undo_last rs);
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs);
  Alcotest.(check int) "compensate_count = 1" 1 counters.compensate_count

let test_undo_last_on_empty_raises () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.check_raises
    "Invalid_operation on non-started slip"
    (RS.Invalid_operation "No work to undo")
    (fun () -> ignore (RS.undo_last rs))

let test_undo_multiple_items () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  let _ = RS.process_next rs in
  let _ = RS.process_next rs in
  Alcotest.(check int) "3 logs" 3 (List.length (RS.completed_work_logs rs));
  let _ = RS.undo_last rs in
  Alcotest.(check int) "2 logs after first undo" 2
    (List.length (RS.completed_work_logs rs));
  let _ = RS.undo_last rs in
  Alcotest.(check int) "1 log after second undo" 1
    (List.length (RS.completed_work_logs rs));
  let _ = RS.undo_last rs in
  Alcotest.(check int) "0 logs after third undo" 0
    (List.length (RS.completed_work_logs rs));
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs)

(* --- URI properties ----------------------------------------------------- *)

let test_progress_uri_returns_next_activity_queue () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check (option string))
    "progress_uri before processing"
    (Some "sb://./Success")
    (RS.progress_uri rs)

let test_progress_uri_returns_none_when_completed () =
  let rs = RS.create () in
  Alcotest.(check (option string)) "None when completed" None (RS.progress_uri rs)

let test_compensation_uri_returns_last_completed_queue () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  let _ = RS.process_next rs in
  Alcotest.(check (option string))
    "compensation_uri after first processed"
    (Some "sb://./SuccessCompensation")
    (RS.compensation_uri rs)

let test_compensation_uri_returns_none_when_not_started () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check (option string)) "None when not started" None
    (RS.compensation_uri rs)

(* --- Full saga integration --------------------------------------------- *)

let test_successful_saga () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Success" () in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
        WI.create ~factory ~arguments:Args.empty;
      ]
      ()
  in
  while not (RS.is_completed rs) do
    let _ = RS.process_next rs in ()
  done;
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "in progress" true (RS.is_in_progress rs);
  Alcotest.(check int) "3 logs" 3 (List.length (RS.completed_work_logs rs))

let test_failed_saga_with_compensation () =
  let counters_ok = make_counters () in
  let counters_fail = make_counters () in
  let factory_ok = stub_factory ~counters:counters_ok ~name:"Success" () in
  let factory_fail =
    stub_factory ~should_succeed:false ~counters:counters_fail ~name:"Fail" ()
  in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_ok ~arguments:Args.empty;
        WI.create ~factory:factory_ok ~arguments:Args.empty;
        WI.create ~factory:factory_fail ~arguments:Args.empty;
      ]
      ()
  in
  let aborted = ref false in
  while (not !aborted) && (not (RS.is_completed rs)) do
    if not (RS.process_next rs) then aborted := true
  done;
  while RS.is_in_progress rs do
    let _ = RS.undo_last rs in ()
  done;
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs);
  Alcotest.(check int) "2 ok-step compensations" 2 counters_ok.compensate_count

let () =
  Alcotest.run "Routing_slip"
    [
      ( "creation",
        [
          Alcotest.test_case "empty" `Quick test_create_empty;
          Alcotest.test_case "with work items" `Quick test_create_with_work_items;
        ] );
      ( "process_next",
        [
          Alcotest.test_case "success" `Quick test_process_next_success;
          Alcotest.test_case "failure" `Quick test_process_next_failure;
          Alcotest.test_case "empty raises" `Quick test_process_next_on_empty_raises;
          Alcotest.test_case "multiple in order" `Quick test_process_multiple_items;
        ] );
      ( "undo_last",
        [
          Alcotest.test_case "success" `Quick test_undo_last_success;
          Alcotest.test_case "non-started raises" `Quick
            test_undo_last_on_empty_raises;
          Alcotest.test_case "multiple in reverse" `Quick test_undo_multiple_items;
        ] );
      ( "uri",
        [
          Alcotest.test_case "progress_uri next queue" `Quick
            test_progress_uri_returns_next_activity_queue;
          Alcotest.test_case "progress_uri None when completed" `Quick
            test_progress_uri_returns_none_when_completed;
          Alcotest.test_case "compensation_uri last queue" `Quick
            test_compensation_uri_returns_last_completed_queue;
          Alcotest.test_case "compensation_uri None when not started" `Quick
            test_compensation_uri_returns_none_when_not_started;
        ] );
      ( "full saga",
        [
          Alcotest.test_case "all succeed" `Quick test_successful_saga;
          Alcotest.test_case "failure triggers compensation" `Quick
            test_failed_saga_with_compensation;
        ] );
    ]
