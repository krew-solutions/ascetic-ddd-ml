(** Tests for [Activity_host]. *)

open Test_helpers

(* accept_message ------------------------------------------------------- *)

let test_accept_work_item_message () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"A1" () in
  let host = S.Activity_host.create ~factory ~send:(fun _ _ -> ()) in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check bool)
    "accepts own work-item queue"
    true
    (S.Activity_host.accept_message host ~uri:"sb://./A1" rs)

let test_accept_compensation_message () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"A1" () in
  let host = S.Activity_host.create ~factory ~send:(fun _ _ -> ()) in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  let _ = RS.process_next rs in
  Alcotest.(check bool)
    "accepts own compensation queue"
    true
    (S.Activity_host.accept_message host ~uri:"sb://./A1Compensation" rs)

let test_reject_unknown_uri () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"A1" () in
  let host = S.Activity_host.create ~factory ~send:(fun _ _ -> ()) in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  Alcotest.(check bool)
    "rejects unknown URI"
    false
    (S.Activity_host.accept_message host ~uri:"sb://./unknown" rs)

let test_reject_other_activity_uri () =
  let counters_one = make_counters () in
  let factory_one = stub_factory ~counters:counters_one ~name:"A1" () in
  let host = S.Activity_host.create ~factory:factory_one ~send:(fun _ _ -> ()) in
  let rs =
    RS.create ~work_items:[ WI.create ~factory:factory_one ~arguments:Args.empty ] ()
  in
  Alcotest.(check bool)
    "rejects other activity's URI"
    false
    (S.Activity_host.accept_message host ~uri:"sb://./A2" rs)

(* process_forward_message --------------------------------------------- *)

let test_forward_success_continues_forward () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"A1" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"A2" () in
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let host = S.Activity_host.create ~factory:factory_a ~send in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_b ~arguments:Args.empty;
      ]
      ()
  in
  S.Activity_host.process_forward_message host rs;
  Alcotest.(check int) "1 dispatch" 1 (List.length !dispatched);
  match !dispatched with
  | [ (uri, _) ] ->
    Alcotest.(check string) "next forward queue" "sb://./A2" uri
  | _ -> Alcotest.fail "unexpected dispatch list"

let test_forward_failure_starts_compensation () =
  let counters_a = make_counters () in
  let counters_fail = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"A1" () in
  let factory_fail =
    stub_factory ~should_succeed:false ~counters:counters_fail ~name:"Fail" ()
  in
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let host = S.Activity_host.create ~factory:factory_fail ~send in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_fail ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in (* complete A1 *)
  S.Activity_host.process_forward_message host rs;
  Alcotest.(check int) "1 dispatch" 1 (List.length !dispatched);
  match !dispatched with
  | [ (uri, _) ] ->
    Alcotest.(check string)
      "compensation queue of A1" "sb://./A1Compensation" uri
  | _ -> Alcotest.fail "unexpected dispatch list"

let test_forward_completed_does_nothing () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"A1" () in
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let host = S.Activity_host.create ~factory ~send in
  let rs = RS.create () in
  S.Activity_host.process_forward_message host rs;
  Alcotest.(check int) "no dispatch" 0 (List.length !dispatched)

(* process_backward_message -------------------------------------------- *)

let test_backward_continues_backward () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"A1" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"A2" () in
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let host = S.Activity_host.create ~factory:factory_b ~send in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_b ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  let _ = RS.process_next rs in
  S.Activity_host.process_backward_message host rs;
  Alcotest.(check int) "1 dispatch" 1 (List.length !dispatched);
  match !dispatched with
  | [ (uri, _) ] ->
    Alcotest.(check string)
      "previous compensation queue" "sb://./A1Compensation" uri
  | _ -> Alcotest.fail "unexpected dispatch list"

let test_backward_not_in_progress_does_nothing () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"A1" () in
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let host = S.Activity_host.create ~factory ~send in
  let rs = RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] () in
  S.Activity_host.process_backward_message host rs;
  Alcotest.(check int) "no dispatch" 0 (List.length !dispatched)

(* full distributed saga ----------------------------------------------- *)

let test_distributed_saga_success () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"A1" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"A2" () in
  let messages : (string * RS.t) list ref = ref [] in
  let send uri rs = messages := !messages @ [ (uri, rs) ] in
  let host_a = S.Activity_host.create ~factory:factory_a ~send in
  let host_b = S.Activity_host.create ~factory:factory_b ~send in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_b ~arguments:Args.empty;
      ]
      ()
  in
  (match RS.progress_uri rs with
   | Some uri -> send uri rs
   | None -> ());
  let rec drain () =
    match !messages with
    | [] -> ()
    | (uri, rs) :: rest ->
      messages := rest;
      let _ = S.Activity_host.accept_message host_a ~uri rs
              || S.Activity_host.accept_message host_b ~uri rs in
      drain ()
  in
  drain ();
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check int) "A1 called" 1 counters_a.call_count;
  Alcotest.(check int) "A2 called" 1 counters_b.call_count

let test_distributed_saga_with_compensation () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let counters_fail = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"A1" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"A2" () in
  let factory_fail =
    stub_factory ~should_succeed:false ~counters:counters_fail ~name:"Fail" ()
  in
  let messages : (string * RS.t) list ref = ref [] in
  let send uri rs = messages := !messages @ [ (uri, rs) ] in
  let host_a = S.Activity_host.create ~factory:factory_a ~send in
  let host_b = S.Activity_host.create ~factory:factory_b ~send in
  let host_f = S.Activity_host.create ~factory:factory_fail ~send in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_b ~arguments:Args.empty;
        WI.create ~factory:factory_fail ~arguments:Args.empty;
      ]
      ()
  in
  (match RS.progress_uri rs with
   | Some uri -> send uri rs
   | None -> ());
  let rec drain () =
    match !messages with
    | [] -> ()
    | (uri, rs) :: rest ->
      messages := rest;
      let _ = S.Activity_host.accept_message host_a ~uri rs
              || S.Activity_host.accept_message host_b ~uri rs
              || S.Activity_host.accept_message host_f ~uri rs in
      drain ()
  in
  drain ();
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs);
  Alcotest.(check int) "A1 called" 1 counters_a.call_count;
  Alcotest.(check int) "A2 called" 1 counters_b.call_count;
  Alcotest.(check int) "A1 compensated" 1 counters_a.compensate_count;
  Alcotest.(check int) "A2 compensated" 1 counters_b.compensate_count

let () =
  Alcotest.run "Activity_host"
    [
      ( "accept_message",
        [
          Alcotest.test_case "own work queue" `Quick test_accept_work_item_message;
          Alcotest.test_case "own compensation queue" `Quick
            test_accept_compensation_message;
          Alcotest.test_case "rejects unknown" `Quick test_reject_unknown_uri;
          Alcotest.test_case "rejects other activity" `Quick
            test_reject_other_activity_uri;
        ] );
      ( "process_forward_message",
        [
          Alcotest.test_case "success forwards" `Quick
            test_forward_success_continues_forward;
          Alcotest.test_case "failure starts compensation" `Quick
            test_forward_failure_starts_compensation;
          Alcotest.test_case "completed does nothing" `Quick
            test_forward_completed_does_nothing;
        ] );
      ( "process_backward_message",
        [
          Alcotest.test_case "continues backward" `Quick
            test_backward_continues_backward;
          Alcotest.test_case "not in progress no-op" `Quick
            test_backward_not_in_progress_does_nothing;
        ] );
      ( "full saga",
        [
          Alcotest.test_case "distributed success" `Quick
            test_distributed_saga_success;
          Alcotest.test_case "distributed compensation" `Quick
            test_distributed_saga_with_compensation;
        ] );
    ]
