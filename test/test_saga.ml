module S = Ascetic_saga
module RS = S.Routing_slip
module WI = S.Work_item
module WL = S.Work_log
module Args = S.Work_item_arguments
module Res = S.Work_result
module Resolver = S.Activity_resolver
module Ser = S.Serializable

let make_activity ?(do_work_returns_log = true) ~name () =
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () -> Option.get !activity_ref
  in
  let do_work _wi =
    if do_work_returns_log then
      Some
        (WL.create_with_factory
           ~factory
           ~result:(Res.of_list [ "ok", `Bool true ]))
    else None
  in
  let compensate _wl _rs = true in
  let activity =
    S.Activity.create
      ~name
      ~do_work
      ~compensate
      ~work_item_queue_address:("sb://./" ^ name)
      ~compensation_queue_address:("sb://./" ^ name ^ "Compensation")
  in
  activity_ref := Some activity;
  activity

let factory_for ?do_work_returns_log ~name () : S.Saga_types.factory =
  fun () -> make_activity ?do_work_returns_log ~name ()

let test_create_empty () =
  let rs = RS.create () in
  Alcotest.(check bool) "completed when empty" true (RS.is_completed rs);
  Alcotest.(check bool) "not in progress" false (RS.is_in_progress rs)

let test_process_next_success () =
  let factory = factory_for ~name:"A" () in
  let item =
    WI.create
      ~factory
      ~arguments:(Args.of_list [ "x", `Int 1 ])
  in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "process_next ok" true (RS.process_next rs);
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "in progress" true (RS.is_in_progress rs);
  Alcotest.(check int)
    "logs len" 1 (List.length (RS.completed_work_logs rs))

let test_process_next_failure () =
  let factory = factory_for ~do_work_returns_log:false ~name:"B" () in
  let item =
    WI.create ~factory ~arguments:Args.empty
  in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "process_next fail" false (RS.process_next rs);
  Alcotest.(check bool) "no logs" false (RS.is_in_progress rs)

let test_progress_uri () =
  let factory = factory_for ~name:"A" () in
  let item = WI.create ~factory ~arguments:Args.empty in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check (option string))
    "progress_uri before processing"
    (Some "sb://./A")
    (RS.progress_uri rs);
  let _ = RS.process_next rs in
  Alcotest.(check (option string))
    "progress_uri after processing" None (RS.progress_uri rs);
  Alcotest.(check (option string))
    "compensation_uri after processing"
    (Some "sb://./ACompensation")
    (RS.compensation_uri rs)

let test_undo_last () =
  let factory = factory_for ~name:"A" () in
  let item = WI.create ~factory ~arguments:Args.empty in
  let rs = RS.create ~work_items:[ item ] () in
  let _ = RS.process_next rs in
  Alcotest.(check bool) "undo result" true (RS.undo_last rs);
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs)

let test_serialize_roundtrip () =
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register
    mb
    ~name:"A"
    ~factory:(factory_for ~name:"A" ());
  Resolver.Map_based.register
    mb
    ~name:"B"
    ~factory:(factory_for ~name:"B" ());
  let resolver = Resolver.Map_based.to_resolver mb in
  let item_a =
    WI.create
      ~factory:(factory_for ~name:"A" ())
      ~arguments:(Args.of_list [ "k", `String "v" ])
  in
  let item_b =
    WI.create
      ~factory:(factory_for ~name:"B" ())
      ~arguments:(Args.of_list [ "n", `Int 7 ])
  in
  let rs = RS.create ~work_items:[ item_a; item_b ] () in
  let _ = RS.process_next rs in
  match Ser.to_serializable rs resolver with
  | Error e -> Alcotest.failf "to_serializable: %s" e
  | Ok srs ->
    let json = Ser.to_string srs in
    (match Ser.of_string json with
     | Error e -> Alcotest.failf "of_string: %s" e
     | Ok srs2 ->
       Alcotest.(check int)
         "logs preserved"
         (List.length srs.completed_work_logs)
         (List.length srs2.completed_work_logs);
       Alcotest.(check int)
         "items preserved"
         (List.length srs.next_work_items)
         (List.length srs2.next_work_items);
       (match Ser.from_serializable srs2 resolver with
        | Error e -> Alcotest.failf "from_serializable: %s" e
        | Ok rs2 ->
          Alcotest.(check int)
            "logs after rebuild"
            1
            (List.length (RS.completed_work_logs rs2));
          Alcotest.(check int)
            "items after rebuild"
            1
            (List.length (RS.pending_work_items rs2));
          Alcotest.(check (option string))
            "progress_uri after rebuild"
            (Some "sb://./B")
            (RS.progress_uri rs2)))

let test_serialize_unknown_activity () =
  let mb = Resolver.Map_based.empty () in
  let resolver = Resolver.Map_based.to_resolver mb in
  let srs : Ser.routing_slip =
    {
      Ser.completed_work_logs = [];
      next_work_items =
        [ { Ser.activity_type_name = "Unknown"; arguments = [] } ];
    }
  in
  match Ser.from_serializable srs resolver with
  | Ok _ -> Alcotest.fail "expected error"
  | Error _ -> ()

let test_activity_host_forward () =
  let dispatched : (string * RS.t) list ref = ref [] in
  let send uri rs = dispatched := !dispatched @ [ (uri, rs) ] in
  let factory = factory_for ~name:"A" () in
  let host = S.Activity_host.create ~factory ~send in
  let item = WI.create ~factory ~arguments:Args.empty in
  let rs = RS.create ~work_items:[ item ] () in
  S.Activity_host.process_forward_message host rs;
  (* No more work, so no dispatch *)
  Alcotest.(check int) "no dispatch on completion" 0 (List.length !dispatched);
  Alcotest.(check bool) "rs completed" true (RS.is_completed rs)

let test_fallback_first_succeeds () =
  let factory_ok = factory_for ~name:"OK" () in
  let factory_fail = factory_for ~do_work_returns_log:false ~name:"FAIL" () in
  let alt1 =
    RS.create
      ~work_items:
        [ WI.create ~factory:factory_ok ~arguments:Args.empty ]
      ()
  in
  let alt2 =
    RS.create
      ~work_items:
        [ WI.create ~factory:factory_fail ~arguments:Args.empty ]
      ()
  in
  let item =
    S.Fallback_activity.work_item ~alternatives:[ alt1; alt2 ]
  in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "fallback ok" true (RS.process_next rs);
  Alcotest.(check bool) "alt1 completed" true (RS.is_completed alt1);
  Alcotest.(check bool) "alt2 untouched" false (RS.is_in_progress alt2)

let test_fallback_all_fail () =
  let factory_fail = factory_for ~do_work_returns_log:false ~name:"FAIL" () in
  let alt1 =
    RS.create
      ~work_items:
        [ WI.create ~factory:factory_fail ~arguments:Args.empty ]
      ()
  in
  let item = S.Fallback_activity.work_item ~alternatives:[ alt1 ] in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "fallback fails" false (RS.process_next rs)

let test_parallel_all_succeed () =
  let factory_ok = factory_for ~name:"OK" () in
  let mk () =
    RS.create
      ~work_items:[ WI.create ~factory:factory_ok ~arguments:Args.empty ]
      ()
  in
  let b1 = mk () and b2 = mk () in
  let item = S.Parallel_activity.work_item ~branches:[ b1; b2 ] in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "parallel ok" true (RS.process_next rs);
  Alcotest.(check bool) "b1 completed" true (RS.is_completed b1);
  Alcotest.(check bool) "b2 completed" true (RS.is_completed b2)

let test_parallel_one_fails () =
  let factory_ok = factory_for ~name:"OK" () in
  let factory_fail = factory_for ~do_work_returns_log:false ~name:"FAIL" () in
  let b1 =
    RS.create
      ~work_items:[ WI.create ~factory:factory_ok ~arguments:Args.empty ]
      ()
  in
  let b2 =
    RS.create
      ~work_items:[ WI.create ~factory:factory_fail ~arguments:Args.empty ]
      ()
  in
  let item = S.Parallel_activity.work_item ~branches:[ b1; b2 ] in
  let rs = RS.create ~work_items:[ item ] () in
  Alcotest.(check bool) "parallel fails" false (RS.process_next rs);
  Alcotest.(check bool) "b1 compensated" false (RS.is_in_progress b1)

let () =
  Alcotest.run "Saga"
    [
      ( "routing_slip",
        [
          Alcotest.test_case "create empty" `Quick test_create_empty;
          Alcotest.test_case "process_next success" `Quick
            test_process_next_success;
          Alcotest.test_case "process_next failure" `Quick
            test_process_next_failure;
          Alcotest.test_case "progress/compensation uri" `Quick
            test_progress_uri;
          Alcotest.test_case "undo_last" `Quick test_undo_last;
        ] );
      ( "serialization",
        [
          Alcotest.test_case "roundtrip" `Quick test_serialize_roundtrip;
          Alcotest.test_case "unknown activity rejected" `Quick
            test_serialize_unknown_activity;
        ] );
      ( "activity_host",
        [
          Alcotest.test_case "forward terminating slip" `Quick
            test_activity_host_forward;
        ] );
      ( "fallback",
        [
          Alcotest.test_case "first succeeds" `Quick test_fallback_first_succeeds;
          Alcotest.test_case "all fail" `Quick test_fallback_all_fail;
        ] );
      ( "parallel",
        [
          Alcotest.test_case "all succeed" `Quick test_parallel_all_succeed;
          Alcotest.test_case "one fails" `Quick test_parallel_one_fails;
        ] );
    ]
