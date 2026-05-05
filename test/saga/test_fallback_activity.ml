(** Tests for [Fallback_activity].

    The OCaml version takes alternatives as a labeled argument to
    [work_item ~alternatives] / [make ~alternatives] (closure capture),
    rather than packing them inside [Work_item_arguments] as Python does.
    The semantic test cases (primary succeeds, primary fails then backup
    succeeds, all fail, multi-step alternative, etc.) translate cleanly. *)

open Test_helpers

let with_alt ~factory =
  RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] ()

(* do_work scenarios --------------------------------------------------- *)

let test_primary_succeeds () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_p = stub_factory ~counters:counters_p ~name:"Primary" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"Backup" () in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[ with_alt ~factory:factory_p; with_alt ~factory:factory_b ];
      ]
      ()
  in
  Alcotest.(check bool) "fallback ok" true (RS.process_next rs);
  Alcotest.(check int) "primary called" 1 counters_p.call_count;
  Alcotest.(check int) "backup not called" 0 counters_b.call_count

let test_primary_fails_backup_succeeds () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b = stub_factory ~counters:counters_b ~name:"Backup" () in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[ with_alt ~factory:factory_p; with_alt ~factory:factory_b ];
      ]
      ()
  in
  Alcotest.(check bool) "fallback ok" true (RS.process_next rs);
  Alcotest.(check int) "primary called" 1 counters_p.call_count;
  Alcotest.(check int) "backup called" 1 counters_b.call_count

let test_multi_step_alternative () =
  let counters_p = make_counters () in
  let counters_c = make_counters () in
  let factory_p = stub_factory ~counters:counters_p ~name:"Primary" () in
  let factory_c = stub_factory ~counters:counters_c ~name:"Confirm" () in
  let alt =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_p ~arguments:Args.empty;
        WI.create ~factory:factory_c ~arguments:Args.empty;
      ]
      ()
  in
  let rs =
    RS.create
      ~work_items:[ S.Fallback_activity.work_item ~alternatives:[ alt ] ]
      ()
  in
  Alcotest.(check bool) "fallback ok" true (RS.process_next rs);
  Alcotest.(check int) "primary called" 1 counters_p.call_count;
  Alcotest.(check int) "confirm called" 1 counters_c.call_count

let test_all_alternatives_fail () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b =
    stub_factory ~should_succeed:false ~counters:counters_b ~name:"Backup" ()
  in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[ with_alt ~factory:factory_p; with_alt ~factory:factory_b ];
      ]
      ()
  in
  Alcotest.(check bool) "fallback fails" false (RS.process_next rs);
  Alcotest.(check int) "primary tried" 1 counters_p.call_count;
  Alcotest.(check int) "backup tried" 1 counters_b.call_count

let test_third_alternative_succeeds () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let counters_t = make_counters () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b =
    stub_factory ~should_succeed:false ~counters:counters_b ~name:"Backup" ()
  in
  let factory_t = stub_factory ~counters:counters_t ~name:"Third" () in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[
            with_alt ~factory:factory_p;
            with_alt ~factory:factory_b;
            with_alt ~factory:factory_t;
          ];
      ]
      ()
  in
  Alcotest.(check bool) "fallback ok" true (RS.process_next rs);
  Alcotest.(check int) "primary tried" 1 counters_p.call_count;
  Alcotest.(check int) "backup tried" 1 counters_b.call_count;
  Alcotest.(check int) "third called" 1 counters_t.call_count

(* compensate scenarios ------------------------------------------------- *)

let test_compensate_primary_only () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_p = stub_factory ~counters:counters_p ~name:"Primary" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"Backup" () in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[ with_alt ~factory:factory_p; with_alt ~factory:factory_b ];
      ]
      ()
  in
  let _ = RS.process_next rs in
  let _ = RS.undo_last rs in
  Alcotest.(check int) "primary compensated" 1 counters_p.compensate_count;
  Alcotest.(check int) "backup not compensated" 0 counters_b.compensate_count

let test_compensate_backup_when_primary_failed () =
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b = stub_factory ~counters:counters_b ~name:"Backup" () in
  let rs =
    RS.create
      ~work_items:[
        S.Fallback_activity.work_item
          ~alternatives:[ with_alt ~factory:factory_p; with_alt ~factory:factory_b ];
      ]
      ()
  in
  let _ = RS.process_next rs in
  let _ = RS.undo_last rs in
  Alcotest.(check int) "primary not compensated" 0 counters_p.compensate_count;
  Alcotest.(check int) "backup compensated" 1 counters_b.compensate_count

let test_compensate_multi_step_alternative () =
  let counters_p = make_counters () in
  let counters_c = make_counters () in
  let factory_p = stub_factory ~counters:counters_p ~name:"Primary" () in
  let factory_c = stub_factory ~counters:counters_c ~name:"Confirm" () in
  let alt =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_p ~arguments:Args.empty;
        WI.create ~factory:factory_c ~arguments:Args.empty;
      ]
      ()
  in
  let rs =
    RS.create
      ~work_items:[ S.Fallback_activity.work_item ~alternatives:[ alt ] ]
      ()
  in
  let _ = RS.process_next rs in
  Alcotest.(check int) "primary called" 1 counters_p.call_count;
  Alcotest.(check int) "confirm called" 1 counters_c.call_count;
  let _ = RS.undo_last rs in
  Alcotest.(check int) "primary compensated" 1 counters_p.compensate_count;
  Alcotest.(check int) "confirm compensated" 1 counters_c.compensate_count

(* queue addresses & integration --------------------------------------- *)

let test_queue_addresses () =
  let activity = S.Fallback_activity.make ~alternatives:[] in
  Alcotest.(check string)
    "work queue"
    "sb://./fallback"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./fallbackCompensation"
    (S.Activity.compensation_queue_address activity)

let test_fallback_step_in_routing_slip () =
  let counters_third = make_counters () in
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_third = stub_factory ~counters:counters_third ~name:"Third" () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b = stub_factory ~counters:counters_b ~name:"Backup" () in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_third ~arguments:Args.empty;
        S.Fallback_activity.work_item
          ~alternatives:[
            with_alt ~factory:factory_p;
            with_alt ~factory:factory_b;
          ];
        WI.create ~factory:factory_third ~arguments:Args.empty;
      ]
      ()
  in
  while not (RS.is_completed rs) do
    Alcotest.(check bool) "step ok" true (RS.process_next rs)
  done;
  Alcotest.(check int) "Third called twice" 2 counters_third.call_count;
  Alcotest.(check int) "Primary tried" 1 counters_p.call_count;
  Alcotest.(check int) "Backup succeeded" 1 counters_b.call_count

let test_all_fallbacks_fail_triggers_outer_compensation () =
  let counters_third = make_counters () in
  let counters_p = make_counters () in
  let counters_b = make_counters () in
  let factory_third = stub_factory ~counters:counters_third ~name:"Third" () in
  let factory_p =
    stub_factory ~should_succeed:false ~counters:counters_p ~name:"Primary" ()
  in
  let factory_b =
    stub_factory ~should_succeed:false ~counters:counters_b ~name:"Backup" ()
  in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_third ~arguments:Args.empty;
        S.Fallback_activity.work_item
          ~alternatives:[
            with_alt ~factory:factory_p;
            with_alt ~factory:factory_b;
          ];
      ]
      ()
  in
  Alcotest.(check bool) "first step ok" true (RS.process_next rs);
  Alcotest.(check bool) "fallback step fails" false (RS.process_next rs);
  while RS.is_in_progress rs do
    let _ = RS.undo_last rs in ()
  done;
  Alcotest.(check int) "Third compensated" 1 counters_third.compensate_count

let () =
  Alcotest.run "Fallback_activity"
    [
      ( "do_work",
        [
          Alcotest.test_case "primary succeeds" `Quick test_primary_succeeds;
          Alcotest.test_case "primary fails, backup succeeds" `Quick
            test_primary_fails_backup_succeeds;
          Alcotest.test_case "multi-step alternative" `Quick
            test_multi_step_alternative;
          Alcotest.test_case "all alternatives fail" `Quick
            test_all_alternatives_fail;
          Alcotest.test_case "third succeeds" `Quick
            test_third_alternative_succeeds;
        ] );
      ( "compensate",
        [
          Alcotest.test_case "compensate primary only" `Quick
            test_compensate_primary_only;
          Alcotest.test_case "compensate backup when primary failed" `Quick
            test_compensate_backup_when_primary_failed;
          Alcotest.test_case "compensate multi-step alternative" `Quick
            test_compensate_multi_step_alternative;
        ] );
      ( "metadata",
        [
          Alcotest.test_case "queue addresses" `Quick test_queue_addresses;
        ] );
      ( "integration",
        [
          Alcotest.test_case "as a step in routing slip" `Quick
            test_fallback_step_in_routing_slip;
          Alcotest.test_case "all fail triggers outer compensation" `Quick
            test_all_fallbacks_fail_triggers_outer_compensation;
        ] );
    ]
