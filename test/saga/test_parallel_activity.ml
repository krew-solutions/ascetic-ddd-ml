(** Tests for [Parallel_activity].

    [Parallel_activity] forks fibers via [Eio.Fiber.List.map] and
    [Eio.Fiber.List.iter], so every test that drives a routing slip
    containing a parallel work item must be invoked from inside an
    [Eio_main.run] context. The [eio_run] helper wraps each test body. *)

open Test_helpers

(** Run a test body inside an [Eio_main.run]. The Alcotest case becomes
    a thin wrapper that establishes the Eio runtime. *)
let eio_run (body : unit -> unit) () : unit =
  Eio_main.run @@ fun _env -> body ()

let with_branch ~factory =
  RS.create ~work_items:[ WI.create ~factory ~arguments:Args.empty ] ()

(* do_work scenarios --------------------------------------------------- *)

let test_all_branches_succeed () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let rs =
    RS.create
      ~work_items:[
        S.Parallel_activity.work_item
          ~branches:[
            with_branch ~factory:factory_a;
            with_branch ~factory:factory_b;
          ];
      ]
      ()
  in
  Alcotest.(check bool) "parallel ok" true (RS.process_next rs);
  Alcotest.(check int) "BranchA called" 1 counters_a.call_count;
  Alcotest.(check int) "BranchB called" 1 counters_b.call_count

let test_multi_step_branches_succeed () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let branch_a =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_a ~arguments:Args.empty;
      ]
      ()
  in
  let branch_b = with_branch ~factory:factory_b in
  let rs =
    RS.create
      ~work_items:[
        S.Parallel_activity.work_item ~branches:[ branch_a; branch_b ];
      ]
      ()
  in
  Alcotest.(check bool) "parallel ok" true (RS.process_next rs);
  Alcotest.(check int) "BranchA called twice" 2 counters_a.call_count;
  Alcotest.(check int) "BranchB called once" 1 counters_b.call_count

let test_one_branch_fails_compensates_all () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let counters_fail = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let factory_fail =
    stub_factory ~should_succeed:false ~counters:counters_fail ~name:"Fail" ()
  in
  let branch_a =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_fail ~arguments:Args.empty;
      ]
      ()
  in
  let branch_b = with_branch ~factory:factory_b in
  let rs =
    RS.create
      ~work_items:[
        S.Parallel_activity.work_item ~branches:[ branch_a; branch_b ];
      ]
      ()
  in
  Alcotest.(check bool) "parallel fails" false (RS.process_next rs);
  Alcotest.(check int) "BranchA call count" 1 counters_a.call_count;
  Alcotest.(check int) "BranchA compensated" 1 counters_a.compensate_count;
  (* Branch B already finished before the failure was observed; it gets
     compensated once. *)
  Alcotest.(check int) "BranchB compensated" 1 counters_b.compensate_count

(* compensate scenarios ------------------------------------------------- *)

let test_compensate_all_branches () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let branch_a =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_a ~arguments:Args.empty;
      ]
      ()
  in
  let branch_b = with_branch ~factory:factory_b in
  let rs =
    RS.create
      ~work_items:[
        S.Parallel_activity.work_item ~branches:[ branch_a; branch_b ];
      ]
      ()
  in
  Alcotest.(check bool) "parallel ok" true (RS.process_next rs);
  Alcotest.(check int) "BranchA called twice" 2 counters_a.call_count;
  Alcotest.(check int) "BranchB called once" 1 counters_b.call_count;
  let _ = RS.undo_last rs in
  Alcotest.(check int) "BranchA compensated twice" 2 counters_a.compensate_count;
  Alcotest.(check int) "BranchB compensated once" 1 counters_b.compensate_count

(* metadata ------------------------------------------------------------- *)

let test_queue_addresses () =
  let activity = S.Parallel_activity.make ~branches:[] in
  Alcotest.(check string)
    "work queue"
    "sb://./parallel"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./parallelCompensation"
    (S.Activity.compensation_queue_address activity)

(* integration --------------------------------------------------------- *)

let test_parallel_step_in_routing_slip () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let parallel_branch_a =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        WI.create ~factory:factory_a ~arguments:Args.empty;
      ]
      ()
  in
  let parallel_branch_b = with_branch ~factory:factory_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        S.Parallel_activity.work_item
          ~branches:[ parallel_branch_a; parallel_branch_b ];
        WI.create ~factory:factory_b ~arguments:Args.empty;
      ]
      ()
  in
  while not (RS.is_completed rs) do
    Alcotest.(check bool) "step ok" true (RS.process_next rs)
  done;
  (* BranchA: 1 (first) + 2 (parallel) = 3. BranchB: 1 (parallel) + 1 (last) = 2. *)
  Alcotest.(check int) "BranchA total calls" 3 counters_a.call_count;
  Alcotest.(check int) "BranchB total calls" 2 counters_b.call_count

let test_parallel_failure_triggers_outer_compensation () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let counters_fail = make_counters () in
  let factory_a = stub_factory ~counters:counters_a ~name:"BranchA" () in
  let factory_b = stub_factory ~counters:counters_b ~name:"BranchB" () in
  let factory_fail =
    stub_factory ~should_succeed:false ~counters:counters_fail ~name:"Fail" ()
  in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:factory_a ~arguments:Args.empty;
        S.Parallel_activity.work_item
          ~branches:[
            with_branch ~factory:factory_b;
            with_branch ~factory:factory_fail;
          ];
      ]
      ()
  in
  Alcotest.(check bool) "first step ok" true (RS.process_next rs);
  Alcotest.(check int) "BranchA called" 1 counters_a.call_count;
  Alcotest.(check bool) "parallel step fails" false (RS.process_next rs);
  while RS.is_in_progress rs do
    let _ = RS.undo_last rs in ()
  done;
  Alcotest.(check int) "BranchA compensated" 1 counters_a.compensate_count

let () =
  Alcotest.run "Parallel_activity"
    [
      ( "do_work",
        [
          Alcotest.test_case "all branches succeed" `Quick
            (eio_run test_all_branches_succeed);
          Alcotest.test_case "multi-step branches succeed" `Quick
            (eio_run test_multi_step_branches_succeed);
          Alcotest.test_case "one branch fails compensates all" `Quick
            (eio_run test_one_branch_fails_compensates_all);
        ] );
      ( "compensate",
        [
          Alcotest.test_case "compensate all branches" `Quick
            (eio_run test_compensate_all_branches);
        ] );
      ( "metadata",
        [
          Alcotest.test_case "queue addresses" `Quick test_queue_addresses;
        ] );
      ( "integration",
        [
          Alcotest.test_case "as a step in routing slip" `Quick
            (eio_run test_parallel_step_in_routing_slip);
          Alcotest.test_case "failure triggers outer compensation" `Quick
            (eio_run test_parallel_failure_triggers_outer_compensation);
        ] );
    ]
