(** Tests for [Activity].

    Python-only tests asserting that an ABC cannot be instantiated and
    that subclasses must implement abstract methods are intentionally
    skipped: in OCaml [Activity.t] is a record, so all four fields are
    required at construction time and the type system enforces it. *)

open Test_helpers

let test_create_complete_activity () =
  let counters = make_counters () in
  let activity = make_stub_activity ~counters ~name:"Complete" () in
  Alcotest.(check string) "name" "Complete" (S.Activity.name activity);
  Alcotest.(check string)
    "work queue"
    "sb://./Complete"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./CompleteCompensation"
    (S.Activity.compensation_queue_address activity)

let test_do_work_receives_work_item () =
  let received_args : Args.t option ref = ref None in
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work wi =
    received_args := Some (WI.arguments wi);
    Some (WL.create_with_factory ~factory ~result:Res.empty)
  in
  let compensate _wl _rs = true in
  let activity =
    S.Activity.create
      ~name:"Test"
      ~do_work
      ~compensate
      ~work_item_queue_address:"sb://./test"
      ~compensation_queue_address:"sb://./testCompensation"
  in
  activity_ref := Some activity;
  let wi =
    WI.create ~factory ~arguments:(Args.of_list [ "key", `String "value" ])
  in
  let _ = S.Activity.do_work activity wi in
  match !received_args with
  | None -> Alcotest.fail "do_work did not capture arguments"
  | Some args ->
    Alcotest.(check (option string))
      "key"
      (Some "\"value\"")
      (Option.map Yojson.Safe.to_string (Args.find args "key"))

let test_compensate_receives_log_and_slip () =
  let received_log : S.Work_log.t option ref = ref None in
  let received_slip : S.Routing_slip.t option ref = ref None in
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work _wi =
    Some
      (WL.create_with_factory
         ~factory
         ~result:(Res.of_list [ "id", `Int 123 ]))
  in
  let compensate wl rs =
    received_log := Some wl;
    received_slip := Some rs;
    true
  in
  let activity =
    S.Activity.create
      ~name:"Test"
      ~do_work
      ~compensate
      ~work_item_queue_address:"sb://./test"
      ~compensation_queue_address:"sb://./testCompensation"
  in
  activity_ref := Some activity;
  let wi = WI.create ~factory ~arguments:Args.empty in
  match S.Activity.do_work activity wi with
  | None -> Alcotest.fail "do_work returned None"
  | Some log ->
    let slip = RS.create () in
    let _ = S.Activity.compensate activity log slip in
    Alcotest.(check bool)
      "log captured"
      true
      (match !received_log with Some captured -> captured == log | None -> false);
    Alcotest.(check bool)
      "slip captured"
      true
      (match !received_slip with Some captured -> captured == slip | None -> false)

let () =
  Alcotest.run "Activity"
    [
      ( "construction",
        [
          Alcotest.test_case "complete activity built" `Quick
            test_create_complete_activity;
        ] );
      ( "callbacks",
        [
          Alcotest.test_case "do_work receives work item" `Quick
            test_do_work_receives_work_item;
          Alcotest.test_case "compensate receives log and slip" `Quick
            test_compensate_receives_log_and_slip;
        ] );
    ]
