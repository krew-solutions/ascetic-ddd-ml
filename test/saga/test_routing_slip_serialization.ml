(** Tests for [Serializable] (RoutingSlip ↔ wire format conversion). *)

open Test_helpers

let make_resolver ~counters_a ~counters_b =
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register
    mb ~name:"A" ~factory:(stub_factory ~counters:counters_a ~name:"A" ());
  Resolver.Map_based.register
    mb ~name:"B" ~factory:(stub_factory ~counters:counters_b ~name:"B" ());
  Resolver.Map_based.to_resolver mb

(* to_serializable ----------------------------------------------------- *)

let test_to_serializable_empty_slip () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs = RS.create () in
  match Ser.to_serializable rs resolver with
  | Error e -> Alcotest.failf "to_serializable: %s" e
  | Ok srs ->
    Alcotest.(check int) "no completed logs" 0
      (List.length srs.Ser.completed_work_logs);
    Alcotest.(check int) "no pending items" 0
      (List.length srs.Ser.next_work_items)

let test_to_serializable_pending_items () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:(Args.of_list [ "k", `String "v" ]);
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
      ]
      ()
  in
  match Ser.to_serializable rs resolver with
  | Error e -> Alcotest.failf "to_serializable: %s" e
  | Ok srs ->
    Alcotest.(check int) "2 pending" 2 (List.length srs.Ser.next_work_items);
    let first = List.hd srs.Ser.next_work_items in
    Alcotest.(check string)
      "first activity name"
      "A"
      first.Ser.activity_type_name;
    Alcotest.(check (option string))
      "first arg k"
      (Some "\"v\"")
      (Option.map Yojson.Safe.to_string (List.assoc_opt "k" first.Ser.arguments))

let test_to_serializable_completed_work () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  match Ser.to_serializable rs resolver with
  | Error e -> Alcotest.failf "to_serializable: %s" e
  | Ok srs ->
    Alcotest.(check int) "1 completed log" 1
      (List.length srs.Ser.completed_work_logs);
    let log = List.hd srs.Ser.completed_work_logs in
    Alcotest.(check string) "name" "A" log.Ser.activity_type_name

let test_to_serializable_unknown_activity () =
  let counters = make_counters () in
  (* No registration -- but stub_factory's activity has name "Unknown",
     so the get_name fallback succeeds. To exercise the failure path we
     use an activity whose name is empty. *)
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () -> match !activity_ref with Some a -> a | None -> assert false
  in
  let do_work _wi =
    Some (WL.create_with_factory ~factory ~result:Res.empty)
  in
  let activity =
    S.Activity.create
      ~name:""
      ~do_work
      ~compensate:(fun _wl _rs -> true)
      ~work_item_queue_address:""
      ~compensation_queue_address:""
  in
  activity_ref := Some activity;
  ignore counters;
  let mb = Resolver.Map_based.empty () in
  let resolver = Resolver.Map_based.to_resolver mb in
  let rs =
    RS.create
      ~work_items:[ WI.create ~factory ~arguments:Args.empty ]
      ()
  in
  match Ser.to_serializable rs resolver with
  | Ok _ -> Alcotest.fail "expected Error for empty-name activity"
  | Error _ -> ()

(* from_serializable --------------------------------------------------- *)

let test_from_serializable_empty () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let srs : Ser.routing_slip =
    { Ser.completed_work_logs = []; next_work_items = [] }
  in
  match Ser.from_serializable srs resolver with
  | Error e -> Alcotest.failf "from_serializable: %s" e
  | Ok rs ->
    Alcotest.(check bool) "completed" true (RS.is_completed rs);
    Alcotest.(check bool) "not in progress" false (RS.is_in_progress rs)

let test_from_serializable_pending_items () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let srs : Ser.routing_slip =
    {
      Ser.completed_work_logs = [];
      next_work_items = [
        { Ser.activity_type_name = "A"; arguments = [ "k", `String "v" ] };
        { Ser.activity_type_name = "B"; arguments = [ "n", `Int 7 ] };
      ];
    }
  in
  match Ser.from_serializable srs resolver with
  | Error e -> Alcotest.failf "from_serializable: %s" e
  | Ok rs ->
    Alcotest.(check int) "2 pending" 2
      (List.length (RS.pending_work_items rs));
    Alcotest.(check (option string))
      "progress_uri = first activity"
      (Some "sb://./A")
      (RS.progress_uri rs)

let test_from_serializable_completed_logs () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let srs : Ser.routing_slip =
    {
      Ser.completed_work_logs = [
        { Ser.activity_type_name = "A"; result = [ "id", `Int 42 ] };
      ];
      next_work_items = [];
    }
  in
  match Ser.from_serializable srs resolver with
  | Error e -> Alcotest.failf "from_serializable: %s" e
  | Ok rs ->
    Alcotest.(check bool) "in progress" true (RS.is_in_progress rs);
    Alcotest.(check int) "1 log" 1 (List.length (RS.completed_work_logs rs))

let test_from_serializable_unregistered () =
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
  | Ok _ -> Alcotest.fail "expected Error"
  | Error _ -> ()

(* round-trip ---------------------------------------------------------- *)

let test_state_is_preserved_after_round_trip () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:(Args.of_list [ "step", `Int 1 ]);
        WI.create ~factory:(stub_factory ~counters:counters_b ~name:"B" ())
          ~arguments:(Args.of_list [ "step", `Int 2 ]);
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:(Args.of_list [ "step", `Int 3 ]);
      ]
      ()
  in
  let _ = RS.process_next rs in
  let srs =
    match Ser.to_serializable rs resolver with
    | Ok srs -> srs
    | Error e -> Alcotest.failf "to_serializable: %s" e
  in
  let restored =
    match Ser.from_serializable srs resolver with
    | Ok rs -> rs
    | Error e -> Alcotest.failf "from_serializable: %s" e
  in
  Alcotest.(check int) "1 completed log" 1
    (List.length (RS.completed_work_logs restored));
  Alcotest.(check int) "2 pending items" 2
    (List.length (RS.pending_work_items restored));
  let _ = RS.process_next restored in
  let _ = RS.process_next restored in
  Alcotest.(check bool) "completed after resume" true (RS.is_completed restored);
  Alcotest.(check int) "3 completed logs" 3
    (List.length (RS.completed_work_logs restored))

let test_round_trip_through_json_string () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:(Args.of_list [ "key", `String "value" ]);
      ]
      ()
  in
  let _ = RS.process_next rs in
  let wire =
    match Ser.to_serializable rs resolver with
    | Ok srs -> Ser.to_string srs
    | Error e -> Alcotest.failf "to_serializable: %s" e
  in
  let restored =
    match Ser.of_string wire with
    | Ok srs ->
      (match Ser.from_serializable srs resolver with
       | Ok rs -> rs
       | Error e -> Alcotest.failf "from_serializable: %s" e)
    | Error e -> Alcotest.failf "of_string: %s" e
  in
  Alcotest.(check int) "1 completed log" 1
    (List.length (RS.completed_work_logs restored))

(* compensation after round-trip --------------------------------------- *)

let test_undo_last_works_after_round_trip () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  let _ = RS.process_next rs in
  let wire =
    match Ser.to_serializable rs resolver with
    | Ok srs -> Ser.to_string srs
    | Error e -> Alcotest.failf "to_serializable: %s" e
  in
  let restored =
    match Ser.of_string wire with
    | Ok srs ->
      (match Ser.from_serializable srs resolver with
       | Ok rs -> rs
       | Error e -> Alcotest.failf "from_serializable: %s" e)
    | Error e -> Alcotest.failf "of_string: %s" e
  in
  while RS.is_in_progress restored do
    let _ = RS.undo_last restored in ()
  done;
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress restored);
  (* Counters from the restored side: the resolver factory created its
     own activity instances, so compensation runs against a fresh
     activity bound to [counters_a]. *)
  Alcotest.(check int)
    "compensate_count incremented twice"
    2
    counters_a.compensate_count;
  ignore counters_b

let test_multi_stage_round_trip () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let resolver = make_resolver ~counters_a ~counters_b in
  let rs =
    RS.create
      ~work_items:[
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
        WI.create ~factory:(stub_factory ~counters:counters_a ~name:"A" ())
          ~arguments:Args.empty;
      ]
      ()
  in
  let _ = RS.process_next rs in
  let stage2 =
    let wire =
      match Ser.to_serializable rs resolver with
      | Ok srs -> Ser.to_string srs
      | Error e -> Alcotest.failf "to_serializable: %s" e
    in
    match Ser.of_string wire with
    | Ok srs ->
      (match Ser.from_serializable srs resolver with
       | Ok rs -> rs
       | Error e -> Alcotest.failf "from_serializable: %s" e)
    | Error e -> Alcotest.failf "of_string: %s" e
  in
  let _ = RS.process_next stage2 in
  let stage3 =
    let wire =
      match Ser.to_serializable stage2 resolver with
      | Ok srs -> Ser.to_string srs
      | Error e -> Alcotest.failf "to_serializable: %s" e
    in
    match Ser.of_string wire with
    | Ok srs ->
      (match Ser.from_serializable srs resolver with
       | Ok rs -> rs
       | Error e -> Alcotest.failf "from_serializable: %s" e)
    | Error e -> Alcotest.failf "of_string: %s" e
  in
  while RS.is_in_progress stage3 do
    let _ = RS.undo_last stage3 in ()
  done;
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress stage3);
  ignore counters_b

(* JSON wire format ---------------------------------------------------- *)

let test_json_wire_format_uses_camel_case () =
  let srs : Ser.routing_slip =
    {
      Ser.completed_work_logs = [
        { Ser.activity_type_name = "ActivityA"; result = [ "id", `Int 1 ] };
      ];
      next_work_items = [
        { Ser.activity_type_name = "ActivityB";
          arguments = [ "k", `String "v" ] };
      ];
    }
  in
  let json = Ser.to_json srs in
  let s = Yojson.Safe.to_string json in
  let contains substr =
    let n = String.length s in
    let m = String.length substr in
    let found = ref false in
    let i = ref 0 in
    while (not !found) && (!i + m <= n) do
      if String.equal (String.sub s !i m) substr then found := true;
      incr i
    done;
    !found
  in
  Alcotest.(check bool) "completedWorkLogs key" true
    (contains "\"completedWorkLogs\"");
  Alcotest.(check bool) "nextWorkItems key" true
    (contains "\"nextWorkItems\"");
  Alcotest.(check bool) "activityTypeName key" true
    (contains "\"activityTypeName\"")

let () =
  Alcotest.run "Routing_slip_serialization"
    [
      ( "to_serializable",
        [
          Alcotest.test_case "empty slip" `Quick test_to_serializable_empty_slip;
          Alcotest.test_case "pending items" `Quick test_to_serializable_pending_items;
          Alcotest.test_case "completed work" `Quick test_to_serializable_completed_work;
          Alcotest.test_case "unknown activity errors" `Quick
            test_to_serializable_unknown_activity;
        ] );
      ( "from_serializable",
        [
          Alcotest.test_case "empty" `Quick test_from_serializable_empty;
          Alcotest.test_case "pending items" `Quick test_from_serializable_pending_items;
          Alcotest.test_case "completed logs" `Quick
            test_from_serializable_completed_logs;
          Alcotest.test_case "unregistered errors" `Quick
            test_from_serializable_unregistered;
        ] );
      ( "round trip",
        [
          Alcotest.test_case "state preserved" `Quick
            test_state_is_preserved_after_round_trip;
          Alcotest.test_case "JSON string" `Quick
            test_round_trip_through_json_string;
        ] );
      ( "compensation after round trip",
        [
          Alcotest.test_case "undo_last after single round-trip" `Quick
            test_undo_last_works_after_round_trip;
          Alcotest.test_case "multi-stage round-trip" `Quick
            test_multi_stage_round_trip;
        ] );
      ( "wire format",
        [
          Alcotest.test_case "camelCase keys" `Quick
            test_json_wire_format_uses_camel_case;
        ] );
    ]
