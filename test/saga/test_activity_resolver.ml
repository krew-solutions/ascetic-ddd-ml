(** Tests for [Activity_resolver] and its [Map_based] implementation. *)

open Test_helpers

(* register / resolve --------------------------------------------------- *)

let test_register_and_resolve () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"TestActivity" () in
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register mb ~name:"TestActivity" ~factory;
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.resolve resolver "TestActivity" with
  | Error e -> Alcotest.failf "expected Ok, got Error %s" e
  | Ok resolved_factory ->
    let activity = resolved_factory () in
    Alcotest.(check string) "name" "TestActivity" (S.Activity.name activity)

let test_resolve_unregistered_returns_error () =
  let mb = Resolver.Map_based.empty () in
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.resolve resolver "Unknown" with
  | Ok _ -> Alcotest.fail "expected Error for unknown name"
  | Error _ -> ()

let test_multiple_registrations_are_independent () =
  let counters_a = make_counters () in
  let counters_b = make_counters () in
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register
    mb ~name:"A" ~factory:(stub_factory ~counters:counters_a ~name:"A" ());
  Resolver.Map_based.register
    mb ~name:"B" ~factory:(stub_factory ~counters:counters_b ~name:"B" ());
  let resolver = Resolver.Map_based.to_resolver mb in
  let a =
    match Resolver.resolve resolver "A" with
    | Ok f -> f ()
    | Error e -> Alcotest.failf "resolve A: %s" e
  in
  let b =
    match Resolver.resolve resolver "B" with
    | Ok f -> f ()
    | Error e -> Alcotest.failf "resolve B: %s" e
  in
  Alcotest.(check string) "A" "A" (S.Activity.name a);
  Alcotest.(check string) "B" "B" (S.Activity.name b)

let test_register_overwrite () =
  let counters_first = make_counters () in
  let counters_second = make_counters () in
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register
    mb
    ~name:"Test"
    ~factory:(stub_factory ~counters:counters_first ~name:"First" ());
  Resolver.Map_based.register
    mb
    ~name:"Test"
    ~factory:(stub_factory ~counters:counters_second ~name:"Second" ());
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.resolve resolver "Test" with
  | Error e -> Alcotest.failf "resolve: %s" e
  | Ok factory ->
    let activity = factory () in
    Alcotest.(check string)
      "second registration wins" "Second" (S.Activity.name activity)

(* get_name ------------------------------------------------------------- *)

let test_get_name_for_registered () =
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Registered" () in
  let mb = Resolver.Map_based.empty () in
  Resolver.Map_based.register mb ~name:"Registered" ~factory;
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.get_name resolver factory with
  | Error e -> Alcotest.failf "get_name: %s" e
  | Ok name -> Alcotest.(check string) "name" "Registered" name

let test_get_name_falls_back_to_activity_name () =
  (* OCaml equivalent of Python's NamedActivity fallback: when the factory
     is unknown, the resolver instantiates it and uses [Activity.name]. *)
  let counters = make_counters () in
  let factory = stub_factory ~counters ~name:"Unregistered" () in
  let mb = Resolver.Map_based.empty () in
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.get_name resolver factory with
  | Error e -> Alcotest.failf "get_name fallback: %s" e
  | Ok name -> Alcotest.(check string) "fallback name" "Unregistered" name

let test_get_name_empty_activity_name_errors () =
  (* Activities whose [Activity.name] is the empty string have no fallback
     identity and can never be serialized -- the resolver must reject. *)
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
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
  let mb = Resolver.Map_based.empty () in
  let resolver = Resolver.Map_based.to_resolver mb in
  match Resolver.get_name resolver factory with
  | Ok name -> Alcotest.failf "expected Error, got Ok %s" name
  | Error _ -> ()

(* Isolation ----------------------------------------------------------- *)

let test_isolated_resolvers () =
  let counters = make_counters () in
  let mb_a = Resolver.Map_based.empty () in
  let mb_b = Resolver.Map_based.empty () in
  Resolver.Map_based.register
    mb_a ~name:"X" ~factory:(stub_factory ~counters ~name:"X" ());
  let resolver_a = Resolver.Map_based.to_resolver mb_a in
  let resolver_b = Resolver.Map_based.to_resolver mb_b in
  (match Resolver.resolve resolver_a "X" with
   | Ok _ -> ()
   | Error e -> Alcotest.failf "resolver_a should resolve X: %s" e);
  match Resolver.resolve resolver_b "X" with
  | Ok _ -> Alcotest.fail "resolver_b must not resolve X"
  | Error _ -> ()

let () =
  Alcotest.run "Activity_resolver"
    [
      ( "register / resolve",
        [
          Alcotest.test_case "register and resolve" `Quick
            test_register_and_resolve;
          Alcotest.test_case "unregistered returns Error" `Quick
            test_resolve_unregistered_returns_error;
          Alcotest.test_case "multiple registrations" `Quick
            test_multiple_registrations_are_independent;
          Alcotest.test_case "overwrite wins" `Quick test_register_overwrite;
        ] );
      ( "get_name",
        [
          Alcotest.test_case "registered" `Quick test_get_name_for_registered;
          Alcotest.test_case "fallback to activity name" `Quick
            test_get_name_falls_back_to_activity_name;
          Alcotest.test_case "empty name errors" `Quick
            test_get_name_empty_activity_name_errors;
        ] );
      ( "isolation",
        [
          Alcotest.test_case "resolvers independent" `Quick
            test_isolated_resolvers;
        ] );
    ]
