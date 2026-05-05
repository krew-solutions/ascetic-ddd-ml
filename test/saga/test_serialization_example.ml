(** Tests for [Serialization_example].

    The example prints to stdout for documentation purposes; we redirect
    stdout to /dev/null during the tests to keep alcotest output clean. *)

module S = Ascetic_saga
module RS = S.Routing_slip
module Resolver = S.Activity_resolver
module Example = S.Serialization_example

(** Run [body] with stdout redirected to /dev/null. *)
let with_silenced_stdout (body : unit -> 'a) : 'a =
  flush stdout;
  let saved = Unix.dup Unix.stdout in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o600 in
  Unix.dup2 devnull Unix.stdout;
  Unix.close devnull;
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 saved Unix.stdout;
      Unix.close saved)
    body

let test_make_orchestrator_resolver_registers_examples () =
  let resolver = Example.make_orchestrator_resolver () in
  let check name =
    match Resolver.resolve resolver name with
    | Ok _ -> ()
    | Error e -> Alcotest.failf "expected %s registered: %s" name e
  in
  check "ReserveCarActivity";
  check "ReserveHotelActivity";
  check "ReserveFlightActivity"

let test_make_orchestrator_resolver_returns_fresh_instance () =
  let a = Example.make_orchestrator_resolver () in
  let b = Example.make_orchestrator_resolver () in
  Alcotest.(check bool) "distinct instances" true (a != b)

let test_run_travel_booking_completes () =
  let rs = with_silenced_stdout Example.run_travel_booking_with_serialization in
  Alcotest.(check bool) "completed" true (RS.is_completed rs);
  Alcotest.(check bool) "in progress" true (RS.is_in_progress rs);
  Alcotest.(check int) "3 logs" 3 (List.length (RS.completed_work_logs rs))

let test_run_compensation_compensates_all_logs () =
  let rs = with_silenced_stdout Example.run_compensation_with_serialization in
  Alcotest.(check bool) "no longer in progress" false (RS.is_in_progress rs);
  Alcotest.(check int) "no remaining logs" 0
    (List.length (RS.completed_work_logs rs))

let () =
  Alcotest.run "Serialization_example"
    [
      ( "make_orchestrator_resolver",
        [
          Alcotest.test_case "registers all example activities" `Quick
            test_make_orchestrator_resolver_registers_examples;
          Alcotest.test_case "returns fresh instance per call" `Quick
            test_make_orchestrator_resolver_returns_fresh_instance;
        ] );
      ( "run_travel_booking_with_serialization",
        [
          Alcotest.test_case "completes after handoff" `Quick
            test_run_travel_booking_completes;
        ] );
      ( "run_compensation_with_serialization",
        [
          Alcotest.test_case "compensates completed work" `Quick
            test_run_compensation_compensates_all_logs;
        ] );
    ]
