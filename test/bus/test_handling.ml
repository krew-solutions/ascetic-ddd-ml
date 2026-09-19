(** The calls of a handler in flight, and a subscription cancelled in good order: it waits
    for what is running, and not for itself. *)

open Ascetic_bus

let run test () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw -> test ~sw

(* Gives other fibers their turn. *)
let settle () =
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done

(* A fiber that waits for the calls to end, and whether it has come back. *)
let waiting ~sw wait =
  let back = Eio.Fiber.fork_promise ~sw wait in
  settle ();
  fun () -> Eio.Promise.is_resolved back

let test_nothing_in_flight_is_nothing_to_wait_for ~sw:_ =
  Handling.quiesce (Handling.create ())

let test_a_call_in_flight_is_waited_for ~sw =
  let handling = Handling.create () in
  let may_end, let_end = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Handling.call handling (fun () -> Eio.Promise.await may_end));
  let back = waiting ~sw (fun () -> Handling.quiesce handling) in
  Alcotest.(check bool) "waits while the call runs" false (back ());
  Eio.Promise.resolve let_end ();
  settle ();
  Alcotest.(check bool) "comes back when it has ended" true (back ())

let test_every_call_in_flight_is_waited_for ~sw =
  let handling = Handling.create () in
  let first, end_first = Eio.Promise.create () in
  let second, end_second = Eio.Promise.create () in
  List.iter
    (fun may_end ->
      Eio.Fiber.fork ~sw (fun () ->
          Handling.call handling (fun () -> Eio.Promise.await may_end)))
    [ first; second ];
  let back = waiting ~sw (fun () -> Handling.quiesce handling) in
  Eio.Promise.resolve end_first ();
  settle ();
  Alcotest.(check bool) "one is still running" false (back ());
  Eio.Promise.resolve end_second ();
  settle ();
  Alcotest.(check bool) "both have ended" true (back ())

let test_a_call_does_not_wait_for_itself ~sw =
  let handling = Handling.create () in
  Handling.call handling (fun () ->
      Handling.quiesce handling;
      (* nor does a fiber the call forked: it is part of the call *)
      Eio.Fiber.fork ~sw (fun () -> Handling.quiesce handling));
  (* Of two subscriptions, a call of one does wait for the other's. *)
  let other = Handling.create () in
  let may_end, let_end = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () -> Handling.call other (fun () -> Eio.Promise.await may_end));
  let back =
    waiting ~sw (fun () -> Handling.call handling (fun () -> Handling.quiesce other))
  in
  Alcotest.(check bool) "waits for the other's call" false (back ());
  Eio.Promise.resolve let_end ();
  settle ();
  Alcotest.(check bool) "comes back" true (back ())

let test_a_call_that_raises_or_is_cancelled_is_in_flight_no_longer ~sw =
  let handling = Handling.create () in
  (match Handling.call handling (fun () -> failwith "the handler broke") with
  | () -> Alcotest.fail "the exception was swallowed"
  | exception Failure _ -> ());
  Handling.quiesce handling;
  let started, set_started = Eio.Promise.create () in
  let cut_short =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Fiber.first
          (fun () ->
            Handling.call handling (fun () ->
                Eio.Promise.resolve set_started ();
                Eio.Fiber.await_cancel ()))
          (fun () -> Eio.Promise.await started))
  in
  Eio.Promise.await_exn cut_short;
  Handling.quiesce handling

(* A subscription served by a loop of its own. *)

let test_cancelling_tells_the_loop_to_stop_and_waits_for_it ~sw =
  let finishing, may_finish = Eio.Promise.create () in
  let log = ref [] in
  let subscription =
    Handling.loop ~sw (fun ~stop ->
        Eio.Promise.await stop;
        log := "told to stop" :: !log;
        (* what the loop has in hand takes a while *)
        Eio.Promise.await finishing;
        log := "finished" :: !log)
  in
  let back = waiting ~sw (fun () -> Subscription.cancel subscription) in
  Alcotest.(check (list string)) "the loop was told" [ "told to stop" ] (List.rev !log);
  Alcotest.(check bool) "cancel waits for it" false (back ());
  (* A second cancel detaches nothing, and waits like the first. *)
  let back_again = waiting ~sw (fun () -> Subscription.cancel subscription) in
  Alcotest.(check bool) "so does a second cancel" false (back_again ());
  Eio.Promise.resolve may_finish ();
  settle ();
  Alcotest.(check (list string))
    "the loop finished what it had in hand"
    [ "told to stop"; "finished" ]
    (List.rev !log);
  Alcotest.(check bool) "cancel came back" true (back ());
  Alcotest.(check bool) "and the second one" true (back_again ())

let test_a_loop_cancelling_itself_does_not_wait_for_itself ~sw =
  let own = ref None in
  let ended, set_ended = Eio.Promise.create () in
  let subscription =
    Handling.loop ~sw (fun ~stop ->
        (* the handler, called by the loop, cancels its own subscription *)
        Eio.Fiber.yield ();
        Option.iter Subscription.cancel !own;
        Alcotest.(check bool) "told to stop" true (Eio.Promise.is_resolved stop);
        Eio.Promise.resolve set_ended ())
  in
  own := Some subscription;
  Eio.Promise.await ended;
  (* from outside, it is over *)
  Subscription.cancel subscription

let test_a_loop_cut_short_by_its_switch_leaves_nobody_waiting ~sw =
  let subscription = ref None in
  (match
     Eio.Switch.run (fun loops ->
         subscription :=
           Some (Handling.loop ~sw:loops (fun ~stop:_ -> Eio.Fiber.await_cancel ()));
         Eio.Switch.fail loops Exit)
   with
  | () -> Alcotest.fail "the switch did not fail"
  | exception Exit -> ());
  let back = waiting ~sw (fun () -> Option.iter Subscription.cancel !subscription) in
  Alcotest.(check bool) "the loop is over, and so is the wait" true (back ())

let test_a_loop_on_a_switch_that_is_over_is_refused ~sw:_ =
  let over = ref None in
  Eio.Switch.run (fun sw -> over := Some sw);
  match Handling.loop ~sw:(Option.get !over) (fun ~stop:_ -> ()) with
  | (_ : Subscription.t) -> Alcotest.fail "a loop was started on a switch that is over"
  | exception Invalid_argument _ -> ()

let () =
  let case name test = Alcotest.test_case name `Quick (run test) in
  Alcotest.run "Handling"
    [
      ( "calls in flight",
        [
          case "nothing in flight is nothing to wait for"
            test_nothing_in_flight_is_nothing_to_wait_for;
          case "a call in flight is waited for" test_a_call_in_flight_is_waited_for;
          case "every call in flight is waited for"
            test_every_call_in_flight_is_waited_for;
          case "a call does not wait for itself" test_a_call_does_not_wait_for_itself;
          case "a call that raises or is cancelled is in flight no longer"
            test_a_call_that_raises_or_is_cancelled_is_in_flight_no_longer;
        ] );
      ( "a loop of its own",
        [
          case "cancelling tells the loop to stop and waits for it"
            test_cancelling_tells_the_loop_to_stop_and_waits_for_it;
          case "a loop cancelling itself does not wait for itself"
            test_a_loop_cancelling_itself_does_not_wait_for_itself;
          case "a loop cut short by its switch leaves nobody waiting"
            test_a_loop_cut_short_by_its_switch_leaves_nobody_waiting;
          case "a loop on a switch that is over is refused"
            test_a_loop_on_a_switch_that_is_over_is_refused;
        ] );
    ]
