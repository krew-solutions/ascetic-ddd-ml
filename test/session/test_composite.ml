(** Composite sessions over in-memory delegates that share one journal, so the
    interleaving of both delegates' statements is visible; labelled observers show which
    delegate opened or closed when. *)

module Memory = Ascetic_session_memory.Memory_session
module Journal = Memory.Journal
module Memory_pool = Ascetic_session_memory.Memory_session_pool
module Error = Ascetic_session.Session_error
module Observer = Ascetic_session.Session_observer
module Composite = Ascetic_session_composite.Composite_session.Make (Memory) (Memory)

module Pool =
  Ascetic_session_composite.Composite_session_pool.Make (Memory_pool) (Memory_pool)

type app_error = Session of Error.t | Refused of string

let lift e = Session e
let atomic session scope = Composite.atomic session ~lift scope

let app_error =
  Alcotest.testable
    (fun ppf -> function
      | Session e -> Format.fprintf ppf "Session (%a)" Error.pp e
      | Refused reason -> Format.fprintf ppf "Refused %S" reason)
    ( = )

let entries = Alcotest.(list string)

(* One list of events for every delegate, each labelled. *)
let events () =
  let seen = ref [] in
  let show (scope : Observer.scope) =
    Printf.sprintf "%d %s" scope.depth
      (match scope.kind with
      | Observer.Session -> "session"
      | Observer.Transaction -> "transaction"
      | Observer.Savepoint -> "savepoint")
  in
  let observer label =
    {
      Observer.on_scope_started =
        (fun scope -> seen := Printf.sprintf "%s: started %s" label (show scope) :: !seen);
      on_scope_ended =
        (fun scope outcome ->
          let outcome =
            match outcome with
            | Observer.Succeeded -> "succeeded"
            | Observer.Failed -> "failed"
          in
          seen := Printf.sprintf "%s: %s %s" label outcome (show scope) :: !seen);
    }
  in
  (observer, fun () -> List.rev !seen)

(* Two delegates over one journal, labelled "first" and "second". *)
let pair ?(fail_first = fun _ -> None) ?(fail_second = fun _ -> None) () =
  let journal = Journal.create () in
  let observer, seen = events () in
  let first = Memory.create ~observer:(observer "first") ~fail:fail_first journal in
  let second = Memory.create ~observer:(observer "second") ~fail:fail_second journal in
  ((first, second), journal, seen)

let test_scopes_nest_across_delegates () =
  let session, journal, seen = pair () in
  let result =
    atomic session (fun (first, second) ->
        Memory.record first "first: INSERT";
        Memory.record second "second: INSERT";
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.check entries "first opens first and closes last"
    [
      "first: started 1 transaction";
      "second: started 1 transaction";
      "second: succeeded 1 transaction";
      "first: succeeded 1 transaction";
    ]
    (seen ());
  Alcotest.check entries "journal"
    [ "BEGIN"; "BEGIN"; "first: INSERT"; "second: INSERT"; "COMMIT"; "COMMIT" ]
    (Journal.entries journal)

let test_a_failure_rolls_both_delegates_back () =
  let session, journal, _ = pair () in
  let result = atomic session (fun _ -> Error (Refused "no")) in
  Alcotest.(check (result unit app_error)) "error returned" (Error (Refused "no")) result;
  Alcotest.check entries "journal"
    [ "BEGIN"; "BEGIN"; "ROLLBACK"; "ROLLBACK" ]
    (Journal.entries journal)

let test_the_inner_delegate_failing_to_commit_rolls_the_outer_back () =
  let session, journal, _ =
    pair ~fail_second:(function "COMMIT" -> Some "disk full" | _ -> None) ()
  in
  let result = atomic session (fun _ -> Ok ()) in
  Alcotest.(check (result unit app_error))
    "the inner commit failure is the scope's error"
    (Error (Session (Error.Commit "disk full"))) result;
  Alcotest.check entries "the outer delegate rolled back"
    [ "BEGIN"; "BEGIN"; "ROLLBACK" ]
    (Journal.entries journal)

(* The honest limit: the inner delegate has committed when the outer one
   fails to, and nothing brings it back. That is what a saga is for. *)
let test_the_outer_delegate_failing_to_commit_leaves_the_inner_committed () =
  let session, journal, _ =
    pair ~fail_first:(function "COMMIT" -> Some "disk full" | _ -> None) ()
  in
  let result = atomic session (fun _ -> Ok ()) in
  Alcotest.(check (result unit app_error))
    "the outer commit failure is the scope's error"
    (Error (Session (Error.Commit "disk full"))) result;
  Alcotest.check entries "the inner delegate's commit stands"
    [ "BEGIN"; "BEGIN"; "COMMIT" ]
    (Journal.entries journal)

let test_nested_composite_scopes_open_savepoints_in_both () =
  let session, journal, _ = pair () in
  let result = atomic session (fun session -> atomic session (fun _ -> Ok ())) in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.check entries "journal"
    [
      "BEGIN";
      "BEGIN";
      "SAVEPOINT sp1";
      "SAVEPOINT sp1";
      "RELEASE SAVEPOINT sp1";
      "RELEASE SAVEPOINT sp1";
      "COMMIT";
      "COMMIT";
    ]
    (Journal.entries journal)

let test_a_second_scope_on_the_composite_is_refused_by_the_first_delegate () =
  let session, _, seen = pair () in
  let result =
    atomic session (fun _ ->
        Alcotest.(check (result unit app_error))
          "refused" (Error (Session Error.Scope_already_open))
          (atomic session (fun _ -> Ok ()));
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "outer committed" (Ok ()) result;
  Alcotest.check entries "the second delegate was never asked"
    [
      "first: started 1 transaction";
      "second: started 1 transaction";
      "second: succeeded 1 transaction";
      "first: succeeded 1 transaction";
    ]
    (seen ())

module Three =
  Ascetic_session_composite.Composite_session.Make
    (Memory)
    (Ascetic_session_composite.Composite_session.Make (Memory) (Memory))

let test_three_delegates_compose_by_nesting () =
  let journal = Journal.create () in
  let observer, seen = events () in
  let session =
    ( Memory.create ~observer:(observer "first") journal,
      ( Memory.create ~observer:(observer "second") journal,
        Memory.create ~observer:(observer "third") journal ) )
  in
  let result =
    Three.atomic session ~lift (fun (first, (second, third)) ->
        Memory.record first "first";
        Memory.record second "second";
        Memory.record third "third";
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.check entries "open in order, close in reverse"
    [
      "first: started 1 transaction";
      "second: started 1 transaction";
      "third: started 1 transaction";
      "third: succeeded 1 transaction";
      "second: succeeded 1 transaction";
      "first: succeeded 1 transaction";
    ]
    (seen ())

let test_the_pool_acquires_left_to_right_and_releases_right_to_left () =
  let observer, seen = events () in
  let pool =
    ( Memory_pool.create ~observer:(observer "first") (),
      Memory_pool.create ~observer:(observer "second") () )
  in
  let result = Pool.session pool ~lift (fun (_, _) -> Ok ()) in
  Alcotest.(check (result unit app_error)) "ran" (Ok ()) result;
  Alcotest.check entries "events"
    [
      "first: started 0 session";
      "second: started 0 session";
      "second: succeeded 0 session";
      "first: succeeded 0 session";
    ]
    (seen ())

let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "Composite_session"
    [
      ( "composition",
        [
          Alcotest.test_case "scopes nest across delegates" `Quick
            test_scopes_nest_across_delegates;
          Alcotest.test_case "nested composite scopes open savepoints in both" `Quick
            test_nested_composite_scopes_open_savepoints_in_both;
          Alcotest.test_case "three delegates compose by nesting" `Quick
            test_three_delegates_compose_by_nesting;
          Alcotest.test_case
            "a second scope on the composite is refused by the first delegate" `Quick
            test_a_second_scope_on_the_composite_is_refused_by_the_first_delegate;
        ] );
      ( "failure",
        [
          Alcotest.test_case "a failure rolls both delegates back" `Quick
            test_a_failure_rolls_both_delegates_back;
          Alcotest.test_case "the inner delegate failing to commit rolls the outer back"
            `Quick test_the_inner_delegate_failing_to_commit_rolls_the_outer_back;
          Alcotest.test_case
            "the outer delegate failing to commit leaves the inner committed" `Quick
            test_the_outer_delegate_failing_to_commit_leaves_the_inner_committed;
        ] );
      ( "pool",
        [
          Alcotest.test_case "the pool acquires left to right and releases right to left"
            `Quick test_the_pool_acquires_left_to_right_and_releases_right_to_left;
        ] );
    ]
