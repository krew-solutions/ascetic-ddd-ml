(** The scope algorithm, exercised over the in-memory session. The PostgreSQL session is
    the same code over another backend, so what is proved here about nesting, failure
    paths, the guard, cancellation and abandonment holds there too; [test_pg.ml] checks
    the backend itself. *)

module Session = Ascetic_session_memory.Memory_session
module Journal = Ascetic_session_memory.Memory_session.Journal
module Pool = Ascetic_session_memory.Memory_session_pool
module Error = Ascetic_session.Session_error
module Driver_error = Ascetic_session.Driver_error
module Observer = Ascetic_session.Session_observer

(* The application's error type: its own cases plus one for the session
   machinery, which [lift] fills. *)
type app_error = Session of Error.t | Refused of string

let lift e = Session e
let atomic session scope = Session.atomic session ~lift scope

let app_error =
  Alcotest.testable
    (fun ppf -> function
      | Session e -> Format.fprintf ppf "Session (%a)" Error.pp e
      | Refused reason -> Format.fprintf ppf "Refused %S" reason)
    ( = )

let entries = Alcotest.(list string)

(* An observer that renders every event into a list. *)
let recording () =
  let seen = ref [] in
  let show (scope : Observer.scope) =
    Printf.sprintf "%d %s" scope.depth
      (match scope.kind with
      | Observer.Session -> "session"
      | Observer.Transaction -> "transaction"
      | Observer.Savepoint -> "savepoint")
  in
  let observer =
    {
      Observer.on_scope_started =
        (fun scope -> seen := ("started " ^ show scope) :: !seen);
      on_scope_ended =
        (fun scope outcome ->
          let outcome =
            match outcome with
            | Observer.Succeeded -> "succeeded"
            | Observer.Failed -> "failed"
          in
          seen := (outcome ^ " " ^ show scope) :: !seen);
    }
  in
  (observer, fun () -> List.rev !seen)

let test_nested_scope_opens_a_savepoint () =
  let journal = Journal.create () in
  let session = Session.create journal in
  let result =
    atomic session (fun session ->
        Session.record session "INSERT 1";
        atomic session (fun session ->
            Session.record session "INSERT 2";
            Ok ()))
  in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.check entries "journal"
    [
      "BEGIN"; "INSERT 1"; "SAVEPOINT sp1"; "INSERT 2"; "RELEASE SAVEPOINT sp1"; "COMMIT";
    ]
    (Journal.entries journal)

let test_failing_nested_scope_leaves_the_outer_alive () =
  let journal = Journal.create () in
  let session = Session.create journal in
  let result =
    atomic session (fun session ->
        Session.record session "INSERT 1";
        match atomic session (fun _ -> Error (Refused "no")) with
        | Error (Refused _) -> Ok "recovered"
        | other -> other)
  in
  Alcotest.(check (result string app_error)) "outer committed" (Ok "recovered") result;
  Alcotest.check entries "journal"
    [ "BEGIN"; "INSERT 1"; "SAVEPOINT sp1"; "ROLLBACK TO SAVEPOINT sp1"; "COMMIT" ]
    (Journal.entries journal)

let test_failing_outer_scope_rolls_back () =
  let journal = Journal.create () in
  let session = Session.create journal in
  let result =
    atomic session (fun session ->
        Session.record session "INSERT 1";
        Error (Refused "no"))
  in
  Alcotest.(check (result unit app_error)) "error returned" (Error (Refused "no")) result;
  Alcotest.check entries "journal"
    [ "BEGIN"; "INSERT 1"; "ROLLBACK" ]
    (Journal.entries journal)

let test_raising_scope_rolls_back_and_reraises () =
  let journal = Journal.create () in
  let session = Session.create journal in
  Alcotest.check_raises "re-raised after the rollback" (Failure "boom") (fun () ->
      ignore (atomic session (fun _ -> failwith "boom")));
  Alcotest.check entries "journal" [ "BEGIN"; "ROLLBACK" ] (Journal.entries journal)

let test_session_errors_lift_into_the_application_error () =
  let journal = Journal.create () in
  let session =
    Session.create ~fail:(function "COMMIT" -> Some "disk full" | _ -> None) journal
  in
  let result = atomic session (fun _ -> Ok ()) in
  Alcotest.(check (result unit app_error))
    "a failed commit is the scope's error"
    (Error (Session (Error.Commit (Driver_error.defect "disk full"))))
    result

let test_second_scope_on_the_same_session_is_refused () =
  let session = Session.create (Journal.create ()) in
  let result =
    atomic session (fun child ->
        Alcotest.(check (result unit app_error))
          "the session that opened this scope is busy"
          (Error (Session Error.Scope_already_open))
          (atomic session (fun _ -> Ok ()));
        Alcotest.(check (result unit app_error))
          "the session it handed out nests" (Ok ())
          (atomic child (fun _ -> Ok ()));
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "outer committed" (Ok ()) result

let test_sequential_scopes_are_allowed_even_after_a_failure () =
  let journal = Journal.create () in
  let session = Session.create journal in
  Alcotest.(check (result unit app_error))
    "first fails" (Error (Refused "no"))
    (atomic session (fun _ -> Error (Refused "no")));
  Alcotest.(check (result unit app_error))
    "second runs" (Ok ())
    (atomic session (fun _ -> Ok ()));
  Alcotest.check entries "journal"
    [ "BEGIN"; "ROLLBACK"; "BEGIN"; "COMMIT" ]
    (Journal.entries journal)

let test_observer_sees_the_whole_lifecycle () =
  let observer, seen = recording () in
  let session = Session.create ~observer (Journal.create ()) in
  let _ = atomic session (fun session -> atomic session (fun _ -> Ok ())) in
  Alcotest.check entries "events"
    [
      "started 1 transaction";
      "started 2 savepoint";
      "succeeded 2 savepoint";
      "succeeded 1 transaction";
    ]
    (seen ())

let test_observers_compose () =
  let first, seen_first = recording () in
  let second, seen_second = recording () in
  let session =
    Session.create ~observer:(Observer.all [ first; second ]) (Journal.create ())
  in
  let _ = atomic session (fun _ -> Error (Refused "no")) in
  let expected = [ "started 1 transaction"; "failed 1 transaction" ] in
  Alcotest.check entries "first" expected (seen_first ());
  Alcotest.check entries "second" expected (seen_second ())

let test_a_failed_rollback_abandons_the_session () =
  let journal = Journal.create () in
  let session =
    Session.create
      ~fail:(function "ROLLBACK TO SAVEPOINT sp1" -> Some "connection lost" | _ -> None)
      journal
  in
  let result =
    atomic session (fun session ->
        (* The nested failure is swallowed by the scope, but its rollback
           failed: the outer scope must not commit. *)
        let _ = atomic session (fun _ -> Error (Refused "no")) in
        Ok ())
  in
  Alcotest.(check (result unit app_error))
    "the outer scope is refused at commit"
    (Error (Session (Error.Abandoned (Driver_error.defect "connection lost"))))
    result;
  Alcotest.(check bool) "abandoned" true (Session.is_abandoned session);
  Alcotest.(check (result unit app_error))
    "every further scope is refused"
    (Error (Session (Error.Abandoned (Driver_error.defect "connection lost"))))
    (atomic session (fun _ -> Ok ()));
  Alcotest.check entries
    "nothing committed, and the outer scope rolled back on its way out"
    [ "BEGIN"; "SAVEPOINT sp1"; "ROLLBACK" ]
    (Journal.entries journal)

let test_a_statement_that_raises_abandons_the_session () =
  let journal = Journal.create () in
  let session =
    Session.create ~fail:(fun sql -> if sql = "COMMIT" then raise Exit else None) journal
  in
  Alcotest.check_raises "the exception goes on" Exit (fun () ->
      ignore (atomic session (fun _ -> Ok ())));
  Alcotest.(check bool)
    "abandoned: whether it committed is unknown" true
    (Session.is_abandoned session);
  Alcotest.(check (result unit app_error))
    "every further scope is refused"
    (Error (Session (Error.Abandoned (Driver_error.defect "Stdlib.Exit"))))
    (atomic session (fun _ -> Ok ()))

let test_a_cancelled_scope_is_rolled_back () =
  let journal = Journal.create () in
  let session = Session.create journal in
  let outcome =
    Eio.Fiber.first
      (fun () ->
        atomic session (fun session ->
            Session.record session "INSERT 1";
            (* Blocks until the fiber is cancelled by the other branch. *)
            Eio.Fiber.await_cancel ()))
      (fun () -> Ok ())
  in
  Alcotest.(check (result unit app_error)) "the other branch won" (Ok ()) outcome;
  Alcotest.check entries "rolled back despite the cancellation"
    [ "BEGIN"; "INSERT 1"; "ROLLBACK" ]
    (Journal.entries journal);
  Alcotest.(check (result unit app_error))
    "the session is still usable" (Ok ())
    (atomic session (fun _ -> Ok ()))

let test_the_pool_scope_is_observed_and_is_not_a_transaction () =
  let observer, seen = recording () in
  let pool = Pool.create ~observer () in
  let result =
    Pool.session pool ~lift (fun session ->
        Session.record session "SELECT 1";
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "ran" (Ok ()) result;
  Alcotest.check entries "events" [ "started 0 session"; "succeeded 0 session" ] (seen ());
  Alcotest.check entries "no transaction" [ "SELECT 1" ]
    (Journal.entries (Pool.journal pool))

(* A session rolls back under [Eio.Cancel.protect], so even the in-memory
   one runs inside an Eio fiber. *)
let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "Session"
    [
      ( "scopes",
        [
          Alcotest.test_case "nested scope opens a savepoint" `Quick
            test_nested_scope_opens_a_savepoint;
          Alcotest.test_case "failing nested scope leaves the outer alive" `Quick
            test_failing_nested_scope_leaves_the_outer_alive;
          Alcotest.test_case "failing outer scope rolls back" `Quick
            test_failing_outer_scope_rolls_back;
          Alcotest.test_case "raising scope rolls back and re-raises" `Quick
            test_raising_scope_rolls_back_and_reraises;
          Alcotest.test_case "session errors lift into the application error" `Quick
            test_session_errors_lift_into_the_application_error;
        ] );
      ( "guard",
        [
          Alcotest.test_case "second scope on the same session is refused" `Quick
            test_second_scope_on_the_same_session_is_refused;
          Alcotest.test_case "sequential scopes are allowed even after a failure" `Quick
            test_sequential_scopes_are_allowed_even_after_a_failure;
        ] );
      ( "observers",
        [
          Alcotest.test_case "observer sees the whole lifecycle" `Quick
            test_observer_sees_the_whole_lifecycle;
          Alcotest.test_case "observers compose" `Quick test_observers_compose;
        ] );
      ( "resilience",
        [
          Alcotest.test_case "a failed rollback abandons the session" `Quick
            test_a_failed_rollback_abandons_the_session;
          Alcotest.test_case "a statement that raises abandons the session" `Quick
            test_a_statement_that_raises_abandons_the_session;
          Alcotest.test_case "a cancelled scope is rolled back" `Quick
            test_a_cancelled_scope_is_rolled_back;
        ] );
      ( "pool",
        [
          Alcotest.test_case "the pool scope is observed and is not a transaction" `Quick
            test_the_pool_scope_is_observed_and_is_not_a_transaction;
        ] );
    ]
