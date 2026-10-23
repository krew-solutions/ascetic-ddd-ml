(** The PostgreSQL backend of the session, against a real database: what [BEGIN],
    savepoints, [COMMIT] and [ROLLBACK] actually do to rows. Skipped when
    [TEST_DATABASE_URL] is not set. *)

module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Error = Ascetic_session.Session_error

type app_error = Session of Error.t | Refused of string

let lift e = Session e
let atomic session scope = Session.atomic session ~lift scope
let ( let* ) = Result.bind

let app_error =
  Alcotest.testable
    (fun ppf -> function
      | Session e -> Format.fprintf ppf "Session (%a)" Error.pp e
      | Refused reason -> Format.fprintf ppf "Refused %S" reason)
    ( = )

let table = "session_test"

let exec conn sql =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let insert session id =
  exec (Session.connection session)
    (Printf.sprintf "INSERT INTO %s (id) VALUES (%d)" table id)

let ids conn =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  let request =
    (Caqti_type.unit ->* Caqti_type.int)
      (Printf.sprintf "SELECT id FROM %s ORDER BY id" table)
  in
  match C.collect_list request () with
  | Ok ids -> ids
  | Error err -> Alcotest.failf "select: %a" Caqti_error.pp err

let connect ~sw ~stdenv uri =
  match Caqti_eio_unix.connect ~sw ~stdenv uri with
  | Ok conn -> conn
  | Error err -> Alcotest.failf "connect failed: %a" Caqti_error.pp err

(* A fresh table for the test, dropped afterwards. *)
let with_table env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let conn = connect ~sw ~stdenv uri in
  exec conn (Printf.sprintf "DROP TABLE IF EXISTS %s" table);
  exec conn (Printf.sprintf "CREATE TABLE %s (id integer PRIMARY KEY)" table);
  let cleanup () =
    try exec conn (Printf.sprintf "DROP TABLE IF EXISTS %s" table) with _ -> ()
  in
  match body ~sw ~stdenv conn with
  | () -> cleanup ()
  | exception exn ->
      cleanup ();
      raise exn

let test_nested_scope_commits_through_a_savepoint env uri () =
  with_table env uri @@ fun ~sw:_ ~stdenv:_ conn ->
  let session = Session.create conn in
  let result =
    atomic session (fun session ->
        insert session 1;
        atomic session (fun session ->
            insert session 2;
            Ok ()))
  in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.(check (list int)) "both rows durable" [ 1; 2 ] (ids conn)

let test_failing_nested_scope_leaves_the_outer_alive env uri () =
  with_table env uri @@ fun ~sw:_ ~stdenv:_ conn ->
  let session = Session.create conn in
  let result =
    atomic session (fun session ->
        insert session 1;
        match
          atomic session (fun session ->
              insert session 2;
              Error (Refused "no"))
        with
        | Error (Refused _) -> Ok ()
        | other -> other)
  in
  Alcotest.(check (result unit app_error)) "outer committed" (Ok ()) result;
  Alcotest.(check (list int))
    "the nested insert is gone, the outer one stays" [ 1 ] (ids conn)

let test_failing_outer_scope_rolls_everything_back env uri () =
  with_table env uri @@ fun ~sw:_ ~stdenv:_ conn ->
  let session = Session.create conn in
  let result =
    atomic session (fun session ->
        insert session 1;
        let* () =
          atomic session (fun session ->
              insert session 2;
              Ok ())
        in
        Error (Refused "no"))
  in
  Alcotest.(check (result unit app_error)) "error returned" (Error (Refused "no")) result;
  Alcotest.(check (list int)) "nothing durable" [] (ids conn)

let test_concurrent_scopes_on_one_session_are_refused env uri () =
  with_table env uri @@ fun ~sw:_ ~stdenv:_ conn ->
  let session = Session.create conn in
  let result =
    atomic session (fun child ->
        Alcotest.(check (result unit app_error))
          "refused" (Error (Session Error.Scope_already_open))
          (atomic session (fun _ -> Ok ()));
        insert child 1;
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "outer committed" (Ok ()) result;
  Alcotest.(check (list int)) "the outer scope's work is durable" [ 1 ] (ids conn)

let test_a_cancelled_scope_is_rolled_back_and_the_connection_stays_usable env uri () =
  with_table env uri @@ fun ~sw ~stdenv conn ->
  (* Rows are read through a second connection: the session's own must not
     be touched from outside while a scope is open on it. *)
  let other = connect ~sw ~stdenv uri in
  let session = Session.create conn in
  let inside, reached = Eio.Promise.create () in
  let outcome =
    Eio.Fiber.first
      (fun () ->
        atomic session (fun session ->
            insert session 1;
            (* The other branch waits for this point, so the cancellation
               lands here, between two statements, and not inside BEGIN. *)
            Eio.Promise.resolve reached ();
            Eio.Fiber.await_cancel ()))
      (fun () ->
        Eio.Promise.await inside;
        Ok ())
  in
  Alcotest.(check (result unit app_error)) "the other branch won" (Ok ()) outcome;
  Alcotest.(check (list int)) "rolled back despite the cancellation" [] (ids other);
  Alcotest.(check bool) "not abandoned" false (Session.is_abandoned session);
  Alcotest.(check (result unit app_error))
    "the same connection serves the next scope" (Ok ())
    (atomic session (fun session ->
         insert session 2;
         Ok ()));
  Alcotest.(check (list int)) "and commits it" [ 2 ] (ids other)

let test_the_pool_hands_out_working_sessions env uri () =
  with_table env uri @@ fun ~sw ~stdenv conn ->
  let pool =
    match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let result =
    Pool.session pool ~lift (fun session ->
        atomic session (fun session ->
            insert session 5;
            Ok ()))
  in
  Alcotest.(check (result unit app_error)) "committed" (Ok ()) result;
  Alcotest.(check (list int)) "visible from another connection" [ 5 ] (ids conn)

(* A loop is a daemon fiber that holds a pooled connection most of the time. When its
   switch ends it is cancelled wherever it is, here in the middle of a statement; the
   pool, on a switch outside, takes the connection back and goes on serving. With the
   loop on the pool's own switch the connection would never come back and the switch
   would never end, so the pool must outlive the loops. *)
let test_a_pool_outlives_loops_cancelled_in_mid_statement env uri () =
  with_table env uri @@ fun ~sw ~stdenv conn ->
  let pool =
    match Caqti_eio_unix.connect_pool ~sw ~stdenv uri with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let wait session =
    let module C = (val Session.connection session) in
    let open Caqti_request.Infix in
    ignore
      (C.find
         ((Caqti_type.unit ->! Caqti_type.string)
            ~oneshot:true "SELECT pg_sleep(1)::text")
         ())
  in
  let in_the_statement, reached = Eio.Promise.create () in
  (* The loops' switch ends once the statement the loop was in has ended on
     the server: the rollback is protected from the cancellation and goes
     after it. *)
  Eio.Switch.run (fun loops ->
      Eio.Fiber.fork_daemon ~sw:loops (fun () ->
          let (_ : (unit, app_error) result) =
            Pool.session pool ~lift (fun session ->
                atomic session (fun session ->
                    insert session 9;
                    Eio.Promise.resolve reached ();
                    wait session;
                    Ok ()))
          in
          `Stop_daemon);
      Eio.Promise.await in_the_statement);
  Alcotest.(check (list int)) "what the loop was doing is rolled back" [] (ids conn);
  let result =
    Pool.session pool ~lift (fun session ->
        atomic session (fun session ->
            insert session 10;
            Ok ()))
  in
  Alcotest.(check (result unit app_error)) "the pool still serves" (Ok ()) result;
  Alcotest.(check (list int)) "committed" [ 10 ] (ids conn)

(* ------------------- an absorbed failure (ADR-0034) -------------------- *)

(* A table whose second unique index refuses a holder twice - the shape that
   [ON CONFLICT (id) DO NOTHING] does not cover - and a child whose foreign key is
   checked at commit. A fresh pair for each test, dropped afterwards. *)
let with_held_table env uri name body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let conn = connect ~sw ~stdenv uri in
  let drop () =
    try exec conn (Printf.sprintf "DROP TABLE IF EXISTS %s_child, %s" name name)
    with _ -> ()
  in
  drop ();
  exec conn
    (Printf.sprintf "CREATE TABLE %s (id bigint PRIMARY KEY, holder bigint NOT NULL)" name);
  exec conn (Printf.sprintf "CREATE UNIQUE INDEX %s_once ON %s (holder)" name name);
  exec conn
    (Printf.sprintf
       "CREATE TABLE %s_child (id bigint PRIMARY KEY, parent bigint REFERENCES %s \
        DEFERRABLE INITIALLY DEFERRED)"
       name name);
  match body ~sw ~stdenv conn name with
  | () -> drop ()
  | exception exn ->
      drop ();
      raise exn

(* A statement through the connection the session hands out, as a repository would run
   it: a refusal is the server's, carried as the application's error. *)
let run session sql =
  let module C = (val Session.connection session : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  Result.map_error
    (fun error -> Refused (Caqti_error.show error))
    (C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) ())

(* Holds [holder] under [id], as a store would. *)
let hold session table id holder =
  run session
    (Printf.sprintf
       "INSERT INTO %s (id, holder) VALUES (%d, %d) ON CONFLICT (id) DO NOTHING" table id
       holder)

let held conn table =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  let request =
    (Caqti_type.unit ->* Caqti_type.int)
      (Printf.sprintf "SELECT id FROM %s ORDER BY id" table)
  in
  match C.collect_list request () with
  | Ok ids -> ids
  | Error err -> Alcotest.failf "select: %a" Caqti_error.pp err

let contains text part =
  let n = String.length part in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = part || at (i + 1))
  in
  at 0

(* An observer that renders the scope events into a list. *)
let recording () =
  let seen = ref [] in
  let show (scope : Ascetic_session.Session_observer.scope) outcome =
    Printf.sprintf "%s %d" outcome scope.depth
  in
  let observer =
    {
      Ascetic_session.Session_observer.on_scope_started =
        (fun scope -> seen := show scope "started" :: !seen);
      on_scope_ended =
        (fun scope outcome ->
          let outcome =
            match outcome with
            | Ascetic_session.Session_observer.Succeeded -> "succeeded"
            | Failed -> "failed"
          in
          seen := show scope outcome :: !seen);
    }
  in
  (observer, fun () -> List.rev !seen)

(* A scope that catches a refused statement and carries on reported success while the
   server, which aborts a transaction on its first failed statement, rolled the whole
   transaction back at its [COMMIT]. It is refused instead, naming the failure it
   absorbed, and the observer hears it end as failed. *)
let test_a_scope_that_absorbed_a_refused_statement_is_aborted_and_keeps_nothing env uri ()
    =
  with_held_table env uri "session_absorbed" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let observer, seen = recording () in
  let session = Session.create ~observer conn in
  let outcome =
    atomic session (fun session ->
        let* () = hold session table 1 7 in
        (* "Held already": the refusal absorbed, the scope going on. *)
        let _ = hold session table 2 7 in
        Ok ())
  in
  (match outcome with
  | Error (Session (Error.Aborted (Some cause))) ->
      Alcotest.(check (option string)) "a unique violation" (Some "23505") cause.sqlstate;
      Alcotest.(check bool)
        ("the constraint named: " ^ cause.message)
        true
        (contains cause.message (table ^ "_once"))
  | other ->
      Alcotest.failf "the absorbed refusal is not reported: %a"
        (Alcotest.pp Alcotest.(result unit app_error))
        other);
  Alcotest.(check (list int)) "nothing is durable" [] (held conn table);
  Alcotest.(check (list string))
    "the scope ended as failed" [ "started 1"; "failed 1" ] (seen ());
  Alcotest.(check bool) "the connection is whole" false (Session.is_abandoned session);
  Alcotest.(check (result unit app_error))
    "and serves the next scope" (Ok ())
    (atomic session (fun session -> hold session table 3 3));
  Alcotest.(check (list int)) "which commits" [ 3 ] (held conn table)

(* A nested scope that absorbed a refused statement is rolled back to its savepoint and
   refused; the enclosing transaction stays alive, so its caller may absorb that refusal
   and commit the rest. *)
let test_a_nested_scope_that_absorbed_a_refusal_is_aborted_and_the_outer_goes_on
    env
    uri
    () =
  with_held_table env uri "session_absorbed_nested" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let session = Session.create conn in
  let result =
    atomic session (fun session ->
        let* () = hold session table 1 1 in
        let inner =
          atomic session (fun session ->
              let _ = hold session table 2 1 in
              Ok ())
        in
        (match inner with
        | Error (Session (Error.Aborted (Some _))) -> ()
        | _ -> Alcotest.fail "the nested scope is not refused");
        hold session table 3 3)
  in
  Alcotest.(check (result unit app_error)) "the outer transaction commits" (Ok ()) result;
  Alcotest.(check (list int)) "durable" [ 1; 3 ] (held conn table)

(* A rollback that fails leaves the connection in a state nobody knows: here a scope
   released its own savepoint, so the rollback to it finds none. The session is
   abandoned, the enclosing scope refused and rolled back, rather than committed
   mid-way. *)
let test_a_failed_rollback_abandons_the_session env uri () =
  with_held_table env uri "session_failed_rollback" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let session = Session.create conn in
  let outer =
    atomic session (fun session ->
        let* () = hold session table 1 1 in
        let inner =
          atomic session (fun session ->
              let* () = run session "RELEASE SAVEPOINT sp1" in
              Error (Refused "refused"))
        in
        Alcotest.(check bool) "the nested scope failed" true (Result.is_error inner);
        Ok ())
  in
  (match outer with
  | Error (Session (Error.Abandoned _)) -> ()
  | _ -> Alcotest.fail "the enclosing scope is not refused");
  Alcotest.(check bool) "abandoned" true (Session.is_abandoned session);
  Alcotest.(check (list int)) "nothing is durable" [] (held conn table)

(* The verdict is the server's, not the connection's memory: a failure the scope
   recovered from itself, to a savepoint of its own, leaves the transaction whole, and it
   commits - the server asked, since a statement failed. *)
let test_a_failure_the_scope_recovered_from_itself_does_not_abort_it env uri () =
  with_held_table env uri "session_recovered" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let session = Session.create conn in
  let result =
    atomic session (fun session ->
        let* () = hold session table 1 7 in
        let* () = run session "SAVEPOINT manual" in
        let _ = hold session table 2 7 in
        let* () = run session "ROLLBACK TO SAVEPOINT manual" in
        hold session table 3 3)
  in
  Alcotest.(check (result unit app_error)) "the transaction commits" (Ok ()) result;
  Alcotest.(check (list int)) "durable" [ 1; 3 ] (held conn table)

(* A [COMMIT] the server refuses for a reason of its own - here a foreign key checked at
   commit - stays a failed commit, not an absorbed failure, and a defect; the server has
   ended the transaction, so the connection goes on serving. *)
let test_a_commit_the_server_refuses_is_a_failed_commit_and_the_connection_stays
    env
    uri
    () =
  with_held_table env uri "session_refused_commit" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let session = Session.create conn in
  let outcome =
    atomic session (fun session ->
        run session
          (Printf.sprintf "INSERT INTO %s_child (id, parent) VALUES (1, 42)" table))
  in
  (match outcome with
  | Error (Session (Error.Commit reason)) ->
      Alcotest.(check bool)
        ("a foreign key is a defect: " ^ reason.text)
        false reason.transient
  | _ -> Alcotest.fail "not a failed commit");
  Alcotest.(check bool) "the connection is healthy" false (Session.is_abandoned session);
  Alcotest.(check (result unit app_error))
    "the connection takes the next transaction" (Ok ())
    (atomic session (fun session -> hold session table 1 1));
  Alcotest.(check (list int)) "durable" [ 1 ] (held conn table)

(* A statement that failed outside any transaction aborted nothing: the next scope on
   the session begins clean and commits. *)
let test_a_failure_before_a_scope_does_not_abort_it env uri () =
  with_held_table env uri "session_before" @@ fun ~sw:_ ~stdenv:_ conn table ->
  let session = Session.create conn in
  let result =
    let* () = hold session table 1 7 in
    let _ = hold session table 2 7 in
    atomic session (fun session -> hold session table 3 3)
  in
  Alcotest.(check (result unit app_error)) "the scope commits" (Ok ()) result;
  Alcotest.(check (list int)) "durable" [ 1; 3 ] (held conn table)

let cases env uri =
  [
    Alcotest.test_case "nested scope commits through a savepoint" `Quick
      (test_nested_scope_commits_through_a_savepoint env uri);
    Alcotest.test_case "failing nested scope leaves the outer alive" `Quick
      (test_failing_nested_scope_leaves_the_outer_alive env uri);
    Alcotest.test_case "failing outer scope rolls everything back" `Quick
      (test_failing_outer_scope_rolls_everything_back env uri);
    Alcotest.test_case "concurrent scopes on one session are refused" `Quick
      (test_concurrent_scopes_on_one_session_are_refused env uri);
    Alcotest.test_case "a cancelled scope is rolled back and the connection stays usable"
      `Quick
      (test_a_cancelled_scope_is_rolled_back_and_the_connection_stays_usable env uri);
    Alcotest.test_case "the pool hands out working sessions" `Quick
      (test_the_pool_hands_out_working_sessions env uri);
    Alcotest.test_case "a pool outlives loops cancelled in mid-statement" `Quick
      (test_a_pool_outlives_loops_cancelled_in_mid_statement env uri);
    Alcotest.test_case
      "a scope that absorbed a refused statement is aborted and keeps nothing" `Quick
      (test_a_scope_that_absorbed_a_refused_statement_is_aborted_and_keeps_nothing env uri);
    Alcotest.test_case
      "a nested scope that absorbed a refusal is aborted and the outer one goes on" `Quick
      (test_a_nested_scope_that_absorbed_a_refusal_is_aborted_and_the_outer_goes_on env
         uri);
    Alcotest.test_case "a failed rollback abandons the session" `Quick
      (test_a_failed_rollback_abandons_the_session env uri);
    Alcotest.test_case "a failure the scope recovered from itself does not abort it"
      `Quick
      (test_a_failure_the_scope_recovered_from_itself_does_not_abort_it env uri);
    Alcotest.test_case
      "a commit the server refuses is a failed commit and the connection stays" `Quick
      (test_a_commit_the_server_refuses_is_a_failed_commit_and_the_connection_stays env
         uri);
    Alcotest.test_case "a failure before a scope does not abort it" `Quick
      (test_a_failure_before_a_scope_does_not_abort_it env uri);
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] session integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Caqti_session" [ ("integration", cases env uri) ]
