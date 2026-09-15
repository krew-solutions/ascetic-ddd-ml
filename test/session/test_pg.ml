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
