(** The two helpers every PostgreSQL adapter over the session uses: which names may go
    into SQL, and which errors are of the moment. No database needed. *)

module Identifier = Ascetic_session_caqti.Identifier
module Transient = Ascetic_session_caqti.Transient

let test_names_that_may_go_into_sql () =
  List.iter
    (fun name ->
      Alcotest.(check (result string string))
        name (Ok name)
        (Result.map Identifier.to_string (Identifier.of_string name)))
    [ "inbox"; "inbox_2"; "_x"; "outbox_orders_offsets" ]

let test_names_that_may_not () =
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (Printf.sprintf "%S is refused" name)
        true
        (Result.is_error (Identifier.of_string name)))
    [
      "";
      "Inbox";
      "2inbox";
      "public.inbox";
      "inbox; drop table inbox; --";
      "inbox-orders";
      "a_very_long_table_name_of_forty_one_charac";
    ]

let test_the_moment_is_told_from_the_defect_by_sqlstate () =
  List.iter
    (fun (code, moment) -> Alcotest.(check bool) code moment (Transient.sqlstate code))
    [
      ("40P01", true) (* deadlock detected *);
      ("40001", true) (* serialization failure *);
      ("08006", true) (* connection failure *);
      ("53300", true) (* too many connections *);
      ("57P01", true) (* admin shutdown *);
      ("42P01", false) (* undefined table *);
      ("23505", false) (* unique violation *);
      ("22P02", false) (* invalid text representation *);
      ("0A000", false) (* feature not supported *);
    ]

let test_a_connection_that_cannot_be_made_is_of_the_moment () =
  let uri = Uri.of_string "postgresql://nobody@localhost/nowhere" in
  let refused = Caqti_error.connect_failed ~uri (Caqti_error.Msg "connection refused") in
  Alcotest.(check bool) "connect failed" true (Transient.of_error refused);
  let malformed =
    Caqti_error.decode_rejected ~uri ~typ:Caqti_type.int (Caqti_error.Msg "not a number")
  in
  Alcotest.(check bool) "a value that cannot be read" false (Transient.of_error malformed);
  let error = Transient.driver_error refused in
  Alcotest.(check bool) "carried by the port" true error.transient

let test_session_errors_of_the_moment () =
  let module Error = Ascetic_session.Session_error in
  let module Driver_error = Ascetic_session.Driver_error in
  Alcotest.(check bool)
    "a pool that timed out" true
    (Error.is_transient (Error.Acquire (Driver_error.transient "pool timed out")));
  Alcotest.(check bool)
    "a scope opened twice" false
    (Error.is_transient Error.Scope_already_open);
  Alcotest.(check bool)
    "a commit refused for a defect" false
    (Error.is_transient (Error.Commit (Driver_error.defect "not the driver's")))

let () =
  Alcotest.run "Ascetic_session_caqti"
    [
      ( "identifier",
        [
          Alcotest.test_case "names that may go into SQL" `Quick
            test_names_that_may_go_into_sql;
          Alcotest.test_case "names that may not" `Quick test_names_that_may_not;
        ] );
      ( "transient",
        [
          Alcotest.test_case "the moment is told from the defect by SQLSTATE" `Quick
            test_the_moment_is_told_from_the_defect_by_sqlstate;
          Alcotest.test_case "a connection that cannot be made is of the moment" `Quick
            test_a_connection_that_cannot_be_made_is_of_the_moment;
          Alcotest.test_case "session errors of the moment" `Quick
            test_session_errors_of_the_moment;
        ] );
    ]
