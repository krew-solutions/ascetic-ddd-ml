(** The REST session: the client reached through the capability, scopes reported as
    logical, requests timed, and the guard against two scopes side by side. *)

module Rest = Ascetic_session_rest.Rest_session
module Rest_pool = Ascetic_session_rest.Rest_session_pool
module Rest_observer = Ascetic_session_rest.Rest_observer
module Observer = Ascetic_session.Session_observer
module Session_error = Ascetic_session.Session_error

type app_error = Session of Session_error.t | Domain of string

let lift error = Session error

(* A stand-in for an HTTP client: the library depends on none. *)
module Fake_client = struct
  type t = { mutable calls : string list }

  let create () = { calls = [] }
  let calls t = List.rev t.calls

  let get t url =
    t.calls <- url :: t.calls;
    Ok 200
end

(* The infrastructure: a gateway that names the capability. *)
let fetch_customer session id =
  let url = Printf.sprintf "https://example.test/customers/%d" id in
  Result.map
    (fun status -> (id, status))
    (Rest.request session ~meth:"GET" ~url (fun () ->
         Fake_client.get (Rest.http session) url))

(* Code polymorphic in the session is given the port. *)
module Rest_port = Rest.Of (Fake_client)

module Use_case (S : Ascetic_session.Session.S) = struct
  let run session work = S.atomic session ~lift work
end

module Rest_use_case = Use_case (Rest_port)

let recording () =
  let scopes = ref [] and requests = ref [] and timed = ref 0 in
  let show (scope : Observer.scope) =
    Printf.sprintf "%d %s" scope.depth
      (match scope.kind with
      | Observer.Session -> "session"
      | Observer.Transaction -> "transaction"
      | Observer.Savepoint -> "savepoint"
      | Observer.Logical -> "logical")
  in
  let observer : Rest_observer.t =
    {
      scopes =
        {
          on_scope_started = (fun scope -> scopes := ("+" ^ show scope) :: !scopes);
          on_scope_ended =
            (fun scope outcome ->
              scopes :=
                Printf.sprintf "-%s %s" (show scope)
                  (match outcome with
                  | Observer.Succeeded -> "succeeded"
                  | Observer.Failed -> "failed")
                :: !scopes);
        };
      on_request_started = ignore;
      on_request_ended =
        (fun request ~elapsed ~failed ->
          requests :=
            Printf.sprintf "%s %s%s" request.meth request.url
              (if failed then " failed" else "")
            :: !requests;
          if elapsed > 0.0 then incr timed);
    }
  in
  (observer, (fun () -> List.rev !scopes), (fun () -> List.rev !requests), timed)

(* A clock that moves with every reading, so that a request takes time. *)
let ticking () =
  let clock = Eio_mock.Clock.Mono.make () in
  Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 1L);
  clock

let test_a_scope_reaches_the_client_through_the_capability env =
  let client = Fake_client.create () in
  let pool = Rest_pool.create ~clock:(Eio.Stdenv.mono_clock env) client in
  let outcome =
    Rest_pool.session pool ~lift (fun session ->
        Rest.atomic session ~lift (fun session -> fetch_customer session 7))
  in
  Alcotest.(check bool) "fetched" true (outcome = Ok (7, 200));
  Alcotest.(check (list string))
    "the shared client was called"
    [ "https://example.test/customers/7" ]
    (Fake_client.calls (Rest_pool.client pool))

let test_scopes_are_reported_as_logical env =
  let observer, scopes, requests, timed = recording () in
  let pool =
    Rest_pool.create ~observer ~clock:(Eio.Stdenv.mono_clock env) (Fake_client.create ())
  in
  let outcome =
    Rest_pool.session pool ~lift (fun session ->
        Rest.atomic session ~lift (fun session ->
            Result.bind (fetch_customer session 1) (fun _ ->
                Rest.atomic session ~lift (fun _nested -> Ok ()))))
  in
  Alcotest.(check bool) "ok" true (outcome = Ok ());
  Alcotest.(check (list string))
    "scopes"
    [
      "+0 session";
      "+1 logical";
      "+2 logical";
      "-2 logical succeeded";
      "-1 logical succeeded";
      "-0 session succeeded";
    ]
    (scopes ());
  Alcotest.(check (list string))
    "requests"
    [ "GET https://example.test/customers/1" ]
    (requests ());
  Alcotest.(check bool) "requests are timed" true (!timed > 0)

let test_a_failing_scope_is_reported_as_failed env =
  let observer, scopes, _, _ = recording () in
  let pool =
    Rest_pool.create ~observer ~clock:(Eio.Stdenv.mono_clock env) (Fake_client.create ())
  in
  let outcome =
    Rest_pool.session pool ~lift (fun session ->
        Rest.atomic session ~lift (fun _ -> Error (Domain "rejected")))
  in
  Alcotest.(check bool) "the scope's error" true (outcome = Error (Domain "rejected"));
  Alcotest.(check (list string))
    "both scopes failed"
    [ "-1 logical failed"; "-0 session failed" ]
    (List.filter (fun line -> line.[0] = '-') (scopes ()))

let test_a_raising_scope_is_reported_as_failed env =
  let observer, scopes, requests, _ = recording () in
  let pool =
    Rest_pool.create ~observer ~clock:(Eio.Stdenv.mono_clock env) (Fake_client.create ())
  in
  let raised =
    match
      Rest_pool.session pool ~lift (fun session ->
          Rest.atomic session ~lift (fun session ->
              Rest.request session ~meth:"GET" ~url:"https://example.test/" (fun () ->
                  failwith "the client broke")))
    with
    | (_ : (unit, app_error) result) -> false
    | exception Failure _ -> true
  in
  Alcotest.(check bool) "the exception went on" true raised;
  Alcotest.(check (list string))
    "the request failed"
    [ "GET https://example.test/ failed" ]
    (requests ());
  Alcotest.(check (list string))
    "both scopes failed"
    [ "-1 logical failed"; "-0 session failed" ]
    (List.filter (fun line -> line.[0] = '-') (scopes ()))

let test_a_second_scope_on_the_same_session_is_refused env =
  let pool =
    Rest_pool.create ~clock:(Eio.Stdenv.mono_clock env) (Fake_client.create ())
  in
  let outcome =
    Rest_pool.session pool ~lift (fun session ->
        Rest.atomic session ~lift (fun _child ->
            let second = Rest.atomic session ~lift (fun _ -> Ok ()) in
            Alcotest.(check bool)
              "refused beside the first" true
              (second = Error (Session Session_error.Scope_already_open));
            Ok ())
        |> fun first ->
        (* Once the first scope has ended, the session opens another. *)
        Result.bind first (fun () -> Rest.atomic session ~lift (fun _ -> Ok ())))
  in
  Alcotest.(check bool) "ok" true (outcome = Ok ())

let test_the_session_fits_the_port env =
  let session = Rest.create ~clock:(Eio.Stdenv.mono_clock env) (Fake_client.create ()) in
  let outcome = Rest_use_case.run session (fun session -> fetch_customer session 3) in
  Alcotest.(check bool) "through the port" true (outcome = Ok (3, 200))

let test_a_request_is_timed_by_the_session_s_clock () =
  let clock = ticking () in
  let elapsed_seen = ref [] in
  let observer =
    {
      Rest_observer.none with
      on_request_ended =
        (fun _ ~elapsed ~failed:_ -> elapsed_seen := elapsed :: !elapsed_seen);
    }
  in
  let session = Rest.create ~observer ~clock (Fake_client.create ()) in
  let outcome =
    Rest.request session ~meth:"GET" ~url:"https://example.test/" (fun () ->
        Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 1_500_000_001L);
        Ok ())
  in
  Alcotest.(check bool) "ok" true (outcome = (Ok () : (unit, app_error) result));
  Alcotest.(check (list (float 1e-9))) "a second and a half" [ 1.5 ] !elapsed_seen

let () =
  Eio_main.run @@ fun env ->
  let case name test = Alcotest.test_case name `Quick (fun () -> test env) in
  Alcotest.run "Rest_session"
    [
      ( "rest",
        [
          case "a scope reaches the client through the capability"
            test_a_scope_reaches_the_client_through_the_capability;
          case "scopes are reported as logical" test_scopes_are_reported_as_logical;
          case "a failing scope is reported as failed"
            test_a_failing_scope_is_reported_as_failed;
          case "a raising scope is reported as failed"
            test_a_raising_scope_is_reported_as_failed;
          case "a second scope on the same session is refused"
            test_a_second_scope_on_the_same_session_is_refused;
          case "the session fits the port" test_the_session_fits_the_port;
          Alcotest.test_case "a request is timed by the session's clock" `Quick
            test_a_request_is_timed_by_the_session_s_clock;
        ] );
    ]
