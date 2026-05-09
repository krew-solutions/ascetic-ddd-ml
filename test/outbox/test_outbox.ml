(** Integration tests for the Transactional Outbox against a real Postgres.

    The tests are skipped (silent success) when [TEST_DATABASE_URL] is not set.
    To run them locally:

    {v
      export TEST_DATABASE_URL=postgresql://user:pass@localhost/test_db
      dune runtest test/outbox
    v}

    Each test recreates [outbox_test] / [outbox_offsets_test] tables to keep
    state fully isolated. *)

module Uow = Ascetic_unit_of_work.Caqti_unit_of_work
module Outbox = Ascetic_outbox.Outbox
module Outbox_message = Ascetic_outbox.Outbox_message
module Provider = Ascetic_outbox.Connection_provider

let outbox_table = "outbox_test"
let offsets_table = "outbox_offsets_test"

(* -------------------------------------------------------------------------- *)
(* Connection / driver helpers                                                *)
(* -------------------------------------------------------------------------- *)

let caqti_err err = Format.asprintf "%a" Caqti_error.pp err

let database_url () = Sys.getenv_opt "TEST_DATABASE_URL"

let connect ~sw ~stdenv uri =
  match Caqti_eio_unix.connect ~sw ~stdenv uri with
  | Ok conn -> conn
  | Error err -> Alcotest.failf "connect failed: %a" Caqti_error.pp err

let exec_sql conn sql =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  let open Caqti_type in
  let req = (unit ->. unit) sql in
  match C.exec req () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "exec_sql failed: %a\nSQL: %s" Caqti_error.pp err sql

let drop_tables conn =
  exec_sql conn (Printf.sprintf "DROP TABLE IF EXISTS %s" outbox_table);
  exec_sql conn (Printf.sprintf "DROP TABLE IF EXISTS %s" offsets_table)

let truncate_tables conn =
  exec_sql conn (Printf.sprintf "TRUNCATE TABLE %s" outbox_table);
  exec_sql conn (Printf.sprintf "TRUNCATE TABLE %s" offsets_table)

let provider_of_connection conn = Provider.of_connection conn

(** A minimal round-robin pool of pre-opened connections. Used by tests
    that need [concurrency > 1] — each fiber takes a connection from the
    queue, uses it, returns it. Caqti detects concurrent reuse of a
    single connection and raises, so a real pool is required for any
    multi-fiber dispatch. *)
let pool_provider conns : Provider.t =
  let stream = Eio.Stream.create (List.length conns) in
  List.iter (Eio.Stream.add stream) conns;
  (module struct
    let with_connection f =
      let conn = Eio.Stream.take stream in
      let result =
        try f conn
        with exn ->
          Eio.Stream.add stream conn;
          raise exn
      in
      Eio.Stream.add stream conn;
      result
  end)

let make_outbox conn =
  Outbox.create ~outbox_table ~offsets_table
    ~provider:(provider_of_connection conn) ()

(* Run a publish inside a real BEGIN/COMMIT on [conn]. *)
let publish_in_tx outbox conn (msg : Outbox_message.t) =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let uow = Uow.of_connection conn in
  match C.start () with
  | Error err -> Error (caqti_err err)
  | Ok () -> (
      match Outbox.publish outbox uow msg with
      | Error e ->
          let _ = C.rollback () in
          Error e
      | Ok () -> Uow.commit uow)

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected Error: %s" e

(* -------------------------------------------------------------------------- *)
(* Per-test fixture: setup tables, run body, drop tables                      *)
(* -------------------------------------------------------------------------- *)

(** [with_outbox_env env uri body] establishes a fresh test environment:
    a connection, an outbox with [setup] applied, and tables truncated.
    Tables are dropped after the body completes regardless of outcome. *)
let with_outbox_env env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let conn = connect ~sw ~stdenv uri in
  let outbox = make_outbox conn in
  let uow = Uow.of_connection conn in
  unwrap (Outbox.setup outbox uow);
  let cleanup () =
    (try drop_tables conn with _ -> ());
    let module C = (val conn : Caqti_eio.CONNECTION) in
    let _ = C.disconnect () in
    ()
  in
  truncate_tables conn;
  match body env conn outbox with
  | () -> cleanup ()
  | exception exn ->
      cleanup ();
      raise exn

(* Build an OutboxMessage with a deterministic event_id for idempotency. *)
let make_message ~event_id ~uri ~payload =
  Outbox_message.make ~uri
    ~payload:(`Assoc payload)
    ~metadata:(`Assoc [ ("event_id", `String event_id) ])
    ()

(* -------------------------------------------------------------------------- *)
(* Subscriber that records messages it has seen                               *)
(* -------------------------------------------------------------------------- *)

type recorder = { mutable seen : Outbox_message.t list }

let make_recorder () = { seen = [] }

let recording_subscriber recorder : Outbox.subscriber =
 fun msg ->
  recorder.seen <- recorder.seen @ [ msg ];
  Ok ()

let payload_field msg key =
  match msg.Outbox_message.payload with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let payload_string msg key =
  match payload_field msg key with
  | Some (`String s) -> s
  | _ -> Alcotest.failf "expected string at key %s" key

let payload_int msg key =
  match payload_field msg key with
  | Some (`Int n) -> n
  | _ -> Alcotest.failf "expected int at key %s" key

(* -------------------------------------------------------------------------- *)
(* Tests                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_publish_and_dispatch env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  unwrap
    (publish_in_tx outbox conn
       (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440001"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated"); ("order_id", `String "123") ]));
  let recorder = make_recorder () in
  let dispatched =
    unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "dispatched true" true dispatched;
  Alcotest.(check int) "1 message seen" 1 (List.length recorder.seen);
  let m = List.hd recorder.seen in
  Alcotest.(check string) "uri" "kafka://orders" m.uri;
  Alcotest.(check string) "order_id" "123" (payload_string m "order_id")

let test_dispatch_returns_false_when_empty env uri () =
  with_outbox_env env uri @@ fun _env _conn outbox ->
  let recorder = make_recorder () in
  let dispatched =
    unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "dispatched false" false dispatched;
  Alcotest.(check int) "no messages" 0 (List.length recorder.seen)

let test_dispatch_updates_position env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  unwrap
    (publish_in_tx outbox conn
       (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440002"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated") ]));
  let recorder = make_recorder () in
  let _ =
    unwrap
      (Outbox.dispatch ~consumer_group:"test-group" outbox
         (recording_subscriber recorder))
  in
  let dispatched_again =
    unwrap
      (Outbox.dispatch ~consumer_group:"test-group" outbox
         (recording_subscriber recorder))
  in
  Alcotest.(check bool) "second dispatch returns false" false dispatched_again

let test_multiple_consumer_groups env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  unwrap
    (publish_in_tx outbox conn
       (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440003"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated") ]));
  let r1 = make_recorder () in
  let _ =
    unwrap
      (Outbox.dispatch ~consumer_group:"group-1" outbox
         (recording_subscriber r1))
  in
  Alcotest.(check int) "group-1 sees 1" 1 (List.length r1.seen);
  let r2 = make_recorder () in
  let _ =
    unwrap
      (Outbox.dispatch ~consumer_group:"group-2" outbox
         (recording_subscriber r2))
  in
  Alcotest.(check int) "group-2 sees same 1" 1 (List.length r2.seen)

let test_ordering_by_position env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  for i = 0 to 2 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:(Printf.sprintf "550e8400-e29b-41d4-a716-44665544000%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]))
  done;
  let recorder = make_recorder () in
  let rec drain () =
    if unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
    then drain ()
  in
  drain ();
  Alcotest.(check int) "all 3 dispatched" 3 (List.length recorder.seen);
  List.iteri
    (fun i m ->
      Alcotest.(check int)
        (Printf.sprintf "order at index %d" i)
        i (payload_int m "order"))
    recorder.seen

let test_batch_dispatch env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  for i = 0 to 4 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544010%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]))
  done;
  let recorder = make_recorder () in
  let dispatched =
    unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "true" true dispatched;
  Alcotest.(check int) "5 in single batch" 5 (List.length recorder.seen)

let test_get_and_set_position env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  let uow = Uow.of_connection conn in
  let txid, off = unwrap (Outbox.get_position ~consumer_group:"test-group" outbox uow) in
  Alcotest.(check string) "initial txid is 0" "0" txid;
  Alcotest.(check int64) "initial offset is 0" 0L off;
  unwrap
    (Outbox.set_position outbox uow ~consumer_group:"test-group" ~uri:""
       ~transaction_id:"100" ~offset:50L);
  let txid2, off2 =
    unwrap (Outbox.get_position ~consumer_group:"test-group" outbox uow)
  in
  Alcotest.(check string) "txid 100" "100" txid2;
  Alcotest.(check int64) "offset 50" 50L off2

let test_get_and_set_position_with_uri env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  let uow = Uow.of_connection conn in
  unwrap
    (Outbox.set_position outbox uow ~consumer_group:"test-group"
       ~uri:"kafka://orders" ~transaction_id:"100" ~offset:50L);
  unwrap
    (Outbox.set_position outbox uow ~consumer_group:"test-group"
       ~uri:"kafka://users" ~transaction_id:"200" ~offset:30L);
  let orders =
    unwrap
      (Outbox.get_position ~consumer_group:"test-group" ~uri:"kafka://orders"
         outbox uow)
  in
  let users =
    unwrap
      (Outbox.get_position ~consumer_group:"test-group" ~uri:"kafka://users"
         outbox uow)
  in
  Alcotest.(check (pair string int64)) "orders" ("100", 50L) orders;
  Alcotest.(check (pair string int64)) "users" ("200", 30L) users

let test_dispatch_with_uri_filter env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  let publish_one event_id u kind =
    unwrap
      (publish_in_tx outbox conn
         (make_message ~event_id ~uri:u
            ~payload:[ ("type", `String kind) ]))
  in
  publish_one "550e8400-e29b-41d4-a716-446655440080" "kafka://orders"
    "OrderCreated";
  publish_one "550e8400-e29b-41d4-a716-446655440081" "kafka://users"
    "UserCreated";
  publish_one "550e8400-e29b-41d4-a716-446655440082" "kafka://orders"
    "OrderShipped";
  let recorder = make_recorder () in
  let r1 =
    unwrap
      (Outbox.dispatch ~consumer_group:"orders-consumer" ~uri:"kafka://orders"
         outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "orders dispatch true" true r1;
  Alcotest.(check int) "2 orders" 2 (List.length recorder.seen);
  Alcotest.(check bool)
    "all are kafka://orders" true
    (List.for_all (fun m -> m.Outbox_message.uri = "kafka://orders") recorder.seen);
  let r2 =
    unwrap
      (Outbox.dispatch ~consumer_group:"orders-consumer" ~uri:"kafka://orders"
         outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "orders dispatch again returns false" false r2;
  let r3 =
    unwrap
      (Outbox.dispatch ~consumer_group:"orders-consumer" ~uri:"kafka://users"
         outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "users dispatch true" true r3;
  Alcotest.(check int) "now 3 in total" 3 (List.length recorder.seen);
  Alcotest.(check string) "third is users" "kafka://users"
    (List.nth recorder.seen 2).uri

let test_multiple_uris_independent_positions env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  for i = 0 to 2 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544009%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]));
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544019%d" i)
            ~uri:"kafka://users"
            ~payload:[ ("type", `String "UserCreated"); ("user", `Int i) ]))
  done;
  let orders = make_recorder () in
  let rec drain_orders () =
    if
      unwrap
        (Outbox.dispatch ~consumer_group:"group1" ~uri:"kafka://orders"
           outbox (recording_subscriber orders))
    then drain_orders ()
  in
  drain_orders ();
  let users = make_recorder () in
  let rec drain_users () =
    if
      unwrap
        (Outbox.dispatch ~consumer_group:"group1" ~uri:"kafka://users"
           outbox (recording_subscriber users))
    then drain_users ()
  in
  drain_users ();
  Alcotest.(check int) "orders 3" 3 (List.length orders.seen);
  Alcotest.(check int) "users 3" 3 (List.length users.seen);
  Alcotest.(check bool)
    "orders only" true
    (List.for_all (fun m -> m.Outbox_message.uri = "kafka://orders")
       orders.seen);
  Alcotest.(check bool)
    "users only" true
    (List.for_all (fun m -> m.Outbox_message.uri = "kafka://users") users.seen)

let test_idempotency_via_event_id env uri () =
  with_outbox_env env uri @@ fun _env conn outbox ->
  unwrap
    (publish_in_tx outbox conn
       (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440060"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated"); ("order_id", `String "123") ]));
  match
    publish_in_tx outbox conn
      (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440060"
         ~uri:"kafka://orders"
         ~payload:[ ("type", `String "OrderCreated"); ("order_id", `String "456") ])
  with
  | Ok () ->
      Alcotest.fail "expected unique violation on duplicate event_id"
  | Error _ -> ()

let test_visibility_rule env uri () =
  with_outbox_env env uri @@ fun _env _conn outbox ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let other = connect ~sw ~stdenv uri in
  let module Other = (val other : Caqti_eio.CONNECTION) in
  unwrap
    ((match Other.start () with
      | Ok () -> Ok ()
      | Error e -> Error (caqti_err e)));
  let open Caqti_request.Infix in
  let open Caqti_type in
  let req =
    (unit ->. unit)
      (Printf.sprintf
         "INSERT INTO %s (uri, payload, metadata, transaction_id) \
          VALUES ('kafka://orders', '{\"type\":\"OrderCreated\"}'::jsonb, \
          '{\"event_id\":\"550e8400-e29b-41d4-a716-446655440050\"}'::jsonb, \
          pg_current_xact_id())"
         outbox_table)
  in
  unwrap
    (match Other.exec req () with
    | Ok () -> Ok ()
    | Error e -> Error (caqti_err e));
  (* Before the other transaction commits, dispatcher must see nothing. *)
  let recorder = make_recorder () in
  let dispatched_before =
    unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "invisible before commit" false dispatched_before;
  Alcotest.(check int) "no rows" 0 (List.length recorder.seen);
  (* Commit the other transaction. *)
  unwrap
    (match Other.commit () with
    | Ok () -> Ok ()
    | Error e -> Error (caqti_err e));
  let dispatched_after =
    unwrap (Outbox.dispatch outbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "visible after commit" true dispatched_after;
  Alcotest.(check int) "1 row" 1 (List.length recorder.seen);
  let _ = Other.disconnect () in
  ()

let test_run_with_single_worker env uri () =
  with_outbox_env env uri @@ fun env conn outbox ->
  for i = 0 to 2 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544030%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]))
  done;
  let recorder = make_recorder () in
  let stop_flag = ref false in
  Eio.Fiber.both
    (fun () ->
      Outbox.run ~poll_interval:0.01 ~stop:(fun () -> !stop_flag) outbox
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun msg ->
          let r = recording_subscriber recorder msg in
          if List.length recorder.seen >= 3 then stop_flag := true;
          r))
    (fun () ->
      (* Safety net: if [run] never sees all messages, force-stop. *)
      Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 2.0;
      stop_flag := true);
  Alcotest.(check int) "all 3 processed" 3 (List.length recorder.seen)

let test_run_with_multiple_workers env uri () =
  with_outbox_env env uri @@ fun env conn outbox ->
  for i = 0 to 9 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544040%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]))
  done;
  (* For concurrency > 1 we need a real pool: a single connection cannot
     be used from multiple fibers simultaneously (Caqti raises). Open
     extra connections and wrap them in [pool_provider]. *)
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let extra_conns = List.init 4 (fun _ -> connect ~sw ~stdenv uri) in
  let pool_outbox =
    Outbox.create ~outbox_table ~offsets_table
      ~provider:(pool_provider extra_conns) ()
  in
  let recorder = make_recorder () in
  let mu = Eio.Mutex.create () in
  let stop_flag = ref false in
  let subscriber msg =
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
        recorder.seen <- recorder.seen @ [ msg ];
        if List.length recorder.seen >= 10 then stop_flag := true);
    Ok ()
  in
  Eio.Fiber.both
    (fun () ->
      Outbox.run ~poll_interval:0.01 ~concurrency:3
        ~stop:(fun () -> !stop_flag)
        pool_outbox
        ~clock:(Eio.Stdenv.mono_clock env)
        subscriber)
    (fun () ->
      (* Safety net. *)
      Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 3.0;
      stop_flag := true);
  List.iter
    (fun c ->
      let module C = (val c : Caqti_eio.CONNECTION) in
      let _ = C.disconnect () in
      ())
    extra_conns;
  Alcotest.(check int) "all 10 processed" 10 (List.length recorder.seen)

let test_iterator env uri () =
  with_outbox_env env uri @@ fun env conn outbox ->
  for i = 0 to 1 do
    unwrap
      (publish_in_tx outbox conn
         (make_message
            ~event_id:
              (Printf.sprintf "550e8400-e29b-41d4-a716-44665544020%d" i)
            ~uri:"kafka://orders"
            ~payload:[ ("type", `String "OrderCreated"); ("order", `Int i) ]))
  done;
  let it =
    Outbox.Iter.start ~clock:(Eio.Stdenv.mono_clock env) outbox
  in
  let m1 =
    match Outbox.Iter.next it with
    | Some m -> m
    | None -> Alcotest.fail "expected first message"
  in
  let m2 =
    match Outbox.Iter.next it with
    | Some m -> m
    | None -> Alcotest.fail "expected second message"
  in
  Outbox.Iter.close it;
  Alcotest.(check int) "order 0" 0 (payload_int m1 "order");
  Alcotest.(check int) "order 1" 1 (payload_int m2 "order")

let test_for_update_prevents_duplicate_processing env uri () =
  with_outbox_env env uri @@ fun env conn outbox ->
  unwrap
    (publish_in_tx outbox conn
       (make_message ~event_id:"550e8400-e29b-41d4-a716-446655440070"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated"); ("order_id", `String "123") ]));
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  (* Each fiber dispatches with its own connection so the [FOR UPDATE]
     lock genuinely competes between sessions. *)
  let dispatch_with_own_conn recorder =
    let conn' = connect ~sw ~stdenv uri in
    let outbox' =
      Outbox.create ~outbox_table ~offsets_table
        ~provider:(provider_of_connection conn')
        ()
    in
    let result =
      Outbox.dispatch ~consumer_group:"test-group" outbox'
        (fun msg ->
          (* Small delay so siblings have time to contend for the lock. *)
          Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.02;
          recording_subscriber recorder msg)
    in
    let module C = (val conn' : Caqti_eio.CONNECTION) in
    let _ = C.disconnect () in
    result
  in
  let r = make_recorder () in
  let results =
    Eio.Fiber.List.map
      (fun () -> dispatch_with_own_conn r)
      [ (); (); () ]
  in
  let successes =
    List.filter (function Ok true -> true | _ -> false) results
    |> List.length
  in
  Alcotest.(check int) "exactly one dispatcher saw a message" 1 successes;
  Alcotest.(check int) "exactly one delivery" 1 (List.length r.seen)

(* -------------------------------------------------------------------------- *)
(* Test runner                                                                *)
(* -------------------------------------------------------------------------- *)

let cases env uri =
  [
    Alcotest.test_case "publish_and_dispatch" `Quick
      (test_publish_and_dispatch env uri);
    Alcotest.test_case "dispatch_returns_false_when_empty" `Quick
      (test_dispatch_returns_false_when_empty env uri);
    Alcotest.test_case "dispatch_updates_position" `Quick
      (test_dispatch_updates_position env uri);
    Alcotest.test_case "multiple_consumer_groups" `Quick
      (test_multiple_consumer_groups env uri);
    Alcotest.test_case "ordering_by_position" `Quick
      (test_ordering_by_position env uri);
    Alcotest.test_case "batch_dispatch" `Quick (test_batch_dispatch env uri);
    Alcotest.test_case "get_and_set_position" `Quick
      (test_get_and_set_position env uri);
    Alcotest.test_case "get_and_set_position_with_uri" `Quick
      (test_get_and_set_position_with_uri env uri);
    Alcotest.test_case "dispatch_with_uri_filter" `Quick
      (test_dispatch_with_uri_filter env uri);
    Alcotest.test_case "multiple_uris_independent_positions" `Quick
      (test_multiple_uris_independent_positions env uri);
    Alcotest.test_case "idempotency_via_event_id" `Quick
      (test_idempotency_via_event_id env uri);
    Alcotest.test_case "visibility_rule" `Quick (test_visibility_rule env uri);
    Alcotest.test_case "run_with_single_worker" `Quick
      (test_run_with_single_worker env uri);
    Alcotest.test_case "run_with_multiple_workers" `Quick
      (test_run_with_multiple_workers env uri);
    Alcotest.test_case "iterator" `Quick (test_iterator env uri);
    Alcotest.test_case "for_update_prevents_duplicate_processing" `Quick
      (test_for_update_prevents_duplicate_processing env uri);
  ]

let () =
  match database_url () with
  | None ->
      print_endline
        "[skip] outbox integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Outbox" [ ("integration", cases env uri) ]
