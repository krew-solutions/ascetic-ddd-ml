(** Integration tests for the Transactional Inbox against a real Postgres.

    Skipped (silent success) when [TEST_DATABASE_URL] is not set; see
    [docker-compose.yml] at the repo root for a one-line local setup. *)

module Inbox = Ascetic_inbox.Inbox
module Inbox_message = Ascetic_inbox.Inbox_message
module Causal_dependency = Ascetic_inbox.Causal_dependency
module Provider = Ascetic_unit_of_work.Caqti_connection_provider
module Uow = Ascetic_unit_of_work.Caqti_unit_of_work

let table = "inbox_test"
let sequence = "inbox_test_received_position_seq"

(* -------------------------------------------------------------------------- *)
(* Connection helpers                                                         *)
(* -------------------------------------------------------------------------- *)

let database_url () = Sys.getenv_opt "TEST_DATABASE_URL"

let connect ~sw ~stdenv uri =
  match Caqti_eio_unix.connect ~sw ~stdenv uri with
  | Ok conn -> conn
  | Error err -> Alcotest.failf "connect failed: %a" Caqti_error.pp err

let exec_sql conn sql =
  let module C = (val conn : Caqti_eio.CONNECTION) in
  let open Caqti_request.Infix in
  let open Caqti_type in
  match C.exec ((unit ->. unit) sql) () with
  | Ok () -> ()
  | Error err ->
      Alcotest.failf "exec_sql failed: %a\nSQL: %s" Caqti_error.pp err sql

let drop_objects conn =
  exec_sql conn (Printf.sprintf "DROP TABLE IF EXISTS %s" table);
  exec_sql conn (Printf.sprintf "DROP SEQUENCE IF EXISTS %s" sequence)

let truncate_table conn =
  exec_sql conn (Printf.sprintf "TRUNCATE TABLE %s" table)

(* Round-robin pool for concurrency tests; wraps N pre-opened connections. *)
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

let make_inbox ?partition conn =
  Inbox.create ~table ~sequence ?partition
    ~provider:(Provider.of_connection conn) ()

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected Error: %s" e

(* -------------------------------------------------------------------------- *)
(* Per-test fixture                                                           *)
(* -------------------------------------------------------------------------- *)

let with_inbox_env ?partition env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let conn = connect ~sw ~stdenv uri in
  let inbox = make_inbox ?partition conn in
  let uow = Uow.of_connection conn in
  unwrap (Inbox.setup inbox uow);
  let cleanup () =
    (try drop_objects conn with _ -> ());
    let module C = (val conn : Caqti_eio.CONNECTION) in
    let _ = C.disconnect () in
    ()
  in
  truncate_table conn;
  match body env conn inbox with
  | () -> cleanup ()
  | exception exn ->
      cleanup ();
      raise exn

(* -------------------------------------------------------------------------- *)
(* Message helpers                                                            *)
(* -------------------------------------------------------------------------- *)

let make_message ?metadata ~stream_position ~stream_id_value ~uri ~payload () =
  Inbox_message.make
    ~tenant_id:"tenant1"
    ~stream_type:"Order"
    ~stream_id:(`Assoc [ ("id", `String stream_id_value) ])
    ~stream_position
    ~uri
    ~payload:(`Assoc payload)
    ?metadata
    ()

type recorder = { mutable seen : Inbox_message.t list }

let make_recorder () = { seen = [] }

let recording_subscriber recorder : Inbox.subscriber =
 fun _uow msg ->
  recorder.seen <- recorder.seen @ [ msg ];
  Ok ()

let payload_int (msg : Inbox_message.t) key =
  match msg.payload with
  | `Assoc fs -> (
      match List.assoc_opt key fs with
      | Some (`Int n) -> n
      | _ -> Alcotest.failf "expected int at key %s" key)
  | _ -> Alcotest.failf "expected assoc payload"

(* -------------------------------------------------------------------------- *)
(* Tests                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_publish_and_dispatch env uri () =
  with_inbox_env env uri @@ fun _env _conn inbox ->
  unwrap
    (Inbox.publish inbox
       (make_message ~stream_position:1 ~stream_id_value:"order-123"
          ~uri:"kafka://orders"
          ~payload:[ ("amount", `Int 100) ]
          ~metadata:
            (`Assoc
              [ ("event_id", `String "550e8400-e29b-41d4-a716-446655440000") ])
          ()));
  let recorder = make_recorder () in
  let dispatched =
    unwrap (Inbox.dispatch inbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "dispatched true" true dispatched;
  Alcotest.(check int) "1 message seen" 1 (List.length recorder.seen);
  let m = List.hd recorder.seen in
  Alcotest.(check string) "tenant" "tenant1" m.tenant_id;
  (match m.stream_id with
  | `Assoc [ ("id", `String s) ] ->
      Alcotest.(check string) "stream_id" "order-123" s
  | _ -> Alcotest.fail "unexpected stream_id shape")

let test_idempotency env uri () =
  with_inbox_env env uri @@ fun _env _conn inbox ->
  let msg =
    make_message ~stream_position:1 ~stream_id_value:"order-123"
      ~uri:"kafka://orders"
      ~payload:[ ("amount", `Int 100) ]
      ()
  in
  unwrap (Inbox.publish inbox msg);
  unwrap (Inbox.publish inbox msg);
  let recorder = make_recorder () in
  let _ = unwrap (Inbox.dispatch inbox (recording_subscriber recorder)) in
  let again = unwrap (Inbox.dispatch inbox (recording_subscriber recorder)) in
  Alcotest.(check bool) "no second dispatch" false again;
  Alcotest.(check int) "exactly one delivery" 1 (List.length recorder.seen)

let test_causal_dependencies env uri () =
  with_inbox_env env uri @@ fun _env _conn inbox ->
  let dependent =
    let dep =
      Causal_dependency.make
        ~tenant_id:"tenant1"
        ~stream_type:"Order"
        ~stream_id:(`Assoc [ ("id", `String "order-123") ])
        ~stream_position:1
    in
    let metadata =
      `Assoc
        [ ("causal_dependencies", `List [ Causal_dependency.to_json dep ]) ]
    in
    make_message ~stream_position:2 ~stream_id_value:"order-123"
      ~uri:"kafka://shipments"
      ~payload:[ ("tracking", `String "123") ]
      ~metadata ()
  in
  unwrap (Inbox.publish inbox dependent);

  let recorder = make_recorder () in
  let blocked =
    unwrap (Inbox.dispatch inbox (recording_subscriber recorder))
  in
  Alcotest.(check bool) "no dispatch when dep missing" false blocked;

  let dependency =
    make_message ~stream_position:1 ~stream_id_value:"order-123"
      ~uri:"kafka://orders"
      ~payload:[ ("amount", `Int 100) ]
      ()
  in
  unwrap (Inbox.publish inbox dependency);

  let r1 = unwrap (Inbox.dispatch inbox (recording_subscriber recorder)) in
  Alcotest.(check bool) "dependency processed" true r1;
  Alcotest.(check string) "first is orders" "kafka://orders"
    (List.nth recorder.seen 0).uri;

  let r2 = unwrap (Inbox.dispatch inbox (recording_subscriber recorder)) in
  Alcotest.(check bool) "dependent processed" true r2;
  Alcotest.(check string) "second is shipments" "kafka://shipments"
    (List.nth recorder.seen 1).uri

let test_ordering_by_received_position env uri () =
  with_inbox_env env uri @@ fun _env _conn inbox ->
  for i = 0 to 2 do
    unwrap
      (Inbox.publish inbox
         (make_message ~stream_position:1
            ~stream_id_value:(Printf.sprintf "order-%d" i)
            ~uri:"kafka://orders"
            ~payload:
              [ ("type", `String "OrderCreated"); ("order", `Int i) ]
            ()))
  done;
  let recorder = make_recorder () in
  let rec drain () =
    if unwrap (Inbox.dispatch inbox (recording_subscriber recorder))
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

let test_routing_by_uri env uri () =
  with_inbox_env env uri @@ fun _env _conn inbox ->
  unwrap
    (Inbox.publish inbox
       (make_message ~stream_position:1 ~stream_id_value:"order-1"
          ~uri:"kafka://orders"
          ~payload:[ ("type", `String "OrderCreated") ]
          ()));
  unwrap
    (Inbox.publish inbox
       (make_message ~stream_position:1 ~stream_id_value:"order-2"
          ~uri:"kafka://shipments"
          ~payload:[ ("type", `String "OrderShipped") ]
          ()));
  let routed = ref [] in
  let subscriber : Inbox.subscriber =
   fun _uow m ->
    let kind =
      if m.uri = "kafka://orders" then "orders" else "shipments"
    in
    routed := !routed @ [ kind ];
    Ok ()
  in
  let _ = unwrap (Inbox.dispatch inbox subscriber) in
  let _ = unwrap (Inbox.dispatch inbox subscriber) in
  Alcotest.(check (list string)) "routed in order"
    [ "orders"; "shipments" ] !routed

let test_iterator env uri () =
  with_inbox_env env uri @@ fun env _conn inbox ->
  for i = 0 to 1 do
    unwrap
      (Inbox.publish inbox
         (make_message ~stream_position:1
            ~stream_id_value:(Printf.sprintf "order-%d" i)
            ~uri:"kafka://orders"
            ~payload:
              [ ("type", `String "OrderCreated"); ("order", `Int i) ]
            ()))
  done;
  let it =
    Inbox.Iter.start ~clock:(Eio.Stdenv.mono_clock env) inbox
  in
  let _, m1 =
    match Inbox.Iter.next it with
    | Some pair -> pair
    | None -> Alcotest.fail "expected first message"
  in
  let _, m2 =
    match Inbox.Iter.next it with
    | Some pair -> pair
    | None -> Alcotest.fail "expected second message"
  in
  Inbox.Iter.close it;
  Alcotest.(check int) "order 0" 0 (payload_int m1 "order");
  Alcotest.(check int) "order 1" 1 (payload_int m2 "order")

let test_run_with_single_worker env uri () =
  with_inbox_env env uri @@ fun env _conn inbox ->
  for i = 0 to 2 do
    unwrap
      (Inbox.publish inbox
         (make_message ~stream_position:1
            ~stream_id_value:(Printf.sprintf "order-%d" i)
            ~uri:"kafka://orders"
            ~payload:
              [ ("type", `String "OrderCreated"); ("order", `Int i) ]
            ()))
  done;
  let recorder = make_recorder () in
  let stop_flag = ref false in
  Eio.Fiber.both
    (fun () ->
      Inbox.run ~poll_interval:0.01
        ~stop:(fun () -> !stop_flag)
        inbox
        ~clock:(Eio.Stdenv.mono_clock env)
        (fun uow msg ->
          let r = recording_subscriber recorder uow msg in
          if List.length recorder.seen >= 3 then stop_flag := true;
          r))
    (fun () ->
      Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 2.0;
      stop_flag := true);
  Alcotest.(check int) "all 3 processed" 3 (List.length recorder.seen)

let test_run_with_multiple_workers env uri () =
  with_inbox_env env uri @@ fun env _conn inbox ->
  for i = 0 to 9 do
    unwrap
      (Inbox.publish inbox
         (make_message ~stream_position:1
            ~stream_id_value:(Printf.sprintf "order-%d" i)
            ~uri:"kafka://orders"
            ~payload:
              [ ("type", `String "OrderCreated"); ("order", `Int i) ]
            ()))
  done;
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let extra_conns = List.init 4 (fun _ -> connect ~sw ~stdenv uri) in
  let pool_inbox =
    Inbox.create ~table ~sequence
      ~provider:(pool_provider extra_conns) ()
  in
  let recorder = make_recorder () in
  let mu = Eio.Mutex.create () in
  let stop_flag = ref false in
  let subscriber : Inbox.subscriber =
   fun _uow msg ->
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
        recorder.seen <- recorder.seen @ [ msg ];
        if List.length recorder.seen >= 10 then stop_flag := true);
    Ok ()
  in
  Eio.Fiber.both
    (fun () ->
      Inbox.run ~poll_interval:0.01 ~concurrency:3
        ~stop:(fun () -> !stop_flag)
        pool_inbox
        ~clock:(Eio.Stdenv.mono_clock env)
        subscriber)
    (fun () ->
      Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 3.0;
      stop_flag := true);
  List.iter
    (fun c ->
      let module C = (val c : Caqti_eio.CONNECTION) in
      let _ = C.disconnect () in
      ())
    extra_conns;
  Alcotest.(check int) "all 10 processed" 10 (List.length recorder.seen)

let test_for_update_skip_locked env uri () =
  with_inbox_env env uri @@ fun env _conn inbox ->
  unwrap
    (Inbox.publish inbox
       (make_message ~stream_position:1 ~stream_id_value:"order-1"
          ~uri:"kafka://orders" ~payload:[] ()));
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let extra_conns = List.init 4 (fun _ -> connect ~sw ~stdenv uri) in
  let pool_inbox =
    Inbox.create ~table ~sequence
      ~provider:(pool_provider extra_conns) ()
  in
  let recorder = make_recorder () in
  let mu = Eio.Mutex.create () in
  let stop_flag = ref false in
  let subscriber : Inbox.subscriber =
   fun _uow msg ->
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
        recorder.seen <- recorder.seen @ [ msg ]);
    Ok ()
  in
  Eio.Fiber.both
    (fun () ->
      Inbox.run ~poll_interval:0.01 ~concurrency:3
        ~stop:(fun () -> !stop_flag)
        pool_inbox
        ~clock:(Eio.Stdenv.mono_clock env)
        subscriber)
    (fun () ->
      Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.3;
      stop_flag := true);
  List.iter
    (fun c ->
      let module C = (val c : Caqti_eio.CONNECTION) in
      let _ = C.disconnect () in
      ())
    extra_conns;
  Alcotest.(check int) "exactly one delivery" 1 (List.length recorder.seen)

(* -------------------------------------------------------------------------- *)
(* Runner                                                                     *)
(* -------------------------------------------------------------------------- *)

let cases env uri =
  [
    Alcotest.test_case "publish_and_dispatch" `Quick
      (test_publish_and_dispatch env uri);
    Alcotest.test_case "idempotency" `Quick (test_idempotency env uri);
    Alcotest.test_case "causal_dependencies" `Quick
      (test_causal_dependencies env uri);
    Alcotest.test_case "ordering_by_received_position" `Quick
      (test_ordering_by_received_position env uri);
    Alcotest.test_case "routing_by_uri" `Quick (test_routing_by_uri env uri);
    Alcotest.test_case "iterator" `Quick (test_iterator env uri);
    Alcotest.test_case "run_with_single_worker" `Quick
      (test_run_with_single_worker env uri);
    Alcotest.test_case "run_with_multiple_workers" `Quick
      (test_run_with_multiple_workers env uri);
    Alcotest.test_case "for_update_skip_locked" `Quick
      (test_for_update_skip_locked env uri);
  ]

let () =
  match database_url () with
  | None ->
      print_endline
        "[skip] inbox integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Inbox" [ ("integration", cases env uri) ]
