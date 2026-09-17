(** An outbox feeding an inbox, watched by one recorder: the dispatcher's subscriber
    stores each message in the inbox, in a transaction of the inbox's own, and the inbox
    processes it once in the transaction that marks it. Needs a live database, like the
    outbox and inbox suites; with [ASCETIC_DDD_TRACE_DIR] set it writes
    [bridge-outbox-to-inbox.jsonl], the run [verify/tla/TraceBridge.tla] checks. *)

module Outbox = Ascetic_outbox.Pg_outbox
module Outbox_message = Ascetic_outbox.Outbox_message
module Selection = Ascetic_outbox.Selection
module Inbox = Ascetic_inbox.Pg_inbox
module Inbox_message = Ascetic_inbox.Inbox_message
module Failure = Ascetic_inbox.Failure
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Trace_file = Ascetic_trace.Trace_file
module Json_trace = Ascetic_trace.Json_trace

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> Ok ()
  | Error err -> Error (Caqti_error.show err)

let exec_exn session sql =
  match exec session sql with
  | Ok () -> ()
  | Error reason -> Alcotest.failf "%s: %s" sql reason

let find session request =
  let module C = (val Session.connection session) in
  match C.find request () with
  | Ok value -> value
  | Error err -> Alcotest.failf "%a" Caqti_error.pp err

let find_int session sql =
  let open Caqti_request.Infix in
  find session ((Caqti_type.unit ->! Caqti_type.int) ~oneshot:true sql)

let find_string session sql =
  let open Caqti_request.Infix in
  find session ((Caqti_type.unit ->! Caqti_type.string) ~oneshot:true sql)

let unwrap what = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "%s: %s" what (Ascetic_inbox.Inbox_error.to_string error)

(* The wire crossing: the row the outbox dispatched, stored under the
   identity the message names. A bus would carry the identity as headers;
   here the message itself does. *)
let inbox_message_of (message : Outbox_message.t) =
  Inbox_message.with_metadata
    (Inbox_message.make ~tenant_id:"t1" ~stream_type:"orders.Order" ~stream_id:(`Int 7)
       ~stream_position:2 ~uri:message.uri ~payload:message.payload)
    message.metadata

let test_the_outbox_feeds_the_inbox env uri () =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let pool_config = Caqti_pool_config.create ~max_size:8 () in
  let sessions =
    match Caqti_eio_unix.connect_pool ~pool_config ~sw ~stdenv uri with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let clock = Eio.Stdenv.mono_clock env in
  let trace = Trace_file.from_env "bridge-outbox-to-inbox" in
  let recorder = Trace_file.recorder trace in
  let outbox_table = "bridge_outbox" and inbox_table = "bridge_inbox" in
  let outbox : Ascetic_inbox.Inbox_error.t Outbox.t =
    Outbox.create
      ~observer:(Json_trace.outbox_observer recorder)
      ~outbox_table:(Identifier.of_string_exn outbox_table)
      ~offsets_table:(Identifier.of_string_exn (outbox_table ^ "_offsets"))
      sessions
  in
  let inbox =
    Inbox.create
      ~observer:(Json_trace.inbox_observer recorder)
      ~table:(Identifier.of_string_exn inbox_table)
      ~sequence:(Identifier.of_string_exn (inbox_table ^ "_seq"))
      sessions
  in
  let session body =
    unwrap "session"
      (Pool.session sessions ~lift:(fun e -> Ascetic_inbox.Inbox_error.Session e) body)
  in
  session (fun s ->
      exec_exn s
        (Printf.sprintf
           "DROP TABLE IF EXISTS %s, %s_meta, %s_offsets, %s, %s_meta, %s_slots, \
            %s_handled"
           outbox_table outbox_table outbox_table inbox_table inbox_table inbox_table
           inbox_table);
      exec_exn s (Printf.sprintf "DROP SEQUENCE IF EXISTS %s_seq" inbox_table);
      exec_exn s
        (Printf.sprintf "CREATE TABLE %s_handled (payload text NOT NULL)" inbox_table);
      let open Ascetic_outbox.Outbox_error in
      (match Outbox.setup outbox s with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "outbox setup: %s"
            (to_string Ascetic_inbox.Inbox_error.to_string error));
      Inbox.setup inbox s);
  Fun.protect ~finally:(fun () -> Trace_file.close trace) @@ fun () ->
  let handled, resolve_handled = Eio.Promise.create () in
  let stop, resolve_stop = Eio.Promise.create () in
  (* The bridge: what the outbox dispatches is stored in the inbox. *)
  let bridge message = Inbox.publish inbox (inbox_message_of message) in
  (* The handler writes through the transaction it is given and reports. *)
  let handler tx (message : Inbox_message.t) =
    match
      exec tx
        (Printf.sprintf "INSERT INTO %s_handled (payload) VALUES ('%s')" inbox_table
           message.payload)
    with
    | Ok () ->
        ignore (Eio.Promise.try_resolve resolve_handled message.payload);
        Ok ()
    | Error reason -> Error (Failure.transient reason)
  in
  let loops = 0.02 in
  let dispatching () =
    match
      Outbox.run outbox ~clock
        ~loops:{ Ascetic_outbox.Loops.default with poll_interval = loops }
        ~shutdown:stop
        (Selection.group "dispatcher")
        bridge
    with
    | Ok () -> ()
    | Error error ->
        Alcotest.failf "the dispatcher failed: %s"
          (Ascetic_outbox.Outbox_error.to_string Ascetic_inbox.Inbox_error.to_string error)
  in
  let processing () =
    unwrap "the processing loop"
      (Inbox.run inbox ~clock
         ~loops:{ Ascetic_inbox.Loops.default with poll_interval = loops }
         ~shutdown:stop handler)
  in
  let publishing () =
    session (fun s ->
        Session.atomic s
          ~lift:(fun e -> Ascetic_inbox.Inbox_error.Session e)
          (fun tx ->
            match
              Outbox.publish outbox tx
                (Outbox_message.make ~uri:"inbox://orders/order-7" ~payload:"shipped"
                   ~metadata:
                     (`Assoc
                        [ ("message_id", `String "00000000-0000-4000-8000-000000000002") ]))
            with
            | Ok () -> Ok ()
            | Error error ->
                Alcotest.failf "publish: %s"
                  (Ascetic_outbox.Outbox_error.to_string
                     Ascetic_inbox.Inbox_error.to_string error)));
    let payload =
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 20.0) (fun () ->
          Eio.Promise.await handled)
    in
    Alcotest.(check string) "processed once" "shipped" payload;
    (* The handler reports before the mark commits, so the count is waited
       for rather than read at once; then both loops are told to stop. *)
    let rec settled tries =
      let processed =
        session (fun s ->
            Ok
              (find_int s
                 (Printf.sprintf
                    "SELECT count(*) FROM %s WHERE processed_position IS NOT NULL"
                    inbox_table)))
      in
      if processed = 1 || tries = 0 then processed
      else begin
        Eio.Time.Mono.sleep clock 0.02;
        settled (tries - 1)
      end
    in
    Alcotest.(check int) "marked" 1 (settled 250);
    (* one more empty poll of each loop, so that the run ends as the recorded
       one does: with nothing left *)
    Eio.Time.Mono.sleep clock (loops *. 3.0);
    ignore (Eio.Promise.try_resolve resolve_stop ())
  in
  Eio.Fiber.all [ dispatching; processing; publishing ];
  Alcotest.(check int)
    "the handler's write committed with the mark" 1
    (session (fun s ->
         Ok (find_int s (Printf.sprintf "SELECT count(*) FROM %s_handled" inbox_table))));
  Alcotest.(check string)
    "the destination the outbox stamped" "inbox://orders/order-7"
    (session (fun s ->
         Ok (find_string s (Printf.sprintf "SELECT uri FROM %s LIMIT 1" inbox_table))))

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] bridge integration test: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Bridge"
        [
          ( "integration",
            [
              Alcotest.test_case "the outbox feeds the inbox" `Quick
                (test_the_outbox_feeds_the_inbox env uri);
            ] );
        ]
