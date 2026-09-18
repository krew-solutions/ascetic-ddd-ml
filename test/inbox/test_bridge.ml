(** The inbox as a channel of the bus: a bridge from a broker channel is the intake, and
    the transactional consumer processes each message once, in the transaction that marks
    it. Needs a live database, like [test_inbox.ml].

    With [ASCETIC_DDD_TRACE_DIR] set, every test writes what its inbox, and, where there
    is one, its outbox, reported, one event per line, for validation against the protocol
    models: see [verify/tla/README.md]. *)

module Bus = Ascetic_bus.Bus
module Bridge = Ascetic_bus.Bridge
module Message = Ascetic_bus.Message
module Producer = Ascetic_bus.Producer
module Subscription = Ascetic_bus.Subscription
module Transactional = Ascetic_bus.Transactional
module Broker = Ascetic_bus_in_memory.In_memory_broker
module Inbox = Ascetic_inbox.Pg_inbox
module Channel = Ascetic_inbox.Inbox_channel
module Loops = Ascetic_inbox.Loops
module Error = Ascetic_inbox.Inbox_error
module Outbox = Ascetic_outbox.Pg_outbox
module Outbox_channel = Ascetic_outbox.Outbox_channel
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Trace_file = Ascetic_trace.Trace_file
module Json_trace = Ascetic_trace.Json_trace

let bus what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Ascetic_bus.Bus_error.pp e

let lift e = Error.Session e

let unwrap what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %s" what (Error.to_string e)

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

type fixture = {
  sw : Eio.Switch.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  sessions : Pool.t;
  inbox : Inbox.t;
  table : string;
  (* One recorder for the inbox and, where a test has one, the outbox, so that
     the trace keeps the order across both. *)
  trace : Trace_file.t;
}

let loops = { Loops.default with poll_interval = 0.02 }

(* A table of its own per test, plus one the handler writes into. [stem] names
   the trace file: [inbox-...] for a run the inbox model checks alone,
   [bridge-...] for one with an outbox in front. *)
let with_fixture ~name ~stem env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions =
    match
      Caqti_eio_unix.connect_pool
        ~pool_config:(Caqti_pool_config.create ~max_size:8 ())
        ~sw ~stdenv uri
    with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let table = "inbox_bridge_" ^ name in
  let trace = Trace_file.from_env stem in
  let inbox =
    Inbox.create
      ~observer:(Json_trace.inbox_observer (Trace_file.recorder trace))
      ~table:(Identifier.of_string_exn table)
      ~sequence:(Identifier.of_string_exn (table ^ "_seq"))
      sessions
  in
  unwrap "setup"
    (Pool.session sessions ~lift (fun session ->
         exec_exn session
           (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_meta, %s_slots, %s_handled" table
              table table table);
         exec_exn session (Printf.sprintf "DROP SEQUENCE IF EXISTS %s_seq" table);
         exec_exn session
           (Printf.sprintf "CREATE TABLE %s_handled (payload text NOT NULL)" table);
         Inbox.setup inbox session));
  Fun.protect
    ~finally:(fun () -> Trace_file.close trace)
    (fun () ->
      body
        {
          sw;
          clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
          sessions;
          inbox;
          table;
          trace;
        })

let count f sql =
  unwrap "count"
    (Pool.session f.sessions ~lift (fun session ->
         let module C = (val Session.connection session) in
         let open Caqti_request.Infix in
         match C.find ((Caqti_type.unit ->! Caqti_type.int) ~oneshot:true sql) () with
         | Ok n -> Ok n
         | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err))

let processed f =
  count f
    (Printf.sprintf "SELECT count(*) FROM %s WHERE processed_position IS NOT NULL" f.table)

let handled f = count f (Printf.sprintf "SELECT count(*) FROM %s_handled" f.table)

let stored_uri f =
  unwrap "uri"
    (Pool.session f.sessions ~lift (fun session ->
         let module C = (val Session.connection session) in
         let open Caqti_request.Infix in
         match
           C.find
             ((Caqti_type.unit ->! Caqti_type.string)
                ~oneshot:true
                (Printf.sprintf "SELECT uri FROM %s LIMIT 1" f.table))
             ()
         with
         | Ok uri -> Ok uri
         | Error err -> Alcotest.failf "uri: %a" Caqti_error.pp err))

(* Waits for the condition, polling; whether it came true in time. *)
let soon f ~seconds condition =
  let rec wait tries =
    condition ()
    || tries > 0
       && begin
         Eio.Time.Mono.sleep f.clock 0.02;
         wait (tries - 1)
       end
  in
  wait (int_of_float (seconds /. 0.02))

(* The handler reports before the mark commits, so the count is waited for
   rather than read at once. *)
let processed_soon f expected =
  ignore (soon f ~seconds:5.0 (fun () -> processed f = expected));
  processed f

let payload_of message = Ok (Message.payload message)

(* Processes with a handler that writes through the transaction it is given and
   reports the payload. *)
let consume f =
  let received = ref [] in
  let orders =
    Channel.consumer ~sw:f.sw ~clock:f.clock ~loops f.inbox ~decode:payload_of
  in
  let processing =
    bus "subscribe"
      (Transactional.Consumer.subscribe orders (fun tx order ->
           match
             exec tx
               (Printf.sprintf "INSERT INTO %s_handled (payload) VALUES ('%s')" f.table
                  order)
           with
           | Error reason -> Error (Ascetic_bus.Failure.transient reason)
           | Ok () ->
               received := order :: !received;
               Ok ()))
  in
  ((fun () -> List.rev !received), processing)

(* A wire message of order 7 with the headers the inbox needs. *)
let order payload position message_id =
  let header name value message = Message.with_header message name value in
  Message.with_key (Message.make payload) "order-7"
  |> header "tenant_id" "t1"
  |> header "stream_type" "orders.Order"
  |> header "stream_id" "7"
  |> header "stream_position" (string_of_int position)
  |> header "message_id" message_id

(* A bus with an in-memory broker and the inbox, and the intake: a bridge from
   the broker's channel to the inbox channel. *)
let with_intake f =
  let registry =
    bus "register in-memory"
      (Bus.register Bus.empty ~scheme:"in-memory"
         (Broker.adapter (Broker.create ~sw:f.sw ())))
  in
  let registry =
    bus "register inbox"
      (Bus.register registry ~scheme:Channel.scheme (Channel.adapter f.inbox))
  in
  let intake =
    bus "bridge"
      (Bridge.run (Bridge.create registry) ~from:"in-memory://orders" ~group:"intake"
         (Bridge.Fixed "inbox://orders"))
  in
  let producer =
    bus "producer" (Bus.producer registry ~uri:"in-memory://orders" ~encode:Fun.id)
  in
  (intake, fun message -> bus "publish" (Producer.publish producer message))

let test_a_message_from_a_broker_is_processed_once_in_the_marking_transaction env uri () =
  with_fixture ~name:"broker" ~stem:"inbox-bridge-broker" env uri @@ fun f ->
  let intake, publish = with_intake f in
  let received, processing = consume f in
  let placed = order "placed" 1 "00000000-0000-4000-8000-000000000001" in
  publish placed;
  publish placed (* delivered twice: the same identity *);
  Alcotest.(check bool)
    "the message is processed" true
    (soon f ~seconds:20.0 (fun () -> received () <> []));
  Eio.Time.Mono.sleep f.clock 0.3;
  Alcotest.(check (list string))
    "the duplicate is not processed again" [ "placed" ] (received ());
  Alcotest.(check int) "marked" 1 (processed_soon f 1);
  Alcotest.(check int) "the handler's write committed with the mark" 1 (handled f);
  Alcotest.(check string)
    "the channel it was published to, key included" "inbox://orders/order-7"
    (stored_uri f);
  Subscription.cancel intake;
  Subscription.cancel processing

let test_the_outbox_feeds_the_inbox_without_a_broker env uri () =
  with_fixture ~name:"outbox" ~stem:"bridge-outbox-to-inbox" env uri @@ fun f ->
  let outbox : Ascetic_bus.Failure.t Outbox.t =
    Outbox.create
      ~observer:(Json_trace.outbox_observer (Trace_file.recorder f.trace))
      ~outbox_table:(Identifier.of_string_exn "inbox_bridge_outbox_out")
      ~offsets_table:(Identifier.of_string_exn "inbox_bridge_outbox_out_offsets")
      f.sessions
  in
  (match
     Pool.session f.sessions
       ~lift:(fun e -> Ascetic_outbox.Outbox_error.Session e)
       (fun session ->
         exec_exn session
           "DROP TABLE IF EXISTS inbox_bridge_outbox_out, inbox_bridge_outbox_out_meta, \
            inbox_bridge_outbox_out_offsets";
         Outbox.setup outbox session)
   with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "outbox setup: %s"
        (Ascetic_outbox.Outbox_error.to_string Ascetic_bus.Failure.to_string e));
  let registry =
    bus "register outbox"
      (Bus.register Bus.empty ~scheme:Outbox_channel.scheme
         (Outbox_channel.adapter ~sw:f.sw ~clock:f.clock
            ~loops:{ Ascetic_outbox.Loops.default with poll_interval = 0.02 }
            outbox))
  in
  let registry =
    bus "register inbox"
      (Bus.register registry ~scheme:Channel.scheme (Channel.adapter f.inbox))
  in
  let dispatcher =
    bus "bridge"
      (Bridge.run (Bridge.create registry) ~from:"outbox://all" ~group:"dispatcher"
         (Bridge.Header "destination"))
  in
  let received, processing = consume f in
  let shipped =
    Outbox_channel.producer outbox ~destination:"inbox://orders/order-7"
      ~encode:(fun payload -> order payload 2 "00000000-0000-4000-8000-000000000002")
  in
  unwrap "commit"
    (Pool.session f.sessions ~lift (fun session ->
         Session.atomic session ~lift (fun tx ->
             bus "publish" (Transactional.Producer.publish shipped tx "shipped");
             Ok ())));
  Alcotest.(check bool)
    "the message is processed" true
    (soon f ~seconds:20.0 (fun () -> received () <> []));
  Alcotest.(check (list string)) "once" [ "shipped" ] (received ());
  Alcotest.(check int) "marked" 1 (processed_soon f 1);
  Alcotest.(check int) "the handler's write committed with the mark" 1 (handled f);
  Alcotest.(check string)
    "the destination the outbox stamped" "inbox://orders/order-7" (stored_uri f);
  Subscription.cancel dispatcher;
  Subscription.cancel processing

(* A handler that fails leaves the message unprocessed and its own writes
   rolled back; the loop retries, and the second attempt goes through. *)
let test_a_failing_handler_is_retried_and_its_writes_are_rolled_back env uri () =
  with_fixture ~name:"retry" ~stem:"inbox-bridge-retry" env uri @@ fun f ->
  let intake, publish = with_intake f in
  let attempts = ref 0 and received = ref [] in
  let orders =
    Channel.consumer ~sw:f.sw ~clock:f.clock ~loops f.inbox ~decode:payload_of
  in
  let processing =
    bus "subscribe"
      (Transactional.Consumer.subscribe orders (fun tx order ->
           match
             exec tx
               (Printf.sprintf "INSERT INTO %s_handled (payload) VALUES ('%s')" f.table
                  order)
           with
           | Error reason -> Error (Ascetic_bus.Failure.transient reason)
           | Ok () ->
               incr attempts;
               if !attempts = 1 then
                 Error
                   (Ascetic_bus.Failure.transient "the first attempt fails on purpose")
               else begin
                 received := order :: !received;
                 Ok ()
               end))
  in
  publish (order "retried" 3 "00000000-0000-4000-8000-000000000003");
  Alcotest.(check bool)
    "the message is processed" true
    (soon f ~seconds:20.0 (fun () -> !received <> []));
  Alcotest.(check (list string)) "processed" [ "retried" ] !received;
  Alcotest.(check int) "marked" 1 (processed_soon f 1);
  Alcotest.(check int) "two attempts" 2 !attempts;
  Alcotest.(check int) "the failed attempt's write was rolled back with it" 1 (handled f);
  Subscription.cancel intake;
  Subscription.cancel processing

(* A handler that says its failure is permanent, the one verdict the bus
   carries, has the message parked at once, with no second attempt. *)
let test_a_handler_s_permanent_verdict_parks_the_message_at_once env uri () =
  with_fixture ~name:"permanent" ~stem:"inbox-bridge-permanent" env uri @@ fun f ->
  let intake, publish = with_intake f in
  let attempts = ref 0 in
  let orders =
    Channel.consumer ~sw:f.sw ~clock:f.clock ~loops f.inbox ~decode:payload_of
  in
  let processing =
    bus "subscribe"
      (Transactional.Consumer.subscribe orders (fun _tx _order ->
           incr attempts;
           Error (Ascetic_bus.Failure.permanent "this message will never open")))
  in
  publish (order "doomed" 4 "00000000-0000-4000-8000-000000000004");
  let parked () =
    count f (Printf.sprintf "SELECT count(*) FROM %s WHERE parked_at IS NOT NULL" f.table)
  in
  ignore (soon f ~seconds:5.0 (fun () -> parked () = 1));
  Alcotest.(check int) "parked" 1 (parked ());
  Alcotest.(check int) "not processed" 0 (processed f);
  Eio.Time.Mono.sleep f.clock 0.1;
  Alcotest.(check int) "no second attempt" 1 !attempts;
  Subscription.cancel intake;
  Subscription.cancel processing

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] inbox channel tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      let case name test = Alcotest.test_case name `Quick (test env uri) in
      Alcotest.run "Inbox_channel"
        [
          ( "integration",
            [
              case "a message from a broker is processed once in the marking transaction"
                test_a_message_from_a_broker_is_processed_once_in_the_marking_transaction;
              case "the outbox feeds the inbox without a broker"
                test_the_outbox_feeds_the_inbox_without_a_broker;
              case "a failing handler is retried and its writes are rolled back"
                test_a_failing_handler_is_retried_and_its_writes_are_rolled_back;
              case "a handler's permanent verdict parks the message at once"
                test_a_handler_s_permanent_verdict_parks_the_message_at_once;
            ] );
        ]
