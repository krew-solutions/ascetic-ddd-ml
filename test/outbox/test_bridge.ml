(** The outbox as a channel of the bus: what is published inside a committed transaction
    reaches its destination through a bridge; what is rolled back never leaves the outbox.
    Needs a live database, like [test_outbox.ml]. *)

module Bus = Ascetic_bus.Bus
module Bridge = Ascetic_bus.Bridge
module Message = Ascetic_bus.Message
module Consumer = Ascetic_bus.Consumer
module Subscription = Ascetic_bus.Subscription
module Transactional = Ascetic_bus.Transactional
module Broker = Ascetic_bus_in_memory.In_memory_broker
module Outbox = Ascetic_outbox.Pg_outbox
module Channel = Ascetic_outbox.Outbox_channel
module Loops = Ascetic_outbox.Loops
module Error = Ascetic_outbox.Outbox_error
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

let bus what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Ascetic_bus.Bus_error.pp e

let lift e = Error.Session e

let unwrap what = function
  | Ok value -> value
  | Error e ->
      Alcotest.failf "%s: %s" what (Error.to_string Ascetic_bus.Failure.to_string e)

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

type fixture = {
  sw : Eio.Switch.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  sessions : Pool.t;
  outbox : Ascetic_bus.Failure.t Outbox.t;
}

let loops = { Loops.default with poll_interval = 0.02 }

let with_fixture ~name env uri body =
  (* The pool is on a switch of its own, outside the one the dispatcher runs
     on: a loop cancelled with a connection in hand gives it back to a pool
     that is still there, where a pool ending with it would wait for the
     connection for ever. *)
  Eio.Switch.run @@ fun pool_sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions =
    match
      Caqti_eio_unix.connect_pool
        ~pool_config:(Caqti_pool_config.create ~max_size:8 ())
        ~sw:pool_sw ~stdenv uri
    with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let table = "outbox_" ^ name in
  let outbox =
    Outbox.create
      ~outbox_table:(Identifier.of_string_exn table)
      ~offsets_table:(Identifier.of_string_exn (table ^ "_offsets"))
      sessions
  in
  unwrap "setup"
    (Pool.session sessions ~lift (fun session ->
         exec session
           (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_meta, %s_offsets" table table
              table);
         Outbox.setup outbox session));
  Eio.Switch.run @@ fun sw ->
  body
    {
      sw;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
      sessions;
      outbox;
    }

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

let test_a_committed_message_crosses_the_bridge_and_a_rolled_back_one_does_not env uri ()
    =
  with_fixture ~name:"bridge" env uri @@ fun f ->
  let registry =
    bus "register outbox"
      (Bus.register Bus.empty ~scheme:Channel.scheme
         (Channel.adapter ~sw:f.sw ~clock:f.clock ~loops f.outbox))
  in
  let registry =
    bus "register in-memory"
      (Bus.register registry ~scheme:"in-memory"
         (Broker.adapter (Broker.create ~sw:f.sw ())))
  in
  let seen = ref [] in
  let _ : Subscription.t =
    bus "subscribe"
      (Consumer.subscribe
         (bus "consumer"
            (Bus.consumer registry ~uri:"in-memory://orders" ~group:"billing"
               ~decode:(fun m -> Ok m)))
         (fun message ->
           seen := message :: !seen;
           Ok ()))
  in
  let dispatcher =
    bus "bridge"
      (Bridge.run (Bridge.create registry) ~from:"outbox://all" ~group:"dispatcher"
         (Bridge.Header "destination"))
  in
  let producer =
    Channel.producer f.outbox ~destination:"in-memory://orders/order-7"
      ~encode:(fun order ->
        Message.with_header (Message.make order) "message_id"
          "00000000-0000-4000-8000-000000000001")
  in
  (* Rolled back: never leaves the outbox. *)
  let rolled_back =
    Pool.session f.sessions ~lift (fun session ->
        Session.atomic session ~lift (fun tx ->
            bus "publish" (Transactional.Producer.publish producer tx "lost");
            Error (Error.Malformed "rolled back on purpose")))
  in
  Alcotest.(check bool) "rolled back" true (Result.is_error rolled_back);
  (* Committed: crosses the bridge. *)
  unwrap "commit"
    (Pool.session f.sessions ~lift (fun session ->
         Session.atomic session ~lift (fun tx ->
             bus "publish" (Transactional.Producer.publish producer tx "placed");
             Ok ())));
  Alcotest.(check bool)
    "the committed message reaches the in-memory channel" true
    (soon f ~seconds:20.0 (fun () -> !seen <> []));
  let message = List.hd (List.rev !seen) in
  Alcotest.(check string) "payload" "placed" (Message.payload message);
  Alcotest.(check (option string)) "key" (Some "order-7") (Message.key message);
  Alcotest.(check (option string))
    "destination" (Some "in-memory://orders/order-7")
    (Message.header message "destination");
  Alcotest.(check (option string))
    "message id" (Some "00000000-0000-4000-8000-000000000001")
    (Message.header message "message_id");
  Eio.Time.Mono.sleep f.clock 0.2;
  Alcotest.(check int) "the rolled-back message never arrives" 1 (List.length !seen);
  Subscription.cancel dispatcher

(* A subscriber of the outbox channel that fails leaves the batch
   unacknowledged; it is delivered again after the poll interval. *)
let test_a_failing_subscriber_gets_the_batch_again env uri () =
  with_fixture ~name:"retry" env uri @@ fun f ->
  let registry =
    bus "register outbox"
      (Bus.register Bus.empty ~scheme:Channel.scheme
         (Channel.adapter ~sw:f.sw ~clock:f.clock ~loops f.outbox))
  in
  let attempts = ref 0 and received = ref [] in
  let flaky =
    bus "consumer"
      (Bus.consumer registry ~uri:"outbox://all" ~group:"flaky" ~decode:(fun m ->
           Ok (Message.payload m)))
  in
  let subscription =
    bus "subscribe"
      (Consumer.subscribe flaky (fun order ->
           incr attempts;
           if !attempts = 1 then
             Error (Ascetic_bus.Failure.transient "the first attempt fails on purpose")
           else begin
             received := order :: !received;
             Ok ()
           end))
  in
  let placed =
    Channel.producer f.outbox ~destination:"in-memory://orders/order-9"
      ~encode:(fun order ->
        Message.with_header (Message.make order) "message_id"
          "00000000-0000-4000-8000-000000000009")
  in
  unwrap "commit"
    (Pool.session f.sessions ~lift (fun session ->
         Session.atomic session ~lift (fun tx ->
             bus "publish" (Transactional.Producer.publish placed tx "placed");
             Ok ())));
  Alcotest.(check bool)
    "the batch is delivered again" true
    (soon f ~seconds:20.0 (fun () -> !received <> []));
  Alcotest.(check (list string)) "delivered" [ "placed" ] !received;
  Alcotest.(check int) "two attempts" 2 !attempts;
  Subscription.cancel subscription

(* Cancelling the dispatcher's subscription waits for the batch it has in
   hand: when it returns, the handler has returned and the batch is
   acknowledged and committed, read here at once, with no polling. *)
let test_cancelling_waits_for_the_batch_in_hand_to_be_committed env uri () =
  with_fixture ~name:"graceful" env uri @@ fun f ->
  let registry =
    bus "register outbox"
      (Bus.register Bus.empty ~scheme:Channel.scheme
         (Channel.adapter ~sw:f.sw ~clock:f.clock ~loops f.outbox))
  in
  let handling, set_handling = Eio.Promise.create () in
  let may_return, let_return = Eio.Promise.create () in
  let returned = ref false in
  let slow =
    bus "consumer"
      (Bus.consumer registry ~uri:"outbox://all" ~group:"slow" ~decode:(fun m ->
           Ok (Message.payload m)))
  in
  let subscription =
    bus "subscribe"
      (Consumer.subscribe slow (fun _order ->
           Eio.Promise.resolve set_handling ();
           Eio.Promise.await may_return;
           returned := true;
           Ok ()))
  in
  let placed =
    Channel.producer f.outbox ~destination:"in-memory://orders/order-3"
      ~encode:(fun order ->
        Message.with_header (Message.make order) "message_id"
          "00000000-0000-4000-8000-000000000003")
  in
  unwrap "commit"
    (Pool.session f.sessions ~lift (fun session ->
         Session.atomic session ~lift (fun tx ->
             bus "publish" (Transactional.Producer.publish placed tx "placed");
             Ok ())));
  let in_time what wait =
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds f.clock 20.0) (fun () ->
        try wait () with Eio.Time.Timeout -> Alcotest.failf "%s: not in time" what)
  in
  in_time "the handler is called" (fun () -> Eio.Promise.await handling);
  let cancelled =
    Eio.Fiber.fork_promise ~sw:f.sw (fun () -> Subscription.cancel subscription)
  in
  Eio.Time.Mono.sleep f.clock 0.2;
  Alcotest.(check bool)
    "cancel waits while the handler runs" false
    (Eio.Promise.is_resolved cancelled);
  Eio.Promise.resolve let_return ();
  in_time "cancel returns" (fun () -> Eio.Promise.await_exn cancelled);
  Alcotest.(check bool) "the handler has returned" true !returned;
  let positions =
    unwrap "positions"
      (Pool.session f.sessions ~lift (fun session ->
           Outbox.positions f.outbox session (Ascetic_outbox.Selection.group "slow")))
  in
  Alcotest.(check bool)
    "the batch is acknowledged and committed" true
    (positions <> []
    && List.for_all
         (fun position ->
           not (Ascetic_outbox.Position.equal position Ascetic_outbox.Position.zero))
         positions)

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] outbox channel tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      let case name test = Alcotest.test_case name `Quick (test env uri) in
      Alcotest.run "Outbox_channel"
        [
          ( "integration",
            [
              case "a committed message crosses the bridge and a rolled back one does not"
                test_a_committed_message_crosses_the_bridge_and_a_rolled_back_one_does_not;
              case "a failing subscriber gets the batch again"
                test_a_failing_subscriber_gets_the_batch_again;
              case "cancelling waits for the batch in hand to be committed"
                test_cancelling_waits_for_the_batch_in_hand_to_be_committed;
            ] );
        ]
