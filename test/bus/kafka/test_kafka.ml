(** Integration tests for the Kafka adapter. They need a live broker, and are skipped when
    [TEST_KAFKA_BROKERS] is not set:

    {v
      docker compose up -d redpanda
      export TEST_KAFKA_BROKERS=localhost:59092
      dune test test/bus/kafka
    v} *)

open Ascetic_bus
module Broker = Ascetic_bus_kafka.Kafka_broker

let ok what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Bus_error.pp e

(* Each test gets topics and groups of its own, so runs do not see each other's
   messages. *)
let counter = ref 0

let unique name =
  incr counter;
  Printf.sprintf "%s-%d-%d" name (int_of_float (Unix.gettimeofday () *. 1000.0)) !counter

type fixture = { bus : Bus.t; clock : float Eio.Time.clock_ty Eio.Resource.t }

let with_bus env brokers f =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let broker =
    Broker.create ~sw ~clock ~offset_reset:Kafka.Consumer.Earliest ~retry_after:0.1
      ~properties:[ ("allow.auto.create.topics", "true") ]
      ~brokers:(String.split_on_char ',' brokers)
      ()
  in
  let bus =
    ok "register" (Bus.register Bus.empty ~scheme:"kafka" (Broker.adapter broker))
  in
  f { bus; clock :> float Eio.Time.clock_ty Eio.Resource.t }

(* A value is a key and a text. *)
let keyed (key, text) = Message.with_key (Message.make text) key
let producer f uri = ok "producer" (Bus.producer f.bus ~uri ~encode:keyed)
let publish producer value = ok "publish" (Producer.publish producer value)

let collect f uri group =
  let seen = ref [] in
  let consumer =
    ok "consumer"
      (Bus.consumer f.bus ~uri ~group ~decode:(fun m -> Ok (Message.payload m)))
  in
  let subscription =
    ok "subscribe"
      (Consumer.subscribe consumer (fun value ->
           seen := value :: !seen;
           Ok ()))
  in
  ((fun () -> List.rev !seen), subscription)

(* Waits for the condition, polling; whether it came true in time. *)
let soon f ~seconds condition =
  let rec wait tries =
    condition ()
    || tries > 0
       && begin
         Eio.Time.sleep f.clock 0.1;
         wait (tries - 1)
       end
  in
  wait (int_of_float (seconds /. 0.1))

let test_a_message_goes_from_producer_to_consumer env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "roundtrip" in
  let seen, _ = collect f uri (unique "g") in
  publish (producer f uri) ("order-1", "hello");
  Alcotest.(check bool) "arrives" true (soon f ~seconds:30.0 (fun () -> seen () <> []));
  Alcotest.(check (list string)) "delivered" [ "hello" ] (seen ())

let test_key_and_headers_cross_the_wire env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "wire" in
  let seen = ref [] in
  let consumer =
    ok "consumer" (Bus.consumer f.bus ~uri ~group:(unique "g") ~decode:(fun m -> Ok m))
  in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun message ->
           seen := message :: !seen;
           Ok ()))
  in
  (* the producer's URI carries the key; the message has none of its own *)
  let wire = ok "producer" (Bus.producer f.bus ~uri:(uri ^ "/order-7") ~encode:Fun.id) in
  ok "publish"
    (Producer.publish wire
       (Message.with_header (Message.make "placed") "destination" "inbox://orders"));
  Alcotest.(check bool) "arrives" true (soon f ~seconds:30.0 (fun () -> !seen <> []));
  let message = List.hd !seen in
  Alcotest.(check string) "payload" "placed" (Message.payload message);
  Alcotest.(check (option string))
    "keyed by the URI" (Some "order-7") (Message.key message);
  Alcotest.(check (option string))
    "header" (Some "inbox://orders")
    (Message.header message "destination")

let test_every_group_receives_the_message env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "fanout" in
  let first, _ = collect f uri (unique "g1") in
  let second, _ = collect f uri (unique "g2") in
  publish (producer f uri) ("order-1", "x");
  Alcotest.(check bool)
    "both" true
    (soon f ~seconds:30.0 (fun () -> first () <> [] && second () <> []));
  Alcotest.(check (list string)) "g1" [ "x" ] (first ());
  Alcotest.(check (list string)) "g2" [ "x" ] (second ())

(* Messages with one key arrive in the order they were published. *)
let test_messages_with_one_key_keep_their_order env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "order" in
  let seen, _ = collect f uri (unique "g") in
  let producer = producer f uri in
  let sent = List.init 20 string_of_int in
  List.iter (fun text -> publish producer ("order-1", text)) sent;
  Alcotest.(check bool)
    "all arrive" true
    (soon f ~seconds:30.0 (fun () -> List.length (seen ()) = 20));
  Alcotest.(check (list string)) "in order" sent (seen ())

let test_a_cancelled_subscription_receives_nothing_more env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "cancel" in
  let seen, subscription = collect f uri (unique "g") in
  let producer = producer f uri in
  publish producer ("k", "before");
  Alcotest.(check bool)
    "before" true
    (soon f ~seconds:30.0 (fun () -> seen () = [ "before" ]));
  Subscription.cancel subscription;
  publish producer ("k", "after");
  Eio.Time.sleep f.clock 3.0;
  Alcotest.(check (list string)) "nothing after" [ "before" ] (seen ())

(* A handler that fails is given the message again, and what follows waits. *)
let test_a_failing_handler_gets_the_message_again env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "retry" in
  let attempts = ref 0 and seen = ref [] in
  let consumer =
    ok "consumer"
      (Bus.consumer f.bus ~uri ~group:(unique "g") ~decode:(fun m ->
           Ok (Message.payload m)))
  in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun value ->
           incr attempts;
           if !attempts = 1 then
             Error (Failure.transient "the first attempt fails on purpose")
           else begin
             seen := value :: !seen;
             Ok ()
           end))
  in
  let producer = producer f uri in
  publish producer ("k", "first");
  publish producer ("k", "second");
  Alcotest.(check bool)
    "both arrive" true
    (soon f ~seconds:30.0 (fun () -> List.length !seen = 2));
  Alcotest.(check (list string))
    "in order, the first retried" [ "first"; "second" ] (List.rev !seen);
  Alcotest.(check int) "three calls" 3 !attempts

(* A failure no retry will mend loses its message, not the partition. *)
let test_a_permanent_failure_is_skipped env brokers () =
  with_bus env brokers @@ fun f ->
  let uri = "kafka://" ^ unique "poison" in
  let seen = ref [] in
  let consumer =
    ok "consumer"
      (Bus.consumer f.bus ~uri ~group:(unique "g") ~decode:(fun m ->
           Ok (Message.payload m)))
  in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun value ->
           if value = "poison" then Error (Failure.permanent "it will never open")
           else begin
             seen := value :: !seen;
             Ok ()
           end))
  in
  let producer = producer f uri in
  publish producer ("k", "poison");
  publish producer ("k", "after");
  Alcotest.(check bool)
    "the next one arrives" true
    (soon f ~seconds:30.0 (fun () -> !seen <> []));
  Alcotest.(check (list string)) "only the next one" [ "after" ] !seen

let () =
  match Sys.getenv_opt "TEST_KAFKA_BROKERS" with
  | None ->
      print_endline "[skip] Kafka adapter tests: TEST_KAFKA_BROKERS is not set";
      exit 0
  | Some brokers ->
      Eio_main.run @@ fun env ->
      let case name test = Alcotest.test_case name `Quick (test env brokers) in
      Alcotest.run "Kafka_broker"
        [
          ( "integration",
            [
              case "a message goes from producer to consumer"
                test_a_message_goes_from_producer_to_consumer;
              case "key and headers cross the wire" test_key_and_headers_cross_the_wire;
              case "every group receives the message"
                test_every_group_receives_the_message;
              case "messages with one key keep their order"
                test_messages_with_one_key_keep_their_order;
              case "a cancelled subscription receives nothing more"
                test_a_cancelled_subscription_receives_nothing_more;
              case "a failing handler gets the message again"
                test_a_failing_handler_gets_the_message_again;
              case "a permanent failure is skipped" test_a_permanent_failure_is_skipped;
            ] );
        ]
