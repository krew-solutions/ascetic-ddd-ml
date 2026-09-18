(** Tests for the in-memory adapter. Each test builds a bus and a broker of its own, so
    tests do not share state. *)

open Ascetic_bus
module Broker = Ascetic_bus_in_memory.In_memory_broker

let error = Alcotest.testable Bus_error.pp Bus_error.equal
let message = Alcotest.testable Message.pp Message.equal

let ok what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Bus_error.pp e

let setup ~sw =
  ok "register"
    (Bus.register Bus.empty ~scheme:"in-memory" (Broker.adapter (Broker.create ~sw ())))

let with_bus f =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw -> f ~sw (setup ~sw)

let as_text message = Ok (Message.payload message)

let as_number message =
  match int_of_string_opt (Message.payload message) with
  | Some n -> Ok n
  | None -> Error "not a number"

let consumer bus uri group = ok "consumer" (Bus.consumer bus ~uri ~group ~decode:as_text)
let producer bus uri = ok "producer" (Bus.producer bus ~uri ~encode:Message.make)
let publish producer value = ok "publish" (Producer.publish producer value)

(* Subscribes and returns where the handler puts what it receives. *)
let collect consumer =
  let seen = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun value ->
           seen := value :: !seen;
           Ok ()))
  in
  fun () -> List.rev !seen

(* Gives the delivery fibers their turn: handlers are plain functions, so a
   few yields deliver everything that was published. *)
let settle () =
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done

let test_a_message_goes_from_producer_to_consumer () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = collect (consumer bus "in-memory://test.t1" "g") in
  publish (producer bus "in-memory://test.t1") "hello";
  settle ();
  Alcotest.(check (list string)) "delivered" [ "hello" ] (seen ())

let test_publishing_without_a_consumer_is_not_an_error () =
  with_bus @@ fun ~sw:_ bus ->
  publish (producer bus "in-memory://test.t1") "abandoned";
  settle ()

let test_every_group_receives_the_message () =
  with_bus @@ fun ~sw:_ bus ->
  let first = collect (consumer bus "in-memory://test.t1" "g1") in
  let second = collect (consumer bus "in-memory://test.t1" "g2") in
  publish (producer bus "in-memory://test.t1") "x";
  settle ();
  Alcotest.(check (list string)) "g1" [ "x" ] (first ());
  Alcotest.(check (list string)) "g2" [ "x" ] (second ())

let test_one_consumer_per_group () =
  with_bus @@ fun ~sw:_ bus ->
  let _first = consumer bus "in-memory://test.t1" "g" in
  match Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~decode:as_text with
  | Error e ->
      Alcotest.check error "refused"
        (Bus_error.Already_in_group { uri = "in-memory://test.t1"; group = "g" })
        e
  | Ok _ -> Alcotest.fail "a second consumer joined the group"

let test_topics_do_not_leak_into_each_other () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = collect (consumer bus "in-memory://test.uri-A" "g") in
  publish (producer bus "in-memory://test.uri-B") "wrong-topic";
  settle ();
  Alcotest.(check (list string)) "nothing arrives" [] (seen ())

let test_brokers_do_not_leak_into_each_other () =
  with_bus @@ fun ~sw bus_a ->
  let bus_b = setup ~sw in
  let seen = collect (consumer bus_a "in-memory://shared" "g") in
  publish (producer bus_b "in-memory://shared") "from-b";
  settle ();
  Alcotest.(check (list string)) "nothing arrives" [] (seen ())

(* The wire is the contract: two consumers of one topic read the same bytes as
   different types. *)
let test_consumers_of_one_topic_may_decode_differently () =
  with_bus @@ fun ~sw:_ bus ->
  let numbers =
    collect
      (ok "consumer"
         (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"as-int" ~decode:as_number))
  in
  let texts = collect (consumer bus "in-memory://test.t1" "as-str") in
  let producer =
    ok "producer"
      (Bus.producer bus ~uri:"in-memory://test.t1" ~encode:(fun n ->
           Message.make (string_of_int n)))
  in
  publish producer 42;
  settle ();
  Alcotest.(check (list int)) "as a number" [ 42 ] (numbers ());
  Alcotest.(check (list string)) "as text" [ "42" ] (texts ())

let test_cancelling_twice_is_harmless () =
  with_bus @@ fun ~sw:_ bus ->
  let subscription =
    ok "subscribe"
      (Consumer.subscribe (consumer bus "in-memory://test.t1" "g") (fun _ -> Ok ()))
  in
  Subscription.cancel subscription;
  Subscription.cancel subscription

let test_a_cancelled_subscription_receives_nothing_more () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = ref [] in
  let subscription =
    ok "subscribe"
      (Consumer.subscribe (consumer bus "in-memory://test.t1" "g") (fun value ->
           seen := value :: !seen;
           Ok ()))
  in
  let producer = producer bus "in-memory://test.t1" in
  publish producer "before";
  settle ();
  Alcotest.(check (list string)) "before" [ "before" ] (List.rev !seen);
  Subscription.cancel subscription;
  publish producer "after";
  settle ();
  Alcotest.(check (list string)) "nothing after" [ "before" ] (List.rev !seen)

(* A message the consumer cannot decode is skipped, not fatal. *)
let test_an_undecodable_message_is_skipped () =
  with_bus @@ fun ~sw:_ bus ->
  let numbers =
    collect
      (ok "consumer"
         (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~decode:as_number))
  in
  let producer = producer bus "in-memory://test.t1" in
  publish producer "not a number";
  publish producer "7";
  settle ();
  Alcotest.(check (list int)) "the next one arrives" [ 7 ] (numbers ())

(* A handler that raises loses its message, not the topic. *)
let test_a_raising_handler_does_not_stop_delivery () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe (consumer bus "in-memory://test.t1" "g") (fun value ->
           if value = "boom" then failwith "boom";
           seen := value :: !seen;
           Ok ()))
  in
  let producer = producer bus "in-memory://test.t1" in
  publish producer "boom";
  publish producer "after";
  settle ();
  Alcotest.(check (list string)) "the next one arrives" [ "after" ] (List.rev !seen)

(* A handler that fails does not stop delivery of what follows. *)
let test_a_failing_handler_does_not_stop_delivery () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe (consumer bus "in-memory://test.t1" "g") (fun value ->
           if value = "bad" then Error (Failure.transient "refused")
           else begin
             seen := value :: !seen;
             Ok ()
           end))
  in
  let producer = producer bus "in-memory://test.t1" in
  publish producer "bad";
  publish producer "good";
  settle ();
  Alcotest.(check (list string)) "the next one arrives" [ "good" ] (List.rev !seen)

(* Messages of one topic arrive in the order they were published. *)
let test_messages_keep_their_order () =
  with_bus @@ fun ~sw:_ bus ->
  let seen = collect (consumer bus "in-memory://test.t1" "g") in
  let producer = producer bus "in-memory://test.t1" in
  let sent = List.init 100 string_of_int in
  List.iter (publish producer) sent;
  settle ();
  Alcotest.(check (list string)) "in order" sent (seen ())

(* A queue that is full makes the producer wait, and lets it go on once the
   topic has delivered: back-pressure, not loss. *)
let test_a_full_queue_makes_the_producer_wait () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus =
    ok "register"
      (Bus.register Bus.empty ~scheme:"in-memory"
         (Broker.adapter (Broker.create ~sw ~capacity:2 ())))
  in
  let seen = collect (consumer bus "in-memory://test.t1" "g") in
  let producer = producer bus "in-memory://test.t1" in
  let sent = List.init 10 string_of_int in
  List.iter (publish producer) sent;
  settle ();
  Alcotest.(check (list string)) "all of them, in order" sent (seen ())

(* scheme://channel/key: the key does not make a topic of its own, and a
   message published with it carries it. *)
let test_a_key_in_the_uri_selects_the_channel_and_keys_the_message () =
  with_bus @@ fun ~sw:_ bus ->
  let seen =
    collect
      (ok "consumer"
         (Bus.consumer bus ~uri:"in-memory://orders" ~group:"g" ~decode:(fun m -> Ok m)))
  in
  let producer =
    ok "producer" (Bus.producer bus ~uri:"in-memory://orders/order-7" ~encode:Fun.id)
  in
  publish producer (Message.make "x");
  settle ();
  Alcotest.(check (list message))
    "keyed by the URI"
    [ Message.with_key (Message.make "x") "order-7" ]
    (seen ())

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "In_memory_broker"
    [
      ( "delivery",
        [
          case "a message goes from producer to consumer"
            test_a_message_goes_from_producer_to_consumer;
          case "publishing without a consumer is not an error"
            test_publishing_without_a_consumer_is_not_an_error;
          case "every group receives the message" test_every_group_receives_the_message;
          case "consumers of one topic may decode differently"
            test_consumers_of_one_topic_may_decode_differently;
          case "messages keep their order" test_messages_keep_their_order;
          case "a full queue makes the producer wait"
            test_a_full_queue_makes_the_producer_wait;
          case "a key in the uri selects the channel and keys the message"
            test_a_key_in_the_uri_selects_the_channel_and_keys_the_message;
        ] );
      ( "isolation",
        [
          case "one consumer per group" test_one_consumer_per_group;
          case "topics do not leak into each other"
            test_topics_do_not_leak_into_each_other;
          case "brokers do not leak into each other"
            test_brokers_do_not_leak_into_each_other;
        ] );
      ( "failures",
        [
          case "an undecodable message is skipped" test_an_undecodable_message_is_skipped;
          case "a raising handler does not stop delivery"
            test_a_raising_handler_does_not_stop_delivery;
          case "a failing handler does not stop delivery"
            test_a_failing_handler_does_not_stop_delivery;
        ] );
      ( "subscriptions",
        [
          case "cancelling twice is harmless" test_cancelling_twice_is_harmless;
          case "a cancelled subscription receives nothing more"
            test_a_cancelled_subscription_receives_nothing_more;
        ] );
    ]
