(** Tests for the bridge, over two in-memory brokers: what arrives on [a://] is published
    on [b://]. *)

open Ascetic_bus
module Broker = Ascetic_bus_in_memory.In_memory_broker

let ok what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Bus_error.pp e

let with_two_brokers f =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus =
    ok "a" (Bus.register Bus.empty ~scheme:"a" (Broker.adapter (Broker.create ~sw ())))
  in
  let bus =
    ok "b" (Bus.register bus ~scheme:"b" (Broker.adapter (Broker.create ~sw ())))
  in
  f bus

(* A consumer on the URI that keeps every wire message. *)
let collect bus uri =
  let consumer =
    ok "consumer" (Bus.consumer bus ~uri ~group:"collector" ~decode:(fun m -> Ok m))
  in
  let seen = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun message ->
           seen := message :: !seen;
           Ok ()))
  in
  fun () -> List.rev !seen

let wire bus uri = ok "producer" (Bus.producer bus ~uri ~encode:Fun.id)
let publish producer message = ok "publish" (Producer.publish producer message)

let settle () =
  for _ = 1 to 20 do
    Eio.Fiber.yield ()
  done

(* The destination is read from a header, so one channel feeds many; key,
   payload and headers arrive untouched. *)
let test_a_bridge_forwards_to_the_destination_a_header_names () =
  with_two_brokers @@ fun bus ->
  let orders = collect bus "b://orders" in
  let payments = collect bus "b://payments" in
  let _run =
    ok "run"
      (Bridge.run (Bridge.create bus) ~from:"a://outbox" ~group:"dispatcher"
         (Bridge.Header "destination"))
  in
  let producer = wire bus "a://outbox" in
  publish producer
    (Message.with_header (Message.make "order placed") "destination" "b://orders/order-7");
  publish producer
    (Message.with_header (Message.make "paid") "destination" "b://payments");
  settle ();
  (match orders () with
  | [ order ] ->
      Alcotest.(check string) "payload" "order placed" (Message.payload order);
      Alcotest.(check (option string))
        "the key comes from the destination URI" (Some "order-7") (Message.key order);
      Alcotest.(check (option string))
        "the header travels" (Some "b://orders/order-7")
        (Message.header order "destination")
  | others -> Alcotest.failf "orders got %d messages" (List.length others));
  Alcotest.(check (list string))
    "payments" [ "paid" ]
    (List.map Message.payload (payments ()))

let test_a_bridge_forwards_to_a_fixed_target () =
  with_two_brokers @@ fun bus ->
  let mirror = collect bus "b://mirror" in
  let _run =
    ok "run"
      (Bridge.run (Bridge.create bus) ~from:"a://events" ~group:"mirror"
         (Bridge.Fixed "b://mirror"))
  in
  publish (wire bus "a://events") (Message.make "x");
  settle ();
  Alcotest.(check (list string)) "mirrored" [ "x" ] (List.map Message.payload (mirror ()))

(* A message without a destination cannot be forwarded: the handler fails, and
   nothing is published anywhere. *)
let test_a_message_without_a_destination_is_not_forwarded () =
  with_two_brokers @@ fun bus ->
  let orders = collect bus "b://orders" in
  let _run =
    ok "run"
      (Bridge.run (Bridge.create bus) ~from:"a://outbox" ~group:"dispatcher"
         (Bridge.Header "destination"))
  in
  publish (wire bus "a://outbox") (Message.make "lost");
  settle ();
  Alcotest.(check int) "nothing arrives" 0 (List.length (orders ()))

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Bridge"
    [
      ( "forwarding",
        [
          case "a bridge forwards to the destination a header names"
            test_a_bridge_forwards_to_the_destination_a_header_names;
          case "a bridge forwards to a fixed target"
            test_a_bridge_forwards_to_a_fixed_target;
          case "a message without a destination is not forwarded"
            test_a_message_without_a_destination_is_not_forwarded;
        ] );
    ]
