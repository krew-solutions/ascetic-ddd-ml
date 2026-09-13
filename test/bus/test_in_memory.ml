(** Tests for the [In_memory] adapter. Each test creates its own [Bus.bus] +
    [In_memory.broker] inside an [Eio.Switch.run] for full isolation; tests can run in
    parallel without state leakage. *)

module Bus = Ascetic_bus.Bus
module In_memory = Ascetic_bus_in_memory.In_memory

let setup ~sw =
  let bus = Bus.create () in
  let broker = In_memory.create ~sw in
  Bus.register bus ~scheme:"in-memory" (In_memory.adapter broker);
  (bus, broker)

(* Drain the dispatch fiber: yield until scheduled subscribers run. *)
let drain () = Eio.Fiber.yield ()

let test_producer_consumer_roundtrip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let received = ref [] in
  let consumer =
    Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id
  in
  let _ : Bus.subscription =
    Bus.subscribe consumer (fun s -> received := s :: !received)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "hello";
  drain ();
  Alcotest.(check (list string)) "delivered" [ "hello" ] !received

let test_publish_preserves_order () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id)
      (fun s -> received := s :: !received)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "1";
  Bus.publish producer "2";
  Bus.publish producer "3";
  drain ();
  Alcotest.(check (list string)) "FIFO per topic" [ "1"; "2"; "3" ] (List.rev !received)

let test_publish_without_consumer_no_error () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "abandoned";
  drain ();
  Alcotest.(check pass) "no exception" () ()

let test_cross_group_fan_out () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let g1 = ref [] in
  let g2 = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g1" ~deserialize:Fun.id)
      (fun s -> g1 := s :: !g1)
  in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g2" ~deserialize:Fun.id)
      (fun s -> g2 := s :: !g2)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "x";
  drain ();
  Alcotest.(check (list string)) "g1 received" [ "x" ] !g1;
  Alcotest.(check (list string)) "g2 received" [ "x" ] !g2

let test_single_consumer_per_group_invariant () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let _ = Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id in
  Alcotest.check_raises "second consumer in same (uri, group) raises"
    (In_memory.Already_registered_in_group { uri = "in-memory://test.t1"; group = "g" })
    (fun () ->
      let _ =
        Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id
      in
      ())

let test_cross_uri_isolation () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.uri-A" ~group:"g" ~deserialize:Fun.id)
      (fun s -> received := s :: !received)
  in
  let producer_b = Bus.producer bus ~uri:"in-memory://test.uri-B" ~serialize:Fun.id in
  Bus.publish producer_b "wrong-topic";
  drain ();
  Alcotest.(check (list string)) "uri-A consumer never sees uri-B traffic" [] !received

let test_cross_broker_isolation () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus_a = Bus.create () in
  let broker_a = In_memory.create ~sw in
  Bus.register bus_a ~scheme:"in-memory" (In_memory.adapter broker_a);
  let bus_b = Bus.create () in
  let broker_b = In_memory.create ~sw in
  Bus.register bus_b ~scheme:"in-memory" (In_memory.adapter broker_b);
  let a_received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus_a ~uri:"in-memory://shared" ~group:"g" ~deserialize:Fun.id)
      (fun s -> a_received := s :: !a_received)
  in
  let producer_b = Bus.producer bus_b ~uri:"in-memory://shared" ~serialize:Fun.id in
  Bus.publish producer_b "from-b";
  drain ();
  Alcotest.(check (list string)) "broker_a sees no broker_b traffic" [] !a_received

let test_different_deserializers_on_same_uri () =
  (* Pin the no-bridge invariant: two consumers in different groups
     reading the same URI may use different deserializers. The wire
     payload is the contract; OCaml types are local to each consumer. *)
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let int_received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"as-int"
         ~deserialize:int_of_string) (fun n -> int_received := n :: !int_received)
  in
  let str_received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"as-str" ~deserialize:Fun.id)
      (fun s -> str_received := s :: !str_received)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:string_of_int in
  Bus.publish producer 42;
  drain ();
  Alcotest.(check (list int)) "int consumer" [ 42 ] !int_received;
  Alcotest.(check (list string)) "str consumer" [ "42" ] !str_received

let test_deserialize_failure_is_isolated () =
  (* A payload one group cannot decode is dropped for that group only;
     the other group on the same topic still receives it, and the
     dispatcher keeps serving later messages. *)
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let int_received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"as-int"
         ~deserialize:int_of_string) (fun n -> int_received := n :: !int_received)
  in
  let str_received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"as-str" ~deserialize:Fun.id)
      (fun s -> str_received := s :: !str_received)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "not-an-int";
  Bus.publish producer "7";
  drain ();
  Alcotest.(check (list int)) "int consumer skips the bad payload" [ 7 ] !int_received;
  Alcotest.(check (list string))
    "str consumer sees both" [ "not-an-int"; "7" ] (List.rev !str_received)

let test_subscriber_exception_does_not_stop_dispatch () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let received = ref [] in
  let _ : Bus.subscription =
    Bus.subscribe
      (Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id)
      (fun s ->
        if s = "boom" then failwith "subscriber failure" else received := s :: !received)
  in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "boom";
  Bus.publish producer "after";
  drain ();
  Alcotest.(check (list string))
    "dispatch continues past the failure" [ "after" ] !received

let test_unsubscribe_stops_delivery () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let received = ref [] in
  let consumer =
    Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id
  in
  let sub = Bus.subscribe consumer (fun s -> received := s :: !received) in
  let producer = Bus.producer bus ~uri:"in-memory://test.t1" ~serialize:Fun.id in
  Bus.publish producer "before";
  drain ();
  Bus.unsubscribe sub;
  Bus.publish producer "after";
  drain ();
  Alcotest.(check (list string)) "only pre-unsubscribe message" [ "before" ] !received

let test_unsubscribe_idempotent () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus, _broker = setup ~sw in
  let consumer =
    Bus.consumer bus ~uri:"in-memory://test.t1" ~group:"g" ~deserialize:Fun.id
  in
  let sub = Bus.subscribe consumer (fun _ -> ()) in
  Bus.unsubscribe sub;
  Bus.unsubscribe sub;
  Alcotest.(check pass) "no exception on second unsubscribe" () ()

let () =
  Alcotest.run "In_memory"
    [
      ( "delivery",
        [
          Alcotest.test_case "producer-consumer roundtrip" `Quick
            test_producer_consumer_roundtrip;
          Alcotest.test_case "publish preserves order" `Quick test_publish_preserves_order;
          Alcotest.test_case "publish without consumer" `Quick
            test_publish_without_consumer_no_error;
          Alcotest.test_case "cross-group fan-out" `Quick test_cross_group_fan_out;
          Alcotest.test_case "different deserializers on same URI" `Quick
            test_different_deserializers_on_same_uri;
        ] );
      ( "isolation",
        [
          Alcotest.test_case "single-consumer-per-group invariant" `Quick
            test_single_consumer_per_group_invariant;
          Alcotest.test_case "cross-URI isolation" `Quick test_cross_uri_isolation;
          Alcotest.test_case "cross-broker isolation" `Quick test_cross_broker_isolation;
        ] );
      ( "resilience",
        [
          Alcotest.test_case "deserialize failure is isolated" `Quick
            test_deserialize_failure_is_isolated;
          Alcotest.test_case "subscriber exception does not stop dispatch" `Quick
            test_subscriber_exception_does_not_stop_dispatch;
        ] );
      ( "subscription",
        [
          Alcotest.test_case "unsubscribe stops delivery" `Quick
            test_unsubscribe_stops_delivery;
          Alcotest.test_case "unsubscribe idempotent" `Quick test_unsubscribe_idempotent;
        ] );
    ]
