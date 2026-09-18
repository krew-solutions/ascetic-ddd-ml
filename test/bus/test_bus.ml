(** Tests for the bus itself: the scheme registry and its error paths. The messaging
    behaviour lives in the adapters and is exercised by their tests. *)

open Ascetic_bus
module Broker = Ascetic_bus_in_memory.In_memory_broker

let error = Alcotest.testable Bus_error.pp Bus_error.equal
let as_text message = Ok (Message.payload message)

let with_bus f =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  match
    Bus.register Bus.empty ~scheme:"in-memory" (Broker.adapter (Broker.create ~sw ()))
  with
  | Ok bus -> f ~sw bus
  | Error e -> Alcotest.failf "register: %a" Bus_error.pp e

let refused = function Ok _ -> None | Error e -> Some e

let test_an_unknown_scheme_is_refused () =
  with_bus @@ fun ~sw:_ bus ->
  Alcotest.(check (option error))
    "the scheme is named" (Some (Bus_error.Unknown_scheme "kafka"))
    (refused (Bus.consumer bus ~uri:"kafka://x" ~group:"g" ~decode:as_text))

let test_a_producer_on_an_unknown_scheme_is_refused () =
  with_bus @@ fun ~sw:_ bus ->
  Alcotest.(check (option error))
    "the scheme is named" (Some (Bus_error.Unknown_scheme "kafka"))
    (refused (Bus.producer bus ~uri:"kafka://x" ~encode:Message.make))

let test_a_scheme_is_registered_once () =
  with_bus @@ fun ~sw bus ->
  Alcotest.(check (option error))
    "refused" (Some (Bus_error.Already_registered "in-memory"))
    (refused
       (Bus.register bus ~scheme:"in-memory" (Broker.adapter (Broker.create ~sw ()))))

let test_a_uri_without_a_scheme_is_refused () =
  with_bus @@ fun ~sw:_ bus ->
  Alcotest.(check (option error))
    "the URI is named" (Some (Bus_error.Unknown_scheme "no-scheme-here"))
    (refused (Bus.consumer bus ~uri:"no-scheme-here" ~group:"g" ~decode:as_text))

(* The registry is a value of each bus, not global. *)
let test_buses_do_not_share_their_registry () =
  with_bus @@ fun ~sw:_ _first ->
  Alcotest.(check (option error))
    "the second bus knows no scheme" (Some (Bus_error.Unknown_scheme "in-memory"))
    (refused (Bus.consumer Bus.empty ~uri:"in-memory://x" ~group:"g" ~decode:as_text))

let test_the_shape_of_a_uri () =
  Alcotest.(check (result string error))
    "scheme" (Ok "kafka")
    (Bus_uri.scheme "kafka://orders/order-7");
  Alcotest.(check (result string error))
    "channel" (Ok "orders")
    (Bus_uri.channel "kafka://orders/order-7");
  Alcotest.(check (option string))
    "key" (Some "order-7")
    (Bus_uri.key "kafka://orders/order-7");
  Alcotest.(check (option string))
    "a key may hold slashes" (Some "a/b")
    (Bus_uri.key "kafka://orders/a/b");
  Alcotest.(check (option string)) "no key" None (Bus_uri.key "in-memory://orders");
  Alcotest.(check (option string))
    "an empty key is none" None
    (Bus_uri.key "in-memory://orders/");
  Alcotest.(check string)
    "without its key" "kafka://orders"
    (Bus_uri.without_key "kafka://orders/order-7");
  Alcotest.(check string)
    "nothing to drop" "kafka://orders"
    (Bus_uri.without_key "kafka://orders");
  Alcotest.(check bool) "no channel" true (Result.is_error (Bus_uri.channel "kafka://"))

let () =
  Alcotest.run "Bus"
    [
      ( "registry",
        [
          Alcotest.test_case "an unknown scheme is refused" `Quick
            test_an_unknown_scheme_is_refused;
          Alcotest.test_case "a producer on an unknown scheme is refused" `Quick
            test_a_producer_on_an_unknown_scheme_is_refused;
          Alcotest.test_case "a scheme is registered once" `Quick
            test_a_scheme_is_registered_once;
          Alcotest.test_case "a uri without a scheme is refused" `Quick
            test_a_uri_without_a_scheme_is_refused;
          Alcotest.test_case "buses do not share their registry" `Quick
            test_buses_do_not_share_their_registry;
        ] );
      ("uri", [ Alcotest.test_case "the shape of a uri" `Quick test_the_shape_of_a_uri ]);
    ]
