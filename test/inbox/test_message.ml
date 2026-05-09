(** Pure-function tests for [Inbox_message] helpers and
    [Causal_dependency] JSON roundtrip. No database required. *)

module Inbox_message = Ascetic_inbox.Inbox_message
module Causal_dependency = Ascetic_inbox.Causal_dependency

let basic_message ?metadata ?received_position ?processed_position () =
  Inbox_message.make ~tenant_id:"tenant1" ~stream_type:"Order"
    ~stream_id:(`Assoc [ ("id", `String "order-123") ])
    ~stream_position:1 ~uri:"kafka://orders"
    ~payload:(`Assoc [ ("amount", `Int 100) ])
    ?metadata ?received_position ?processed_position ()

(* -------------------------------------------------------------------------- *)
(* Inbox_message construction & defaults                                      *)
(* -------------------------------------------------------------------------- *)

let test_create_message () =
  let m = basic_message () in
  Alcotest.(check string) "tenant" "tenant1" m.tenant_id;
  Alcotest.(check string) "stream_type" "Order" m.stream_type;
  Alcotest.(check int) "stream_position" 1 m.stream_position;
  Alcotest.(check string) "uri" "kafka://orders" m.uri;
  Alcotest.(check bool) "metadata is None" true (m.metadata = None);
  Alcotest.(check bool)
    "received_position is None" true
    (m.received_position = None);
  Alcotest.(check bool)
    "processed_position is None" true
    (m.processed_position = None)

let test_create_message_with_metadata () =
  let m =
    basic_message
      ~metadata:
        (`Assoc
          [
            ("event_id", `String "uuid-123");
            ("timestamp", `String "2024-01-01T00:00:00Z");
          ])
      ()
  in
  Alcotest.(check (option string))
    "event_id" (Some "uuid-123") (Inbox_message.event_id m)

let test_received_and_processed_positions () =
  let m = basic_message ~received_position:100L ~processed_position:50L () in
  Alcotest.(check (option int64))
    "received_position" (Some 100L) m.received_position;
  Alcotest.(check (option int64))
    "processed_position" (Some 50L) m.processed_position

(* -------------------------------------------------------------------------- *)
(* causal_dependencies                                                        *)
(* -------------------------------------------------------------------------- *)

let test_causal_dependencies_empty_when_no_metadata () =
  let m = basic_message () in
  Alcotest.(check int)
    "no deps without metadata" 0
    (List.length (Inbox_message.causal_dependencies m))

let test_causal_dependencies_empty_when_not_present () =
  let m =
    basic_message ~metadata:(`Assoc [ ("event_id", `String "uuid-1") ]) ()
  in
  Alcotest.(check int)
    "no deps when key missing" 0
    (List.length (Inbox_message.causal_dependencies m))

let test_causal_dependencies_returns_list () =
  let dep1 =
    Causal_dependency.make ~tenant_id:"tenant1" ~stream_type:"User"
      ~stream_id:(`Assoc [ ("id", `String "user-1") ])
      ~stream_position:5
  in
  let dep2 =
    Causal_dependency.make ~tenant_id:"tenant1" ~stream_type:"Product"
      ~stream_id:(`Assoc [ ("id", `String "prod-1") ])
      ~stream_position:3
  in
  let m =
    basic_message
      ~metadata:
        (`Assoc
          [
            ( "causal_dependencies",
              `List
                [
                  Causal_dependency.to_json dep1;
                  Causal_dependency.to_json dep2;
                ] );
          ])
      ()
  in
  let deps = Inbox_message.causal_dependencies m in
  Alcotest.(check int) "two deps" 2 (List.length deps);
  let d1 = List.nth deps 0 in
  Alcotest.(check string) "first tenant" "tenant1" d1.tenant_id;
  Alcotest.(check string) "first stream_type" "User" d1.stream_type;
  Alcotest.(check int) "first position" 5 d1.stream_position;
  let d2 = List.nth deps 1 in
  Alcotest.(check string) "second stream_type" "Product" d2.stream_type;
  Alcotest.(check int) "second position" 3 d2.stream_position

(* Edge case beyond the Python tests: a malformed dependency entry inside
   the list is silently dropped rather than crashing. *)
let test_causal_dependencies_skips_malformed () =
  let valid =
    Causal_dependency.make ~tenant_id:"t" ~stream_type:"S"
      ~stream_id:(`Int 1) ~stream_position:1
  in
  let m =
    basic_message
      ~metadata:
        (`Assoc
          [
            ( "causal_dependencies",
              `List
                [
                  `Assoc [ ("not_a_dependency", `Bool true) ];
                  Causal_dependency.to_json valid;
                  `String "garbage";
                ] );
          ])
      ()
  in
  let deps = Inbox_message.causal_dependencies m in
  Alcotest.(check int) "only valid kept" 1 (List.length deps);
  Alcotest.(check string) "valid tenant" "t" (List.hd deps).tenant_id

(* -------------------------------------------------------------------------- *)
(* event_id                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_event_id_none_when_no_metadata () =
  let m = basic_message () in
  Alcotest.(check (option string))
    "no event_id without metadata" None (Inbox_message.event_id m)

let test_event_id_returns_value () =
  let m =
    basic_message ~metadata:(`Assoc [ ("event_id", `String "uuid-456") ]) ()
  in
  Alcotest.(check (option string))
    "event_id present" (Some "uuid-456") (Inbox_message.event_id m)

(* -------------------------------------------------------------------------- *)
(* Causal_dependency JSON roundtrip                                            *)
(* -------------------------------------------------------------------------- *)

let test_causal_dependency_roundtrip () =
  let original =
    Causal_dependency.make ~tenant_id:"tenant1" ~stream_type:"Order"
      ~stream_id:(`Assoc [ ("id", `String "order-42") ])
      ~stream_position:7
  in
  match Causal_dependency.of_json (Causal_dependency.to_json original) with
  | None -> Alcotest.fail "expected Some after roundtrip"
  | Some d ->
      Alcotest.(check string) "tenant" "tenant1" d.tenant_id;
      Alcotest.(check string) "stream_type" "Order" d.stream_type;
      Alcotest.(check int) "position" 7 d.stream_position;
      Alcotest.(check string)
        "stream_id roundtrip"
        (Yojson.Safe.to_string original.stream_id)
        (Yojson.Safe.to_string d.stream_id)

let test_causal_dependency_of_json_rejects_garbage () =
  Alcotest.(check bool)
    "string is not a dependency" true
    (Causal_dependency.of_json (`String "x") = None);
  Alcotest.(check bool)
    "missing field" true
    (Causal_dependency.of_json
       (`Assoc [ ("tenant_id", `String "x") ])
    = None)

(* -------------------------------------------------------------------------- *)
(* Runner                                                                     *)
(* -------------------------------------------------------------------------- *)

let () =
  Alcotest.run "Inbox_message"
    [
      ( "construction",
        [
          Alcotest.test_case "create" `Quick test_create_message;
          Alcotest.test_case "create_with_metadata" `Quick
            test_create_message_with_metadata;
          Alcotest.test_case "received_and_processed_positions" `Quick
            test_received_and_processed_positions;
        ] );
      ( "causal_dependencies",
        [
          Alcotest.test_case "empty_when_no_metadata" `Quick
            test_causal_dependencies_empty_when_no_metadata;
          Alcotest.test_case "empty_when_not_present" `Quick
            test_causal_dependencies_empty_when_not_present;
          Alcotest.test_case "returns_list" `Quick
            test_causal_dependencies_returns_list;
          Alcotest.test_case "skips_malformed" `Quick
            test_causal_dependencies_skips_malformed;
        ] );
      ( "event_id",
        [
          Alcotest.test_case "none_when_no_metadata" `Quick
            test_event_id_none_when_no_metadata;
          Alcotest.test_case "returns_value" `Quick
            test_event_id_returns_value;
        ] );
      ( "Causal_dependency",
        [
          Alcotest.test_case "roundtrip" `Quick
            test_causal_dependency_roundtrip;
          Alcotest.test_case "of_json_rejects_garbage" `Quick
            test_causal_dependency_of_json_rejects_garbage;
        ] );
    ]
