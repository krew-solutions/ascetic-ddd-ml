(** The transactional producer and consumer pass the caller's transaction through to the
    wire, untouched and unknown to the bus. *)

open Ascetic_bus

let ok what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Bus_error.pp e

(* A string stands in for a session: the bus only passes it through. *)

let test_the_handler_gets_the_transaction_with_the_decoded_value () =
  let slot = ref None in
  (* Keeps the handler it is given, so the test can call it with transactions
     of its own. *)
  let wire : string Transactional.wire_consumer =
    {
      subscribe =
        (fun handler ->
          slot := Some handler;
          Ok (Subscription.make ignore));
    }
  in
  let consumer =
    Transactional.Consumer.make wire ~decode:(fun message ->
        Option.to_result ~none:"not a number"
          (int_of_string_opt (Message.payload message)))
  in
  let seen = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Transactional.Consumer.subscribe consumer (fun tx order ->
           seen := (tx, order) :: !seen;
           Ok ()))
  in
  let handler = Option.get !slot in
  let handled tx payload =
    Alcotest.(check bool) tx true (Result.is_ok (handler tx (Message.make payload)))
  in
  handled "tx-1" "7";
  (* undecodable: reported and acknowledged, not an error of the transport *)
  handled "tx-2" "not a number";
  handled "tx-3" "8";
  Alcotest.(check (list (pair string int)))
    "each in its transaction"
    [ ("tx-1", 7); ("tx-3", 8) ]
    (List.rev !seen)

let test_the_value_is_encoded_and_published_in_the_given_transaction () =
  let seen = ref [] in
  (* Records what it was asked to publish, and in which transaction. *)
  let wire : string Transactional.wire_producer =
    {
      publish =
        (fun tx message ->
          seen := (tx, Message.payload message) :: !seen;
          Ok ());
    }
  in
  let producer =
    Transactional.Producer.make wire ~encode:(fun order ->
        Message.make (string_of_int order))
  in
  ok "publish" (Transactional.Producer.publish producer "tx-1" 7);
  ok "publish" (Transactional.Producer.publish producer "tx-2" 8);
  Alcotest.(check (list (pair string string)))
    "each in its transaction"
    [ ("tx-1", "7"); ("tx-2", "8") ]
    (List.rev !seen)

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Transactional"
    [
      ( "pass-through",
        [
          case "the handler gets the transaction with the decoded value"
            test_the_handler_gets_the_transaction_with_the_decoded_value;
          case "the value is encoded and published in the given transaction"
            test_the_value_is_encoded_and_published_in_the_given_transaction;
        ] );
    ]
