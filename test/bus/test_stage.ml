(** Stages of the wire: a message goes out through them and comes back in through them,
    and a stage's failure is the message's. *)

open Ascetic_bus
module Broker = Ascetic_bus_in_memory.In_memory_broker

let ok what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Bus_error.pp e

let reverse text =
  String.init (String.length text) (fun i -> text.[String.length text - 1 - i])

(* Reverses the payload on the way out and back on the way in, and marks the
   message with a header while it is reversed. *)
let reversing : Stage.t =
  {
    outbound =
      (fun message ->
        Ok
          (Message.with_header
             (Message.with_payload message (reverse (Message.payload message)))
             "reversed" "yes"));
    inbound =
      (fun message ->
        match Message.header message "reversed" with
        | None -> Error (Failure.transient "the message was not reversed")
        | Some _ ->
            Ok
              (Message.without_header
                 (Message.with_payload message (reverse (Message.payload message)))
                 "reversed"));
  }

(* Appends a byte on the way out and strips it on the way in. *)
let tagging tag : Stage.t =
  {
    outbound =
      (fun message ->
        Ok (Message.with_payload message (Message.payload message ^ String.make 1 tag)));
    inbound =
      (fun message ->
        let payload = Message.payload message in
        let n = String.length payload in
        if n > 0 && payload.[n - 1] = tag then
          Ok (Message.with_payload message (String.sub payload 0 (n - 1)))
        else Error (Failure.permanent "not tagged by this stage"));
  }

(* Refuses every message. *)
let refusing : Stage.t =
  {
    outbound = (fun _ -> Error (Failure.transient "refused on the way out"));
    inbound = (fun _ -> Error (Failure.permanent "refused on the way in"));
  }

let settle () =
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done

let with_bus f =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  f
    (ok "register"
       (Bus.register Bus.empty ~scheme:"in-memory"
          (Broker.adapter (Broker.create ~sw ()))))

let test_a_message_goes_out_through_the_stages_and_comes_back_in_through_them () =
  with_bus @@ fun bus ->
  let seen = ref [] and raw = ref [] in
  let consumer =
    Consumer.through
      (Consumer.through
         (ok "consumer"
            (Bus.consumer bus ~uri:"in-memory://orders" ~group:"billing" ~decode:(fun m ->
                 Ok (Message.payload m))))
         reversing)
      (tagging '!')
  in
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe consumer (fun order ->
           seen := order :: !seen;
           Ok ()))
  in
  (* Without the stages, the wire carries the reversed, tagged bytes. *)
  let _ : Subscription.t =
    ok "subscribe"
      (Consumer.subscribe
         (ok "consumer"
            (Bus.consumer bus ~uri:"in-memory://orders" ~group:"audit" ~decode:(fun m ->
                 Ok m)))
         (fun message ->
           raw := message :: !raw;
           Ok ()))
  in
  let producer =
    Producer.through
      (Producer.through
         (ok "producer" (Bus.producer bus ~uri:"in-memory://orders" ~encode:Message.make))
         reversing)
      (tagging '!')
  in
  ok "publish" (Producer.publish producer "order-7");
  settle ();
  Alcotest.(check (list string)) "as it was sent" [ "order-7" ] !seen;
  match !raw with
  | [ on_the_wire ] ->
      Alcotest.(check string)
        "reversed, then tagged" "7-redro!" (Message.payload on_the_wire);
      Alcotest.(check (option string))
        "marked" (Some "yes")
        (Message.header on_the_wire "reversed")
  | others -> Alcotest.failf "the audit saw %d messages" (List.length others)

let test_a_stage_that_refuses_fails_the_publish () =
  with_bus @@ fun bus ->
  let producer =
    Producer.through
      (ok "producer" (Bus.producer bus ~uri:"in-memory://orders" ~encode:Message.make))
      refusing
  in
  match Producer.publish producer "order-7" with
  | Error (Bus_error.Stage failure) ->
      Alcotest.(check string)
        "the stage's reason" "refused on the way out" (Failure.message failure)
  | Error e -> Alcotest.failf "another error: %a" Bus_error.pp e
  | Ok () -> Alcotest.fail "the publish went through"

(* The transactional twins, over fake wires that hand the handler over. *)

let test_a_transactional_producer_publishes_what_its_stages_made () =
  let seen = ref [] in
  let wire : string Transactional.wire_producer =
    {
      publish =
        (fun tx message ->
          seen := (tx, Message.payload message) :: !seen;
          Ok ());
    }
  in
  let producer =
    Transactional.Producer.through
      (Transactional.Producer.make wire ~encode:(fun order ->
           Message.make (string_of_int order)))
      reversing
  in
  ok "publish" (Transactional.Producer.publish producer "tx-1" 42);
  Alcotest.(check (list (pair string string)))
    "reversed, in the transaction"
    [ ("tx-1", "24") ]
    !seen

let test_a_stage_that_refuses_on_the_way_in_fails_the_handling_and_keeps_its_verdict () =
  let slot = ref None in
  let wire : string Transactional.wire_consumer =
    {
      subscribe =
        (fun handler ->
          slot := Some handler;
          Ok (Subscription.make ignore));
    }
  in
  let consumer =
    Transactional.Consumer.through
      (Transactional.Consumer.through
         (Transactional.Consumer.make wire ~decode:(fun m -> Ok (Message.payload m)))
         reversing)
      (tagging '!')
  in
  let handled = ref [] in
  let _ : Subscription.t =
    ok "subscribe"
      (Transactional.Consumer.subscribe consumer (fun tx order ->
           handled := (tx, order) :: !handled;
           Ok ()))
  in
  let handler = Option.get !slot in
  (* What the stages made on the way out comes back in, in reverse order. *)
  let outcome =
    handler "tx-1" (Message.with_header (Message.make "7-redro!") "reversed" "yes")
  in
  Alcotest.(check bool) "handled" true (Result.is_ok outcome);
  Alcotest.(check (list (pair string string)))
    "as it was sent"
    [ ("tx-1", "order-7") ]
    !handled;
  (* A message the stages cannot take back is not handled, and not skipped. *)
  (match handler "tx-2" (Message.make "untagged") with
  | Error failure ->
      Alcotest.(check bool) "the verdict is kept" true (Failure.is_permanent failure)
  | Ok () -> Alcotest.fail "the message was skipped");
  Alcotest.(check int) "not handled" 1 (List.length !handled)

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run "Stage"
    [
      ( "stages",
        [
          case "a message goes out through the stages and comes back in through them"
            test_a_message_goes_out_through_the_stages_and_comes_back_in_through_them;
          case "a stage that refuses fails the publish"
            test_a_stage_that_refuses_fails_the_publish;
          case "a transactional producer publishes what its stages made"
            test_a_transactional_producer_publishes_what_its_stages_made;
          case
            "a stage that refuses on the way in fails the handling and keeps its verdict"
            test_a_stage_that_refuses_on_the_way_in_fails_the_handling_and_keeps_its_verdict;
        ] );
    ]
