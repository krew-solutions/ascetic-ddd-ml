(** What the inbox offers without a database: the port a fake can implement, the snapshot
    a walk reports, the backoff of the retries. *)

module Inbox_port = Ascetic_inbox.Inbox_port
module Message = Ascetic_inbox.Inbox_message
module Snapshot = Ascetic_inbox.Snapshot
module Retries = Ascetic_inbox.Retries
module Causal_dependency = Ascetic_inbox.Causal_dependency

let message stream position =
  Message.make ~tenant_id:"tenant-1" ~stream_type:"orders.Order"
    ~stream_id:(`Assoc [ ("id", `String stream) ])
    ~stream_position:position
    ~uri:(Printf.sprintf "kafka://orders/%s" stream)
    ~payload:(Printf.sprintf "%s@%d" stream position)

module Fake : sig
  include Inbox_port.S

  val create : unit -> t
  val received : t -> string list
end = struct
  type t = string list ref

  let create () = ref []
  let received t = List.rev !t

  let publish t (message : Message.t) =
    t := message.payload :: !t;
    Ok ()
end

(* The adapter is an instance of the port. *)
module Pg : Inbox_port.S with type t = Ascetic_inbox.Pg_inbox.t = struct
  type t = Ascetic_inbox.Pg_inbox.t

  let publish = Ascetic_inbox.Pg_inbox.publish
end

let _ = Pg.publish

let test_the_port_is_implementable_without_a_database () =
  let fake = Fake.create () in
  Alcotest.(check bool) "received" true (Result.is_ok (Fake.publish fake (message "a" 1)));
  Alcotest.(check (list string)) "collected" [ "a@1" ] (Fake.received fake)

let snapshot = Alcotest.testable Snapshot.pp Snapshot.equal

let test_a_snapshot_is_parsed_from_its_text () =
  Alcotest.(check (result snapshot string))
    "three parts"
    (Ok { Snapshot.xmin = 10; xmax = 20; in_progress = [ 12; 15 ] })
    (Snapshot.of_string "10:20:12,15");
  Alcotest.(check (result snapshot string))
    "nothing in progress"
    (Ok { Snapshot.xmin = 10; xmax = 10; in_progress = [] })
    (Snapshot.of_string "10:10:");
  Alcotest.(check bool) "two parts" true (Result.is_error (Snapshot.of_string "10:20"));
  Alcotest.(check bool) "not numbers" true (Result.is_error (Snapshot.of_string "a:b:"))

let test_it_sees_what_had_ended_and_not_what_was_in_progress_or_not_yet_started () =
  let snapshot = Result.get_ok (Snapshot.of_string "10:20:12,15") in
  Alcotest.(check bool) "ended before xmin" true (Snapshot.sees snapshot 9);
  Alcotest.(check bool) "between, not in progress" true (Snapshot.sees snapshot 11);
  Alcotest.(check bool) "in progress" false (Snapshot.sees snapshot 12);
  Alcotest.(check bool) "just below xmax" true (Snapshot.sees snapshot 19);
  Alcotest.(check bool) "not yet started" false (Snapshot.sees snapshot 20);
  Alcotest.(check bool) "later still" false (Snapshot.sees snapshot 21)

let test_the_dependencies_travel_as_json_or_as_its_text () =
  let cause = message "user" 5 in
  let dependent = Message.depending_on (message "order" 1) [ Message.identity cause ] in
  Alcotest.(check (list string))
    "structured"
    [ Causal_dependency.to_string (Message.identity cause) ]
    (List.map Causal_dependency.to_string (Message.causal_dependencies dependent));
  let flat =
    Message.with_metadata (message "order" 1)
      (`Assoc
         [
           ( "causal_dependencies",
             `String
               (Yojson.Safe.to_string
                  (`List [ Causal_dependency.to_json (Message.identity cause) ])) );
         ])
  in
  Alcotest.(check (list string))
    "as text, how flat broker headers carry it"
    [ Causal_dependency.to_string (Message.identity cause) ]
    (List.map Causal_dependency.to_string (Message.causal_dependencies flat));
  let noise =
    Message.with_metadata (message "order" 1)
      (`Assoc [ ("causal_dependencies", `List [ `Int 7 ]) ])
  in
  Alcotest.(check int)
    "entries that are not dependencies are ignored" 0
    (List.length (Message.causal_dependencies noise))

let test_the_backoff_doubles_up_to_the_cap () =
  let backoff = Retries.exponential ~base:1.0 ~cap:10.0 in
  Alcotest.(check (list (float 0.001)))
    "1, 2, 4, 8, 10, 10"
    [ 1.0; 2.0; 4.0; 8.0; 10.0; 10.0 ]
    (List.map backoff [ 1; 2; 3; 4; 5; 6 ]);
  Alcotest.(check int) "unlimited by default" 0 Retries.unlimited.max_attempts;
  Alcotest.(check (float 0.001)) "no backoff by default" 0.0 (Retries.unlimited.backoff 3)

let () =
  Alcotest.run "Inbox port"
    [
      ( "port",
        [
          Alcotest.test_case "the port is implementable without a database" `Quick
            test_the_port_is_implementable_without_a_database;
        ] );
      ( "snapshot",
        [
          Alcotest.test_case "a snapshot is parsed from its text" `Quick
            test_a_snapshot_is_parsed_from_its_text;
          Alcotest.test_case
            "it sees what had ended and not what was in progress or not yet started"
            `Quick
            test_it_sees_what_had_ended_and_not_what_was_in_progress_or_not_yet_started;
        ] );
      ( "message",
        [
          Alcotest.test_case "the dependencies travel as JSON or as its text" `Quick
            test_the_dependencies_travel_as_json_or_as_its_text;
        ] );
      ( "retries",
        [
          Alcotest.test_case "the backoff doubles up to the cap" `Quick
            test_the_backoff_doubles_up_to_the_cap;
        ] );
    ]
