(** Integration tests for the PostgreSQL outbox. They need a live database, and are
    skipped when [TEST_DATABASE_URL] is not set:

    {v
      export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
      dune test test/outbox
    v}

    With [ASCETIC_DDD_TRACE_DIR] set, every test writes what its outbox reported as
    [outbox-<name>.jsonl] into that directory, one event per line, for validation against
    the protocol model: see [verify/tla/README.md]. A test with several dispatchers at
    once records nothing: the order its events are logged in is not the order of their
    commits. *)

module Outbox = Ascetic_outbox.Pg_outbox
module Message = Ascetic_outbox.Outbox_message
module Position = Ascetic_outbox.Position
module Selection = Ascetic_outbox.Selection
module Loops = Ascetic_outbox.Loops
module Error = Ascetic_outbox.Outbox_error
module Observer = Ascetic_outbox.Outbox_observer
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Trace_file = Ascetic_trace.Trace_file
module Json_trace = Ascetic_trace.Json_trace

(* The subscriber's error type of these tests. *)
type error = string Error.t

let lift e = Error.Session e
let ( let* ) = Result.bind
let show (error : error) = Error.to_string Fun.id error

let unwrap what = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %s" what (show error)

let position = Alcotest.testable Position.pp Position.equal

(* ------------------------------------------------------------------------ *)
(* Plain SQL through a session's connection                                   *)

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let find_int session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.find ((Caqti_type.unit ->! Caqti_type.int) ~oneshot:true sql) () with
  | Ok n -> n
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let find_bool session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.find ((Caqti_type.unit ->! Caqti_type.bool) ~oneshot:true sql) () with
  | Ok b -> b
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

(* ------------------------------------------------------------------------ *)
(* The fixture                                                                *)

type fixture = {
  sessions : Pool.t;
  outbox : string Outbox.t;
  outbox_table : string;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

let with_session f body = unwrap "session" (Pool.session f.sessions ~lift body)

let connect_pool ~sw ~stdenv uri =
  let pool_config = Caqti_pool_config.create ~max_size:8 () in
  match Caqti_eio_unix.connect_pool ~pool_config ~sw ~stdenv uri with
  | Ok pool -> Pool.of_pool pool
  | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err

(* Tables of its own per test; one slot unless said otherwise. The number of
   slots is a property of the table, so it is set before the table is created.
   [~trace:false] is for a test that runs several dispatchers at once, or
   that exercises what the model does not describe, and records no trace. *)
let with_fixture ?(slots = 1) ?batch_size ?(observer = Observer.none) ?(trace = true)
    ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions = connect_pool ~sw ~stdenv uri in
  let trace =
    if trace then Trace_file.from_env ("outbox-" ^ name) else Trace_file.off ()
  in
  let outbox_table = "outbox_" ^ name in
  let offsets_table = outbox_table ^ "_offsets" in
  let outbox =
    Outbox.create
      ~observer:
        (Observer.all
           [ observer; Json_trace.outbox_observer (Trace_file.recorder trace) ])
      ~outbox_table:(Identifier.of_string_exn outbox_table)
      ~offsets_table:(Identifier.of_string_exn offsets_table)
      ?batch_size ~slots sessions
  in
  let f =
    {
      sessions;
      outbox;
      outbox_table;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
    }
  in
  with_session f (fun session ->
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" outbox_table);
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s_meta" outbox_table);
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" offsets_table);
      Outbox.setup outbox session);
  Fun.protect ~finally:(fun () -> Trace_file.close trace) (fun () -> body f)

let sleep f seconds = Eio.Time.Mono.sleep f.clock seconds

let elapsed f body =
  let started = Eio.Time.Mono.now f.clock in
  let value = body () in
  (value, Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now f.clock)) /. 1e9)

let within f seconds body =
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds f.clock seconds) body

(* Waits until everything in the table is older than every running
   transaction. *)
let wait_visible f =
  let sql =
    Printf.sprintf
      "SELECT coalesce((SELECT transaction_id FROM %s ORDER BY transaction_id DESC LIMIT \
       1) < pg_snapshot_xmin(pg_current_snapshot()), true)"
      f.outbox_table
  in
  let rec wait tries =
    if with_session f (fun session -> Ok (find_bool session sql)) then ()
    else if tries = 0 then Alcotest.fail "messages never became visible"
    else begin
      sleep f 0.02;
      wait (tries - 1)
    end
  in
  wait 1000

(* Publishes each message in a transaction of its own, then waits until the
   dispatcher may see them: a message is visible only once every transaction
   older than its own has ended. *)
let publish f messages =
  List.iter
    (fun message ->
      with_session f (fun session ->
          Session.atomic session ~lift (fun tx -> Outbox.publish f.outbox tx message)))
    messages;
  wait_visible f

let positions f selection =
  with_session f (fun session -> Outbox.positions f.outbox session selection)

let dispatch f selection subscriber =
  unwrap "dispatch" (Outbox.dispatch f.outbox selection subscriber)

(* Two URIs that land in different slots, by the table's cut, for tests that
   need two slots busy at once. *)
let uris_in_different_slots f =
  let slots = Outbox.slots f.outbox in
  let slot_of uri =
    with_session f (fun session ->
        let module C = (val Session.connection session) in
        let open Caqti_request.Infix in
        let request =
          (Caqti_type.(t2 string int) ->! Caqti_type.int)
            "SELECT (hashtext($1) & 2147483647) % $2"
        in
        match C.find request (uri, slots) with
        | Ok slot -> Ok slot
        | Error err -> Alcotest.failf "slot of %s: %a" uri Caqti_error.pp err)
  in
  let first = "kafka://orders/order-1" in
  let slot = slot_of first in
  let rec other i =
    let candidate = Printf.sprintf "kafka://orders/order-%d" i in
    if slot_of candidate <> slot then candidate else other (i + 1)
  in
  (first, other 2)

let event = ref 0

let message uri id =
  incr event;
  Message.make ~uri ~payload:(string_of_int id)
    ~metadata:
      (`Assoc
         [
           ("message_id", `String (Printf.sprintf "00000000-0000-4000-8000-%012x" !event));
         ])

let id_of (message : Message.t) = int_of_string message.payload

(* A subscriber that collects the ids it was given. *)
let collector () =
  let seen = ref [] in
  ( (fun () -> List.rev !seen),
    fun (message : Message.t) ->
      seen := id_of message :: !seen;
      Ok () )

let broker = Selection.group "broker"

(* ------------------------------------------------------------------------ *)
(* The tests                                                                  *)

let test_published_messages_are_dispatched_in_order_once env uri () =
  with_fixture ~name:"roundtrip" env uri @@ fun f ->
  publish f [ message "kafka://orders" 1; message "kafka://orders" 2 ];
  let seen, subscriber = collector () in
  Alcotest.(check bool) "a batch" true (dispatch f broker subscriber);
  Alcotest.(check bool) "nothing more" false (dispatch f broker subscriber);
  Alcotest.(check (list int)) "in order, once" [ 1; 2 ] (seen ())

let test_every_consumer_group_gets_every_message env uri () =
  with_fixture ~name:"groups" env uri @@ fun f ->
  publish f [ message "kafka://orders" 1 ];
  let seen_a, a = collector () in
  let seen_b, b = collector () in
  ignore (dispatch f (Selection.group "a") a);
  ignore (dispatch f (Selection.group "b") b);
  Alcotest.(check (list int)) "a" [ 1 ] (seen_a ());
  Alcotest.(check (list int)) "b" [ 1 ] (seen_b ())

let test_the_position_follows_the_last_acknowledged_message_and_can_be_moved env uri () =
  with_fixture ~name:"position" ~trace:false env uri @@ fun f ->
  Alcotest.(check (list position)) "no contact yet" [] (positions f broker);
  publish f [ message "kafka://orders" 1; message "kafka://orders" 2 ];
  let seen, subscriber = collector () in
  ignore (dispatch f broker subscriber);
  let position = List.hd (positions f broker) in
  Alcotest.(check bool) "a transaction" true (position.transaction_id > 0);
  Alcotest.(check int) "the last message" 2 position.offset;
  (* Back to the beginning: everything is delivered again. *)
  with_session f (fun session ->
      Outbox.set_position f.outbox session broker Position.zero);
  ignore (dispatch f broker subscriber);
  Alcotest.(check (list int)) "delivered again" [ 1; 2; 1; 2 ] (seen ())

let test_a_uri_selects_itself_and_everything_under_it env uri () =
  with_fixture ~name:"uri" ~trace:false env uri @@ fun f ->
  publish f
    [
      message "kafka://orders" 1;
      message "kafka://orders/order-7" 2;
      message "kafka://payments" 3;
    ];
  let orders_seen, orders = collector () in
  let payments_seen, payments = collector () in
  ignore (dispatch f (Selection.uri broker "kafka://orders") orders);
  ignore (dispatch f (Selection.uri broker "kafka://payments") payments);
  Alcotest.(check (list int)) "orders and everything under it" [ 1; 2 ] (orders_seen ());
  Alcotest.(check (list int)) "payments" [ 3 ] (payments_seen ());
  (* Each (group, uri) has a position of its own. *)
  Alcotest.(check int)
    "the payments position" 3
    (List.hd (positions f (Selection.uri broker "kafka://payments"))).offset;
  Alcotest.(check (list position))
    "the group alone was never dispatched" [] (positions f broker)

let test_a_batch_is_at_most_batch_size_messages env uri () =
  with_fixture ~name:"batch" ~batch_size:2 env uri @@ fun f ->
  publish f (List.init 5 (fun i -> message "kafka://orders" (i + 1)));
  let seen, subscriber = collector () in
  let rec drain batches =
    if dispatch f broker subscriber then drain (batches + 1) else batches
  in
  Alcotest.(check int) "three batches" 3 (drain 0);
  Alcotest.(check (list int)) "all of them" [ 1; 2; 3; 4; 5 ] (seen ())

(* A message of a transaction that is still open is not visible, and neither
   is a later transaction's message that has already committed: the order
   would be lost otherwise. *)
let test_nothing_is_dispatched_past_an_open_transaction env uri () =
  with_fixture ~name:"visibility" env uri @@ fun f ->
  let seen, subscriber = collector () in
  with_session f (fun session ->
      Session.atomic session ~lift (fun open_tx ->
          let* () = Outbox.publish f.outbox open_tx (message "kafka://orders" 1) in
          (* A later transaction commits while the first is open. *)
          let* () =
            Pool.session f.sessions ~lift (fun session ->
                Session.atomic session ~lift (fun tx ->
                    Outbox.publish f.outbox tx (message "kafka://orders" 2)))
          in
          let* dispatched = Outbox.dispatch f.outbox broker subscriber in
          Alcotest.(check bool) "nothing while the first is open" false dispatched;
          Ok ()));
  wait_visible f;
  Alcotest.(check bool) "both once it ended" true (dispatch f broker subscriber);
  Alcotest.(check (list int)) "in order" [ 1; 2 ] (seen ())

let test_a_failing_subscriber_rolls_the_batch_back env uri () =
  with_fixture ~name:"failure" env uri @@ fun f ->
  publish f [ message "kafka://orders" 1 ];
  let failing _ = Error "broker down" in
  (match Outbox.dispatch f.outbox broker failing with
  | Error (Error.Subscriber "broker down") -> ()
  | Error error -> Alcotest.failf "another error: %s" (show error)
  | Ok _ -> Alcotest.fail "the batch went through");
  (* The first contact created the positions inside the transaction that
     rolled back, so there are none yet; either way nothing is acknowledged. *)
  Alcotest.(check bool)
    "nothing acknowledged" true
    (List.for_all (Position.equal Position.zero) (positions f broker));
  let seen, subscriber = collector () in
  Alcotest.(check bool) "delivered again" true (dispatch f broker subscriber);
  Alcotest.(check (list int)) "the same message" [ 1 ] (seen ())

let test_a_message_id_is_published_once env uri () =
  with_fixture ~name:"message_id" env uri @@ fun f ->
  let first = message "kafka://orders" 1 in
  let duplicate = { (message "kafka://orders" 2) with metadata = first.metadata } in
  publish f [ first ];
  let refused =
    Pool.session f.sessions ~lift (fun session ->
        Session.atomic session ~lift (fun tx -> Outbox.publish f.outbox tx duplicate))
  in
  match refused with
  | Error (Error.Database reason) ->
      Alcotest.(check bool) "a defect, not of the moment" false reason.transient
  | Error error -> Alcotest.failf "another error: %s" (show error)
  | Ok () -> Alcotest.fail "the duplicate was accepted"

(* Two dispatchers of one group and one slot: the second finds the slot held
   and nothing else with work, so no message is processed twice. *)
let test_two_dispatchers_of_one_group_do_not_overlap env uri () =
  with_fixture ~name:"lock" ~trace:false env uri @@ fun f ->
  publish f (List.init 3 (fun i -> message "kafka://orders" (i + 1)));
  let seen = ref [] in
  let slow (message : Message.t) =
    sleep f 0.05;
    seen := id_of message :: !seen;
    Ok ()
  in
  let a, b =
    Eio.Fiber.pair (fun () -> dispatch f broker slow) (fun () -> dispatch f broker slow)
  in
  Alcotest.(check bool) "exactly one of them had the batch" true (a <> b);
  Alcotest.(check (list int)) "once, in order" [ 1; 2; 3 ] (List.rev !seen)

(* Two slots, two dispatchers, a subscriber that takes its time: the locks are
   per slot, so the two run side by side, not one after the other. The
   selection's position rows exist already: their creation, at first contact,
   is one transaction's work and would serialize the pair. *)
let test_two_slots_are_dispatched_at_once env uri () =
  with_fixture ~name:"parallel" ~slots:2 ~trace:false env uri @@ fun f ->
  let quick _ = Ok () in
  Alcotest.(check bool) "nothing yet" false (dispatch f broker quick);
  let first, second = uris_in_different_slots f in
  publish f [ message first 1; message second 2 ];
  let slow _ =
    sleep f 0.3;
    Ok ()
  in
  let (a, b), elapsed =
    elapsed f (fun () ->
        Eio.Fiber.pair
          (fun () -> dispatch f broker slow)
          (fun () -> dispatch f broker slow))
  in
  Alcotest.(check (pair bool bool)) "both had a batch" (true, true) (a, b);
  Alcotest.(check bool)
    (Printf.sprintf "two slots took %.3fs: one after the other" elapsed)
    true (elapsed < 0.55)

(* Two URIs in two slots, four messages each, two loops, a subscriber that
   takes its time and notes when it ran: the messages of one URI are never in
   the subscriber at the same time, while the two URIs are; the run is shorter
   than the sum of the pauses, so the check could have seen an overlap. *)
let test_messages_of_one_uri_are_never_dispatched_at_once env uri () =
  let pause = 0.04 in
  with_fixture ~name:"serial_uri" ~slots:2 ~trace:false env uri @@ fun f ->
  let quick _ = Ok () in
  Alcotest.(check bool) "the position rows first" false (dispatch f broker quick);
  let first, second = uris_in_different_slots f in
  publish f
    (List.concat_map
       (fun i -> [ message first i; message second (10 + i) ])
       [ 1; 2; 3; 4 ]);
  let spans = ref [] in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber (message : Message.t) =
    let started = Eio.Time.Mono.now f.clock in
    sleep f pause;
    spans := (message.uri, started, Eio.Time.Mono.now f.clock) :: !spans;
    if List.length !spans = 8 then ignore (Eio.Promise.try_resolve resolve_done ());
    Ok ()
  in
  let loops = { Loops.default with concurrency = 2; poll_interval = 0.005 } in
  let ran, elapsed =
    elapsed f (fun () ->
        within f 10.0 (fun () ->
            Outbox.run f.outbox ~clock:f.clock ~loops ~shutdown:all_done broker subscriber))
  in
  unwrap "run" ran;
  List.iter
    (fun uri ->
      let own =
        List.sort
          (fun (_, a, _) (_, b, _) -> Mtime.compare a b)
          (List.filter (fun (u, _, _) -> u = uri) !spans)
      in
      Alcotest.(check int) "four runs" 4 (List.length own);
      let rec disjoint = function
        | (_, _, ended) :: ((_, started, _) :: _ as rest) ->
            Alcotest.(check bool)
              (Printf.sprintf "two messages of %s were in the subscriber at once" uri)
              true
              (Mtime.compare ended started <= 0);
            disjoint rest
        | _ -> ()
      in
      disjoint own)
    [ first; second ];
  Alcotest.(check bool)
    (Printf.sprintf "the two URIs did not run side by side: %.3fs for 8 pauses of %.3fs"
       elapsed pause)
    true
    (elapsed < pause *. 7.0)

(* Every keyed URI lands in exactly one of the slots, including the half
   whose hashtext is negative, and a dispatcher with no identity drains them
   all, one slot per call. *)
let test_slots_share_the_uris_without_gaps_or_overlap env uri () =
  with_fixture ~name:"workers" ~slots:3 env uri @@ fun f ->
  publish f
    (List.init 40 (fun i ->
         message (Printf.sprintf "kafka://orders/order-%d" (i + 1)) (i + 1)));
  let seen, subscriber = collector () in
  let selection = Selection.uri broker "kafka://orders" in
  let rec drain batches =
    if dispatch f selection subscriber then drain (batches + 1) else batches
  in
  let batches = drain 0 in
  Alcotest.(check (list int))
    "each once"
    (List.init 40 (fun i -> i + 1))
    (List.sort compare (seen ()));
  Alcotest.(check int) "one batch per slot with work" 3 batches;
  let positions = positions f selection in
  Alcotest.(check int) "a position per slot" 3 (List.length positions);
  Alcotest.(check bool)
    "all moved" true
    (List.for_all (fun (p : Position.t) -> p.offset > 0) positions)

(* [run] dispatches until told to stop, and stops between batches; two loops
   share four slots through the locks alone. *)
let test_run_dispatches_until_shutdown env uri () =
  with_fixture ~name:"run" ~slots:4 ~trace:false env uri @@ fun f ->
  publish f
    (List.init 6 (fun i ->
         message (Printf.sprintf "kafka://orders/order-%d" (i + 1)) (i + 1)));
  let seen = ref [] in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber (message : Message.t) =
    seen := id_of message :: !seen;
    if List.length !seen = 6 then ignore (Eio.Promise.try_resolve resolve_done ());
    Ok ()
  in
  let loops = { Loops.default with concurrency = 2; poll_interval = 0.02 } in
  unwrap "run"
    (within f 10.0 (fun () ->
         Outbox.run f.outbox ~clock:f.clock ~loops ~shutdown:all_done broker subscriber));
  Alcotest.(check (list int)) "all of them" [ 1; 2; 3; 4; 5; 6 ] (List.sort compare !seen)

(* A subscriber that fails does not stop [run]: the batch rolls back, the loop
   waits and delivers it again (ADR-0009). *)
let test_run_outlives_a_failing_subscriber env uri () =
  with_fixture ~name:"flaky_run" env uri @@ fun f ->
  publish f (List.init 3 (fun i -> message "kafka://orders" (i + 1)));
  let seen = ref [] in
  let calls = ref 0 in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber (message : Message.t) =
    let call = !calls in
    incr calls;
    if call < 2 then Error (Printf.sprintf "the broker is away, call %d" call)
    else begin
      seen := id_of message :: !seen;
      if List.length !seen = 3 then ignore (Eio.Promise.try_resolve resolve_done ());
      Ok ()
    end
  in
  let loops = { Loops.concurrency = 1; poll_interval = 0.02; max_pause = 0.05 } in
  unwrap "run"
    (within f 10.0 (fun () ->
         Outbox.run f.outbox ~clock:f.clock ~loops ~shutdown:all_done broker subscriber));
  Alcotest.(check (list int)) "delivered after the failures" [ 1; 2; 3 ] (List.rev !seen)

(* Two processes set the same tables up at once: both succeed, and the cut is
   written once. *)
let test_two_setups_at_once_agree env uri () =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions = connect_pool ~sw ~stdenv uri in
  let outbox_table = "outbox_twice" and offsets_table = "outbox_twice_offsets" in
  let build () =
    Outbox.create
      ~outbox_table:(Identifier.of_string_exn outbox_table)
      ~offsets_table:(Identifier.of_string_exn offsets_table)
      ~slots:2 sessions
  in
  let first : unit Outbox.t = build () and second : unit Outbox.t = build () in
  let session body = unwrap "session" (Pool.session sessions ~lift body) in
  session (fun s ->
      exec s
        (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_meta, %s" outbox_table outbox_table
           offsets_table);
      Ok ());
  let a, b =
    Eio.Fiber.pair
      (fun () -> Pool.session sessions ~lift (fun s -> Outbox.setup first s))
      (fun () -> Pool.session sessions ~lift (fun s -> Outbox.setup second s))
  in
  unwrap "first setup" a;
  unwrap "second setup" b;
  let cuts =
    session (fun s ->
        Ok (find_int s (Printf.sprintf "SELECT count(*) FROM %s_meta" outbox_table)))
  in
  Alcotest.(check int) "one cut" 1 cuts

(* Records what the outbox reports, in the words of the protocol model. *)
type recorder = {
  events : string list ref;
  receipts : Observer.receipt list ref;
  fetches : (int * int option) list ref;
  acked : Position.t list ref;
}

let recorder () : recorder * string Observer.t =
  let r = { events = ref []; receipts = ref []; fetches = ref []; acked = ref [] } in
  let note event = r.events := event :: !(r.events) in
  ( r,
    {
      on_published =
        (fun event ->
          r.receipts := event.receipt :: !(r.receipts);
          note (Printf.sprintf "published %d" (id_of event.message)));
      on_fetched =
        (fun event ->
          let newest =
            List.fold_left
              (fun acc (m : Message.t) ->
                match (acc, m.transaction_id) with
                | None, id -> id
                | Some a, Some b -> Some (max a b)
                | acc, None -> acc)
              None event.messages
          in
          r.fetches := (event.horizon, newest) :: !(r.fetches);
          note
            (Printf.sprintf "fetched [%s]"
               (String.concat ", "
                  (List.map (fun m -> string_of_int (id_of m)) event.messages))));
      on_handled =
        (fun event ->
          note
            (Printf.sprintf "handled %d %s" (id_of event.message)
               (if Result.is_ok event.outcome then "ok" else "failed")));
      on_acked =
        (fun event ->
          r.acked := event.position :: !(r.acked);
          note "acked");
      on_dispatched =
        (fun event ->
          note
            (match event.outcome with
            | Ok true -> "dispatched a batch"
            | Ok false -> "dispatched nothing"
            | Error _ -> "rolled back"));
    } )

(* The observer sees the protocol the model in verify/tla/Outbox.tla is
   written in: publish with the writing transaction's id, fetch with the
   visibility horizon, one handling per message, the acknowledgement, the
   close of the transaction; a rollback when the subscriber failed, and the
   batch again afterwards. *)
let test_the_observer_sees_the_protocol env uri () =
  let r, observer = recorder () in
  with_fixture ~name:"observed" ~observer env uri @@ fun f ->
  publish f [ message "kafka://orders" 1; message "kafka://orders" 2 ];
  let _, subscriber = collector () in
  Alcotest.(check bool) "a batch" true (dispatch f broker subscriber);
  (* The subscriber fails once: the batch is rolled back and comes again. *)
  publish f [ message "kafka://orders" 3 ];
  let failed_once = ref false in
  let flaky _ =
    if !failed_once then Ok ()
    else begin
      failed_once := true;
      Error "the first attempt fails on purpose"
    end
  in
  Alcotest.(check bool)
    "rolled back" true
    (Result.is_error (Outbox.dispatch f.outbox broker flaky));
  Alcotest.(check bool) "again" true (dispatch f broker flaky);
  Alcotest.(check (list string))
    "the protocol"
    [
      "published 1";
      "published 2";
      "fetched [1, 2]";
      "handled 1 ok";
      "handled 2 ok";
      "acked";
      "dispatched a batch";
      "published 3";
      "fetched [3]";
      "handled 3 failed";
      "rolled back";
      "fetched [3]";
      "handled 3 ok";
      "acked";
      "dispatched a batch";
    ]
    (List.rev !(r.events));
  (* Every batch was read below a horizon past its newest transaction: the
     visibility rule, as the dispatcher saw it. *)
  let fetches = List.rev !(r.fetches) in
  Alcotest.(check int) "three fetches" 3 (List.length fetches);
  Alcotest.(check bool)
    "each below the horizon" true
    (List.for_all
       (fun (horizon, newest) ->
         match newest with Some newest -> newest < horizon | None -> false)
       fetches);
  (* The receipt names the row the dispatcher later acknowledges. *)
  let receipts = List.rev !(r.receipts) in
  Alcotest.(check int) "three receipts" 3 (List.length receipts);
  let rec ascending = function
    | (a : Observer.receipt) :: (b :: _ as rest) ->
        a.position < b.position && ascending rest
    | _ -> true
  in
  Alcotest.(check bool) "positions ascend" true (ascending receipts);
  let of_receipt (r : Observer.receipt) =
    { Position.transaction_id = r.transaction_id; offset = r.position }
  in
  Alcotest.(check (list position))
    "acked what was published"
    [ of_receipt (List.nth receipts 1); of_receipt (List.nth receipts 2) ]
    (List.rev !(r.acked))

(* Whether [sub] occurs in [text]. *)
let contains ~sub text =
  let n = String.length sub and m = String.length text in
  let rec at i = i + n <= m && (String.sub text i n = sub || at (i + 1)) in
  at 0

(* What the library logged while the body ran: source, level and text. *)
let logged body =
  let seen = ref [] in
  let report src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun text ->
            seen := (Logs.Src.name src, level, text) :: !seen;
            over ();
            k ())
          fmt)
  in
  Logs.set_reporter { Logs.report };
  Logs.set_level (Some Logs.Warning);
  Fun.protect
    ~finally:(fun () -> Logs.set_reporter Logs.nop_reporter)
    (fun () ->
      body ();
      List.rev !seen)

let said logs ~src ~level ~text =
  List.exists (fun (s, l, t) -> s = src && l = level && contains ~sub:text t) logs

(* A loop that waits after a failure says so on the outbox's own source, so
   that a broker away is not silent when no observer is attached. *)
let test_a_loop_that_waits_after_a_failure_says_so env uri () =
  with_fixture ~name:"logged" ~trace:false env uri @@ fun f ->
  publish f [ message "kafka://orders" 1 ];
  let calls = ref 0 in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber _ =
    incr calls;
    if !calls = 1 then Error "the broker is away"
    else begin
      ignore (Eio.Promise.try_resolve resolve_done ());
      Ok ()
    end
  in
  let loops = { Loops.concurrency = 1; poll_interval = 0.02; max_pause = 0.05 } in
  let logs =
    logged (fun () ->
        unwrap "run"
          (within f 10.0 (fun () ->
               Outbox.run f.outbox ~clock:f.clock ~loops ~shutdown:all_done broker
                 subscriber)))
  in
  Alcotest.(check bool)
    "the wait is a warning" true
    (said logs ~src:"ascetic_ddd.outbox" ~level:Logs.Warning
       ~text:"a loop failed, waiting")

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (test env uri) in
  [
    case "published messages are dispatched in order once"
      test_published_messages_are_dispatched_in_order_once;
    case "every consumer group gets every message"
      test_every_consumer_group_gets_every_message;
    case "the position follows the last acknowledged message and can be moved"
      test_the_position_follows_the_last_acknowledged_message_and_can_be_moved;
    case "a uri selects itself and everything under it"
      test_a_uri_selects_itself_and_everything_under_it;
    case "a batch is at most batch_size messages"
      test_a_batch_is_at_most_batch_size_messages;
    case "nothing is dispatched past an open transaction"
      test_nothing_is_dispatched_past_an_open_transaction;
    case "a failing subscriber rolls the batch back"
      test_a_failing_subscriber_rolls_the_batch_back;
    case "a message_id is published once" test_a_message_id_is_published_once;
    case "two dispatchers of one group do not overlap"
      test_two_dispatchers_of_one_group_do_not_overlap;
    case "two slots are dispatched at once" test_two_slots_are_dispatched_at_once;
    case "messages of one uri are never dispatched at once"
      test_messages_of_one_uri_are_never_dispatched_at_once;
    case "slots share the uris without gaps or overlap"
      test_slots_share_the_uris_without_gaps_or_overlap;
    case "run dispatches until shutdown" test_run_dispatches_until_shutdown;
    case "run outlives a failing subscriber" test_run_outlives_a_failing_subscriber;
    case "two setups at once agree" test_two_setups_at_once_agree;
    case "the observer sees the protocol" test_the_observer_sees_the_protocol;
    case "a loop that waits after a failure says so"
      test_a_loop_that_waits_after_a_failure_says_so;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] outbox integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Pg_outbox" [ ("integration", cases env uri) ]
