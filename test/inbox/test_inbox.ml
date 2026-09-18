(** Integration tests for the PostgreSQL inbox. They need a live database, and are skipped
    when [TEST_DATABASE_URL] is not set:

    {v
      export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
      dune test test/inbox
    v}

    With [ASCETIC_DDD_TRACE_DIR] set, every test with one dispatcher at a time writes what
    its inbox reported as [inbox-<name>.jsonl] into that directory, one event per line,
    for validation against the protocol model: see [verify/tla/README.md]. A test with
    several dispatchers at once records nothing: the order its events are logged in is not
    the order of their commits; it asserts its outcome and the table's state instead. *)

module Inbox = Ascetic_inbox.Pg_inbox
module Message = Ascetic_inbox.Inbox_message
module Dependency = Ascetic_inbox.Causal_dependency
module Outcome = Ascetic_inbox.Outcome
module Retries = Ascetic_inbox.Retries
module Loops = Ascetic_inbox.Loops
module Failure = Ascetic_inbox.Failure
module Error = Ascetic_inbox.Inbox_error
module Observer = Ascetic_inbox.Inbox_observer
module Partition_key = Ascetic_inbox.Partition_key
module Snapshot = Ascetic_inbox.Snapshot
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Trace_file = Ascetic_trace.Trace_file
module Json_trace = Ascetic_trace.Json_trace

let lift e = Error.Session e

let unwrap what = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %s" what (Error.to_string error)

let outcome = Alcotest.testable Outcome.pp Outcome.equal

(* ------------------------------------------------------------------------ *)
(* Plain SQL through a session's connection                                   *)

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> Ok ()
  | Error err -> Error (Caqti_error.show err)

let exec_exn session sql =
  match exec session sql with
  | Ok () -> ()
  | Error reason -> Alcotest.failf "%s: %s" sql reason

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
  inbox : Inbox.t;
  table : string;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

let with_session f body = unwrap "session" (Pool.session f.sessions ~lift body)

let connect_pool ~sw ~stdenv uri =
  let pool_config = Caqti_pool_config.create ~max_size:8 () in
  match Caqti_eio_unix.connect_pool ~pool_config ~sw ~stdenv uri with
  | Ok pool -> Pool.of_pool pool
  | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err

(* A table of its own per test, cut by stream, the cut that keeps causal
   order; one slot unless said otherwise. The number of slots and the key are
   read at setup, so they are set before the table exists. [~trace:false] is
   for a test that runs several dispatchers at once, which records no trace. *)
let with_fixture ?(slots = 1) ?(observer = Observer.none) ?(trace = true) ?retries
    ?max_wait ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions = connect_pool ~sw ~stdenv uri in
  let trace =
    if trace then Trace_file.from_env ("inbox-" ^ name) else Trace_file.off ()
  in
  let table = "inbox_" ^ name in
  let sequence = table ^ "_seq" in
  let inbox =
    Inbox.create
      ~observer:
        (Observer.all [ observer; Json_trace.inbox_observer (Trace_file.recorder trace) ])
      ~table:(Identifier.of_string_exn table)
      ~sequence:(Identifier.of_string_exn sequence)
      ~partition:Partition_key.by_stream ~slots ?retries ?max_wait sessions
  in
  let f =
    {
      sessions;
      inbox;
      table;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
    }
  in
  with_session f (fun session ->
      exec_exn session
        (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_meta, %s_slots" table table table);
      exec_exn session (Printf.sprintf "DROP SEQUENCE IF EXISTS %s" sequence);
      Inbox.setup inbox session);
  Fun.protect ~finally:(fun () -> Trace_file.close trace) (fun () -> body f)

let sleep f seconds = Eio.Time.Mono.sleep f.clock seconds

let elapsed f body =
  let started = Eio.Time.Mono.now f.clock in
  let value = body () in
  (value, Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now f.clock)) /. 1e9)

let within f seconds body =
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds f.clock seconds) body

(* One dispatch call that must not fail on the database. *)
let dispatch f subscriber = unwrap "dispatch" (Inbox.dispatch f.inbox subscriber)

let publish f messages =
  List.iter (fun m -> unwrap "publish" (Inbox.publish f.inbox m)) messages

let parked_messages f = with_session f (fun session -> Inbox.parked f.inbox session)

let unpark f message =
  with_session f (fun session -> Inbox.unpark f.inbox session message)

let resolve f message =
  with_session f (fun session -> Inbox.resolve f.inbox session message)

let count f sql = with_session f (fun session -> Ok (find_int session sql))
let rows f = count f (Printf.sprintf "SELECT count(*) FROM %s" f.table)

let processed f =
  count f
    (Printf.sprintf "SELECT count(*) FROM %s WHERE processed_position IS NOT NULL" f.table)

(* A message of a stream at a position, with a unique message id. *)
let event = ref 0

let message stream position =
  incr event;
  Message.with_metadata
    (Message.make ~tenant_id:"tenant-1" ~stream_type:"orders.Order"
       ~stream_id:(`Assoc [ ("id", `String stream) ])
       ~stream_position:position
       ~uri:(Printf.sprintf "kafka://orders/%s" stream)
       ~payload:(Printf.sprintf "%s@%d" stream position))
    (`Assoc
       [ ("message_id", `String (Printf.sprintf "00000000-0000-4000-8000-%012x" !event)) ])

let stream_of (message : Message.t) =
  match message.stream_id with
  | `Assoc [ ("id", `String stream) ] -> stream
  | other -> Alcotest.failf "not a test stream: %s" (Yojson.Safe.to_string other)

let label (message : Message.t) =
  Printf.sprintf "%s@%d" (stream_of message) message.stream_position

let parked f = List.map label (parked_messages f)

(* A subscriber that collects what it was given. *)
let collector () =
  let seen = ref [] in
  ( (fun () -> List.rev !seen),
    fun _ (message : Message.t) ->
      seen := label message :: !seen;
      Ok () )

(* A subscriber that fails on one message and succeeds on the others. *)
let failing_on poison =
  let seen = ref [] in
  ( (fun () -> List.rev !seen),
    fun _ (message : Message.t) ->
      if label message = poison then Error (Failure.transient (poison ^ " is poison"))
      else begin
        seen := label message :: !seen;
        Ok ()
      end )

(* A subscriber that fails the first time it is called and succeeds after. *)
let failing_once () =
  let seen = ref [] in
  let calls = ref 0 in
  ( (fun () -> List.rev !seen),
    fun _ (message : Message.t) ->
      let call = !calls in
      incr calls;
      if call = 0 then Error (Failure.transient "the first attempt fails on purpose")
      else begin
        seen := label message :: !seen;
        Ok ()
      end )

(* Two stream ids that land in different slots, by the cut's own expression,
   for tests that need two slots busy at once. *)
let streams_in_different_slots f =
  let slots = Inbox.slots f.inbox in
  let sql =
    Printf.sprintf
      "SELECT ((hashtext(%s) & 2147483647) %% $2)::int FROM (SELECT 'tenant-1'::varchar \
       AS tenant_id, 'orders.Order'::varchar AS stream_type, $1::jsonb AS stream_id) AS \
       r"
      Partition_key.by_stream.sql_expression
  in
  let slot_of stream =
    with_session f (fun session ->
        let module C = (val Session.connection session) in
        let open Caqti_request.Infix in
        let request = (Caqti_type.(t2 string int) ->! Caqti_type.int) sql in
        match
          C.find request (Yojson.Safe.to_string (`Assoc [ ("id", `String stream) ]), slots)
        with
        | Ok slot -> Ok slot
        | Error err -> Alcotest.failf "slot of %s: %a" stream Caqti_error.pp err)
  in
  let first = "s1" in
  let slot = slot_of first in
  let rec other i =
    let candidate = Printf.sprintf "s%d" i in
    if slot_of candidate <> slot then candidate else other (i + 1)
  in
  (first, other 2)

(* Makes the slot of the stream the least recently served, so that the next
   take picks it. *)
let serve_first f stream =
  with_session f (fun session ->
      exec session
        (Printf.sprintf
           "UPDATE %s_slots SET served_at = served_at - interval '1 hour' WHERE slot = \
            (SELECT slot FROM %s WHERE stream_id = '%s'::jsonb LIMIT 1)"
           f.table f.table
           (Yojson.Safe.to_string (`Assoc [ ("id", `String stream) ])))
      |> Result.map_error (fun reason -> Error.Malformed reason))

(* Rows waiting for a dependency that is processed: wakes lost. Must be none
   whatever the interleaving (ADR-0008). *)
let lost_wakes f =
  count f
    (Printf.sprintf
       "SELECT count(*) FROM %s w WHERE w.waiting_for IS NOT NULL AND EXISTS (SELECT 1 \
        FROM %s d WHERE d.tenant_id = w.waiting_for->>'tenant_id' AND d.stream_type = \
        w.waiting_for->>'stream_type' AND d.stream_id = w.waiting_for->'stream_id' AND \
        d.stream_position = (w.waiting_for->>'stream_position')::int AND \
        d.processed_position IS NOT NULL)"
       f.table f.table)

(* Whether [sub] occurs in [text]. *)
let contains ~sub text =
  let n = String.length sub and m = String.length text in
  let rec at i = i + n <= m && (String.sub text i n = sub || at (i + 1)) in
  at 0

let rec drain f subscriber =
  if dispatch f subscriber <> Outcome.Nothing then drain f subscriber

(* ------------------------------------------------------------------------ *)
(* The tests                                                                  *)

let test_a_received_message_is_processed_once_in_its_transaction env uri () =
  with_fixture ~name:"roundtrip" env uri @@ fun f ->
  publish f [ message "a" 1 ];
  let seen = ref [] in
  (* The subscriber sees its message through the transaction it is given,
     still unprocessed. *)
  let subscriber tx (message : Message.t) =
    let unprocessed =
      find_bool tx
        (Printf.sprintf
           "SELECT processed_position IS NULL FROM %s WHERE received_position = %d"
           f.table
           (Option.get message.received_position))
    in
    Alcotest.(check bool) "unprocessed while handled" true unprocessed;
    seen := label message :: !seen;
    Ok ()
  in
  Alcotest.check outcome "processed" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "nothing more" Outcome.Nothing (dispatch f subscriber);
  Alcotest.(check (list string)) "once" [ "a@1" ] (List.rev !seen);
  Alcotest.(check int) "marked" 1 (processed f)

let test_receiving_the_same_message_twice_stores_it_once env uri () =
  with_fixture ~name:"idempotent" env uri @@ fun f ->
  let once = message "a" 1 in
  let again = { once with payload = "a duplicate with a different payload" } in
  publish f [ once; again ];
  Alcotest.(check int) "one row" 1 (rows f)

let test_a_message_id_is_stored_once env uri () =
  with_fixture ~name:"message_id" env uri @@ fun f ->
  let first = message "a" 1 in
  let other = { (message "a" 2) with metadata = first.metadata } in
  publish f [ first ];
  match Inbox.publish f.inbox other with
  | Error (Error.Database reason) ->
      Alcotest.(check bool) "a defect" false reason.transient
  | Error error -> Alcotest.failf "another error: %s" (Error.to_string error)
  | Ok () -> Alcotest.fail "the duplicate id was accepted"

let test_messages_are_processed_in_order_of_arrival env uri () =
  with_fixture ~name:"order" env uri @@ fun f ->
  publish f [ message "b" 1; message "a" 1; message "a" 2 ];
  let seen, subscriber = collector () in
  drain f subscriber;
  Alcotest.(check (list string)) "arrival order" [ "b@1"; "a@1"; "a@2" ] (seen ())

(* A message that depends on one not yet processed waits, even though it
   arrived first; a message that depends on nothing goes ahead of it. *)
let test_a_message_waits_for_its_causal_dependencies env uri () =
  with_fixture ~name:"causal" env uri @@ fun f ->
  let cause = message "user" 5 in
  let dependent = Message.depending_on (message "order" 1) [ Message.identity cause ] in
  let unrelated = message "other" 1 in
  publish f [ dependent; unrelated ] (* the cause has not arrived *);
  let seen, subscriber = collector () in
  Alcotest.check outcome "the dependent" Outcome.Set_aside (dispatch f subscriber);
  Alcotest.check outcome "the unrelated" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "nothing more" Outcome.Nothing (dispatch f subscriber);
  Alcotest.(check (list string)) "the dependent waits" [ "other@1" ] (seen ());
  publish f [ cause ];
  drain f subscriber;
  Alcotest.(check (list string))
    "the cause, then the dependent"
    [ "other@1"; "user@5"; "order@1" ]
    (seen ())

let test_a_failing_subscriber_leaves_the_message_unprocessed env uri () =
  with_fixture ~name:"failure" env uri @@ fun f ->
  publish f [ message "a" 1 ];
  let failing _ _ = Error (Failure.transient "handler down") in
  Alcotest.check outcome "failed"
    (Outcome.Failed { attempts = 1; parked = false })
    (dispatch f failing);
  Alcotest.(check int) "unprocessed" 0 (processed f);
  let seen, subscriber = collector () in
  Alcotest.check outcome "processed" Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string)) "the same message" [ "a@1" ] (seen ())

(* After max_attempts failures the message is parked, in place, and the
   partition behind it flows (ADR-0004). *)
let test_a_poison_message_is_parked_after_its_attempts_and_the_partition_flows env uri ()
    =
  with_fixture ~name:"parking" ~retries:(Retries.up_to 2) env uri @@ fun f ->
  publish f [ message "a" 1; message "b" 1 ];
  let seen, subscriber = failing_on "a@1" in
  Alcotest.check outcome "first attempt"
    (Outcome.Failed { attempts = 1; parked = false })
    (dispatch f subscriber);
  Alcotest.check outcome "second, parked"
    (Outcome.Failed { attempts = 2; parked = true })
    (dispatch f subscriber);
  Alcotest.check outcome "the partition flows" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "nothing more" Outcome.Nothing (dispatch f subscriber);
  Alcotest.(check (list string)) "b went through" [ "b@1" ] (seen ());
  Alcotest.(check (list string)) "a is parked" [ "a@1" ] (parked f);
  Alcotest.(check int) "a parked row stays in the table" 2 (rows f);
  let parked = List.hd (parked_messages f) in
  Alcotest.(check int) "attempts" 2 parked.attempts;
  Alcotest.(check (option string)) "last error" (Some "a@1 is poison") parked.last_error

(* While a failed message waits for its backoff it holds its partition: the
   message behind it is not processed first. *)
let test_a_message_in_backoff_holds_its_partition env uri () =
  with_fixture ~name:"backoff"
    ~retries:(Retries.with_backoff Retries.unlimited (fun _ -> 0.3))
    env uri
  @@ fun f ->
  publish f [ message "a" 1; message "b" 1 ];
  let seen, subscriber = failing_once () in
  Alcotest.check outcome "failed"
    (Outcome.Failed { attempts = 1; parked = false })
    (dispatch f subscriber);
  Alcotest.check outcome "b@1 waits behind a@1" Outcome.Nothing (dispatch f subscriber);
  sleep f 0.35;
  Alcotest.check outcome "a@1 again" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "then b@1" Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string)) "in order" [ "a@1"; "b@1" ] (seen ())

let test_an_unparked_message_is_tried_again env uri () =
  with_fixture ~name:"unpark" ~retries:(Retries.up_to 1) env uri @@ fun f ->
  let poison = message "a" 1 in
  publish f [ poison ];
  let seen, subscriber = failing_once () in
  Alcotest.check outcome "parked at once"
    (Outcome.Failed { attempts = 1; parked = true })
    (dispatch f subscriber);
  Alcotest.check outcome "nothing" Outcome.Nothing (dispatch f subscriber);
  Alcotest.(check bool) "was parked" true (unpark f poison);
  Alcotest.check outcome "processed" Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string)) "seen" [ "a@1" ] (seen ());
  Alcotest.(check (list string)) "nothing parked" [] (parked f)

(* Resolving a parked message marks it processed without its effects, and
   what depended on it goes ahead. *)
let test_a_resolved_message_releases_its_dependents env uri () =
  with_fixture ~name:"resolve" ~retries:(Retries.up_to 1) env uri @@ fun f ->
  let cause = message "a" 1 in
  let dependent = Message.depending_on (message "b" 1) [ Message.identity cause ] in
  publish f [ cause; dependent ];
  let seen, subscriber = failing_on "a@1" in
  Alcotest.check outcome "parked"
    (Outcome.Failed { attempts = 1; parked = true })
    (dispatch f subscriber);
  Alcotest.check outcome "b@1 waits" Outcome.Set_aside (dispatch f subscriber);
  Alcotest.(check bool) "resolved" true (resolve f cause);
  Alcotest.check outcome "b@1 was woken" Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string)) "a@1 never had its effects" [ "b@1" ] (seen ());
  Alcotest.(check int) "both marked" 2 (processed f);
  Alcotest.(check (list string)) "nothing parked" [] (parked f)

(* Two dispatchers at once on one slot: the slot's row is locked by the first,
   SKIP LOCKED passes the second by, and nothing is processed twice or out of
   order. *)
let test_a_held_slot_is_passed_by env uri () =
  with_fixture ~name:"skip_locked" ~trace:false env uri @@ fun f ->
  publish f [ message "a" 1; message "b" 1 ];
  let seen = ref [] in
  let slow _ (message : Message.t) =
    sleep f 0.1;
    seen := label message :: !seen;
    Ok ()
  in
  let a, b = Eio.Fiber.pair (fun () -> dispatch f slow) (fun () -> dispatch f slow) in
  let outcomes = List.sort compare [ a; b ] in
  Alcotest.(check (list outcome))
    "one worked, one passed by"
    [ Outcome.Nothing; Outcome.Processed ]
    outcomes;
  Alcotest.(check (list string)) "the head only" [ "a@1" ] (List.rev !seen);
  Alcotest.check outcome "then the next" Outcome.Processed (dispatch f slow);
  Alcotest.(check (list string)) "in order" [ "a@1"; "b@1" ] (List.rev !seen)

(* Every stream lands in exactly one of the slots, including the half whose
   hashtext is negative, and a dispatcher without identity drains them all. *)
let test_slots_share_the_streams_without_gaps_or_overlap env uri () =
  with_fixture ~name:"slots" ~slots:3 env uri @@ fun f ->
  publish f (List.init 40 (fun i -> message (Printf.sprintf "stream-%d" (i + 1)) 1));
  let seen, subscriber = collector () in
  drain f subscriber;
  let expected =
    List.sort compare (List.init 40 (fun i -> Printf.sprintf "stream-%d@1" (i + 1)))
  in
  Alcotest.(check (list string)) "each once" expected (List.sort compare (seen ()))

let test_run_processes_until_shutdown env uri () =
  with_fixture ~name:"run" ~slots:4 ~trace:false env uri @@ fun f ->
  publish f (List.init 6 (fun i -> message (Printf.sprintf "s%d" (i + 1)) 1));
  let seen = ref [] in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber _ (message : Message.t) =
    seen := label message :: !seen;
    if List.length !seen = 6 then ignore (Eio.Promise.try_resolve resolve_done ());
    Ok ()
  in
  let loops = { Loops.default with concurrency = 2; poll_interval = 0.02 } in
  unwrap "run"
    (within f 10.0 (fun () ->
         Inbox.run f.inbox ~clock:f.clock ~loops ~shutdown:all_done subscriber));
  Alcotest.(check int) "six seen" 6 (List.length !seen);
  Alcotest.(check int) "six marked" 6 (processed f)

(* Two slots, two dispatchers, a subscriber that takes its time: the locks are
   per slot, so the two run side by side, not one after the other. *)
let test_two_slots_are_worked_at_once env uri () =
  with_fixture ~name:"parallel" ~slots:2 ~trace:false env uri @@ fun f ->
  let first, second = streams_in_different_slots f in
  publish f [ message first 1; message second 1 ];
  let slow _ _ =
    sleep f 0.3;
    Ok ()
  in
  let (a, b), elapsed =
    elapsed f (fun () ->
        Eio.Fiber.pair (fun () -> dispatch f slow) (fun () -> dispatch f slow))
  in
  Alcotest.(check (pair outcome outcome))
    "both processed"
    (Outcome.Processed, Outcome.Processed)
    (a, b);
  Alcotest.(check bool)
    (Printf.sprintf "two slots took %.3fs: one after the other" elapsed)
    true (elapsed < 0.55)

(* Tells the test when a row was set aside to wait: the moment the races
   below are opened. *)
let on_waiting () =
  let waited, resolve = Eio.Promise.create () in
  ( waited,
    {
      Observer.none with
      on_waiting = (fun _ -> ignore (Eio.Promise.try_resolve resolve ()));
    } )

(* A dispatcher that finds a dependency unprocessed sets its row aside and
   commits, under a lock on the dependency's identity that the mark of the
   dependency takes too. However the two interleave, the row is woken
   (ADR-0008). Here the dependency has not arrived when the row is set aside,
   and arrives and is processed in another slot right after. *)
let test_a_wait_set_while_its_dependency_arrives_and_is_marked_elsewhere_is_woken env uri
    () =
  let waited, observer = on_waiting () in
  with_fixture ~name:"lost_wake" ~observer ~slots:2 ~trace:false env uri @@ fun f ->
  let dependent, dependency = streams_in_different_slots f in
  let d = message dependency 1 in
  let m = Message.depending_on (message dependent 1) [ Message.identity d ] in
  let behind = message dependent 2 in
  publish f [ m; behind ];
  let seen, subscriber = collector () in
  let a, b =
    Eio.Fiber.pair
      (fun () -> dispatch f subscriber)
      (fun () ->
        (* m is being set aside; the dependency arrives and is processed in
           its own slot, racing the commit of the wait *)
        Eio.Promise.await waited;
        publish f [ d ];
        dispatch f subscriber)
  in
  Alcotest.(check (pair outcome outcome))
    "set aside, processed"
    (Outcome.Set_aside, Outcome.Processed)
    (a, b);
  Alcotest.check outcome "the dependent message was woken by the mark of its dependency"
    Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string))
    "in causal order"
    [ dependency ^ "@1"; dependent ^ "@1" ]
    (seen ());
  Alcotest.(check int) "no wake lost" 0 (lost_wakes f)

(* The same race with the dependency already in the table, unprocessed, in
   its own slot: the dependent's slot is served first, the dependency's right
   after. *)
let test_a_wait_set_while_its_dependency_is_being_marked_elsewhere_is_woken env uri () =
  let waited, observer = on_waiting () in
  with_fixture ~name:"lost_wake_in_flight" ~observer ~slots:2 ~trace:false env uri
  @@ fun f ->
  let dependent, dependency = streams_in_different_slots f in
  let d = message dependency 1 in
  let m = Message.depending_on (message dependent 1) [ Message.identity d ] in
  let behind = message dependent 2 in
  publish f [ d; m; behind ];
  (* The dependent's slot is the least recently served, so it is taken first. *)
  serve_first f dependent;
  let seen, subscriber = collector () in
  let a, b =
    Eio.Fiber.pair
      (fun () -> dispatch f subscriber)
      (fun () ->
        Eio.Promise.await waited;
        dispatch f subscriber)
  in
  Alcotest.(check (pair outcome outcome))
    "set aside, processed"
    (Outcome.Set_aside, Outcome.Processed)
    (a, b);
  Alcotest.check outcome "the dependent message was woken by the mark of its dependency"
    Outcome.Processed (dispatch f subscriber);
  Alcotest.(check (list string))
    "in causal order"
    [ dependency ^ "@1"; dependent ^ "@1" ]
    (seen ());
  Alcotest.(check int) "no wake lost" 0 (lost_wakes f)

(* Several loops over several slots; messages with dependencies across slots,
   published in random order while the loops run, so dependencies arrive
   before and after their dependents; one poison message, parked and resolved
   by hand. Everything ends processed, no lock cycle stops the loops, and no
   row waits for a processed dependency: no wake was lost (ADR-0008). *)
let test_loops_over_slots_with_dependencies_across_them_lose_no_wake env uri () =
  let n = 120 in
  with_fixture ~name:"stress" ~slots:4 ~trace:false ~retries:(Retries.up_to 1)
    ~max_wait:60.0 env uri
  @@ fun f ->
  (* A fixed pseudo-random sequence, so the run is the same every time. *)
  let seed = ref 0x9E3779B97F4A7C15L in
  let next () =
    seed := Int64.add (Int64.mul !seed 6364136223846793005L) 1442695040888963407L;
    Int64.to_int (Int64.shift_right_logical !seed 33)
  in
  let messages = Array.make n (message "s0" 1) in
  for i = 0 to n - 1 do
    let m = message (Printf.sprintf "s%d" (i mod 7)) ((i / 7) + 1) in
    let m =
      if i > 0 && next () mod 2 = 0 then
        Message.depending_on m [ Message.identity messages.(next () mod i) ]
      else m
    in
    messages.(i) <- m
  done;
  let order = Array.init n Fun.id in
  for i = n - 1 downto 1 do
    let j = next () mod (i + 1) in
    let swapped = order.(i) in
    order.(i) <- order.(j);
    order.(j) <- swapped
  done;
  let poison = label messages.(n / 3) in
  let seen, subscriber = failing_on poison in
  let subscriber session (message : Message.t) =
    sleep f
      (float_of_int (Option.value message.received_position ~default:0 mod 3) /. 1000.0);
    subscriber session message
  in
  let all_done, resolve_done = Eio.Promise.create () in
  let run_finished, resolve_run_finished = Eio.Promise.create () in
  let loops = { Loops.default with concurrency = 3; poll_interval = 0.005 } in
  let ran = ref (Ok ()) in
  let publishing () =
    Array.iter
      (fun i ->
        publish f [ messages.(i) ];
        sleep f 0.001)
      order
  in
  (* resolves the parked poison as an operator would, and stops the loops
     once everything is processed *)
  let rec operating () =
    if not (Eio.Promise.is_resolved run_finished) then begin
      sleep f 0.02;
      List.iter (fun parked -> ignore (resolve f parked)) (parked_messages f);
      if processed f = n then ignore (Eio.Promise.try_resolve resolve_done ())
      else operating ()
    end
  in
  let running () =
    Fun.protect
      ~finally:(fun () -> Eio.Promise.resolve resolve_run_finished ())
      (fun () ->
        ran :=
          within f 60.0 (fun () ->
              Inbox.run f.inbox ~clock:f.clock ~loops ~shutdown:all_done subscriber))
  in
  Eio.Fiber.all [ publishing; operating; running ];
  unwrap "no loop fails, in particular on a lock cycle" !ran;
  Alcotest.(check int) "everything processed" n (processed f);
  Alcotest.(check int) "no wake lost" 0 (lost_wakes f);
  Alcotest.(check int)
    "every message but the poison had its effects once" (n - 1)
    (List.length (seen ()));
  Alcotest.(check (list string)) "nothing parked" [] (parked f)

(* Two streams in two slots, four messages each, two loops, a subscriber that
   takes its time and notes when it ran: the messages of one stream are never
   in the subscriber at the same time, while the two streams are; the run is
   shorter than the sum of the pauses, so the check could have seen an
   overlap. *)
let test_messages_of_one_stream_are_never_processed_at_once env uri () =
  let pause = 0.04 in
  with_fixture ~name:"serial_stream" ~slots:2 ~trace:false env uri @@ fun f ->
  let first, second = streams_in_different_slots f in
  publish f
    (List.concat_map (fun i -> [ message first i; message second i ]) [ 1; 2; 3; 4 ]);
  let spans = ref [] in
  let all_done, resolve_done = Eio.Promise.create () in
  let subscriber _ (message : Message.t) =
    let started = Eio.Time.Mono.now f.clock in
    sleep f pause;
    spans := (stream_of message, started, Eio.Time.Mono.now f.clock) :: !spans;
    if List.length !spans = 8 then ignore (Eio.Promise.try_resolve resolve_done ());
    Ok ()
  in
  let loops = { Loops.default with concurrency = 2; poll_interval = 0.005 } in
  let ran, elapsed =
    elapsed f (fun () ->
        within f 10.0 (fun () ->
            Inbox.run f.inbox ~clock:f.clock ~loops ~shutdown:all_done subscriber))
  in
  unwrap "run" ran;
  List.iter
    (fun stream ->
      let own =
        List.sort
          (fun (_, a, _) (_, b, _) -> Mtime.compare a b)
          (List.filter (fun (s, _, _) -> s = stream) !spans)
      in
      Alcotest.(check int) "four runs" 4 (List.length own);
      let rec disjoint = function
        | (_, _, ended) :: ((_, started, _) :: _ as rest) ->
            Alcotest.(check bool)
              (Printf.sprintf "two messages of stream %s were in the subscriber at once"
                 stream)
              true
              (Mtime.compare ended started <= 0);
            disjoint rest
        | _ -> ()
      in
      disjoint own)
    [ first; second ];
  Alcotest.(check bool)
    (Printf.sprintf
       "the two streams did not run side by side: %.3fs for 8 pauses of %.3fs" elapsed
       pause)
    true
    (elapsed < pause *. 7.0)

(* A subscriber's verdict that no retry will succeed parks the message at
   once, attempts left or not, and the slot flows (ADR-0009). *)
let test_a_permanent_failure_parks_the_message_at_once env uri () =
  with_fixture ~name:"permanent" env uri @@ fun f ->
  publish f [ message "a" 1; message "b" 1 ];
  let seen, subscriber = collector () in
  let judging tx (message : Message.t) =
    if label message = "a@1" then Error (Failure.permanent "the payload cannot be read")
    else subscriber tx message
  in
  Alcotest.check outcome "parked at once"
    (Outcome.Failed { attempts = 1; parked = true })
    (dispatch f judging);
  Alcotest.check outcome "b@1 flows" Outcome.Processed (dispatch f judging);
  Alcotest.check outcome "nothing more" Outcome.Nothing (dispatch f judging);
  Alcotest.(check (list string)) "seen" [ "b@1" ] (seen ());
  Alcotest.(check (list string)) "parked" [ "a@1" ] (parked f);
  Alcotest.(check (option string))
    "the verdict recorded" (Some "the payload cannot be read")
    (List.hd (parked_messages f)).last_error

(* A loop that loses its connection, here the subscriber has the server
   terminate it, waits and goes on; the message comes back and is processed
   (ADR-0009). *)
let test_a_loop_outlives_a_lost_connection env uri () =
  with_fixture ~name:"lost_connection" env uri @@ fun f ->
  publish f [ message "a" 1 ];
  let seen, subscriber = collector () in
  let all_done, resolve_done = Eio.Promise.create () in
  let calls = ref 0 in
  let cutting tx (message : Message.t) =
    let first = !calls = 0 in
    incr calls;
    if first then
      (* the statement kills its own backend: an error of the moment *)
      Result.map_error Failure.transient
        (exec tx "SELECT pg_terminate_backend(pg_backend_pid())")
    else begin
      let handled = subscriber tx message in
      ignore (Eio.Promise.try_resolve resolve_done ());
      handled
    end
  in
  let loops = { Loops.concurrency = 1; poll_interval = 0.02; max_pause = 0.1 } in
  unwrap "the loop goes on after the lost connection"
    (within f 10.0 (fun () ->
         Inbox.run f.inbox ~clock:f.clock ~loops ~shutdown:all_done cutting));
  Alcotest.(check (list string)) "processed after all" [ "a@1" ] (seen ());
  Alcotest.(check int) "marked" 1 (processed f)

(* A defect, here the subscriber renames the table under the dispatcher, so
   the mark fails on an undefined table, stops the loops with the error; the
   transaction rolled back, the table is as it was (ADR-0009). *)
let test_a_defect_stops_the_loops env uri () =
  with_fixture ~name:"defect" env uri @@ fun f ->
  publish f [ message "a" 1 ];
  let breaking tx _ =
    Result.map_error Failure.transient
      (exec tx (Printf.sprintf "ALTER TABLE %s RENAME TO %s_gone" f.table f.table))
  in
  let never, _ = Eio.Promise.create () in
  let stopped = Inbox.run f.inbox ~clock:f.clock ~shutdown:never breaking in
  (match stopped with
  | Error (Error.Database reason) ->
      Alcotest.(check bool) "a defect" false reason.transient
  | Error error -> Alcotest.failf "another error: %s" (Error.to_string error)
  | Ok () -> Alcotest.fail "the loops went on");
  Alcotest.(check int) "the table is as it was" 0 (processed f)

(* Two processes set the same table up at once: both succeed, and the cut is
   written once. Without the lock, both would see no cut and write theirs. *)
let test_two_setups_at_once_agree env uri () =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions = connect_pool ~sw ~stdenv uri in
  let table = "inbox_twice" and sequence = "inbox_twice_seq" in
  let build () =
    Inbox.create
      ~table:(Identifier.of_string_exn table)
      ~sequence:(Identifier.of_string_exn sequence)
      ~slots:2 sessions
  in
  let first = build () and second = build () in
  let session body = unwrap "session" (Pool.session sessions ~lift body) in
  session (fun s ->
      exec_exn s
        (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_meta, %s_slots" table table table);
      exec_exn s (Printf.sprintf "DROP SEQUENCE IF EXISTS %s" sequence);
      Ok ());
  let a, b =
    Eio.Fiber.pair
      (fun () -> Pool.session sessions ~lift (fun s -> Inbox.setup first s))
      (fun () -> Pool.session sessions ~lift (fun s -> Inbox.setup second s))
  in
  unwrap "first setup" a;
  unwrap "second setup" b;
  let cuts =
    session (fun s ->
        Ok (find_int s (Printf.sprintf "SELECT count(*) FROM %s_meta" table)))
  in
  Alcotest.(check int) "one cut" 1 cuts

(* Records what the inbox reports, in the words of the protocol model. *)
type recorder = {
  events : string list ref;
  received : (string * Observer.receipt option) list ref;
  (* each fetch of a row, with whether its snapshot saw the row's transaction *)
  fetches_saw_their_row : bool list ref;
  marked : (string * int) list ref;
}

let recorder () =
  let r =
    {
      events = ref [];
      received = ref [];
      fetches_saw_their_row = ref [];
      marked = ref [];
    }
  in
  let note event = r.events := event :: !(r.events) in
  let dependency (d : Dependency.t) =
    match d.stream_id with
    | `Assoc [ ("id", `String stream) ] -> Printf.sprintf "%s@%d" stream d.stream_position
    | _ -> Dependency.to_string d
  in
  let woken messages = String.concat " " (List.map label messages) in
  ( r,
    {
      Observer.on_received =
        (fun event ->
          r.received := (label event.message, event.receipt) :: !(r.received);
          note
            (Printf.sprintf "received %s %s" (label event.message)
               (match event.receipt with Some _ -> "stored" | None -> "duplicate")));
      on_waiting =
        (fun event ->
          note
            (Printf.sprintf "waiting %s for %s" (label event.message)
               (dependency event.dependency)));
      on_expired =
        (fun event -> note (Printf.sprintf "expired %s" (woken event.messages)));
      on_fetched =
        (fun event ->
          (match event.message with
          | Some message ->
              let stored =
                List.find_map
                  (fun (name, receipt) -> if name = label message then receipt else None)
                  !(r.received)
              in
              r.fetches_saw_their_row :=
                (match stored with
                | Some (receipt : Observer.receipt) ->
                    Snapshot.sees event.snapshot receipt.transaction_id
                | None -> false)
                :: !(r.fetches_saw_their_row)
          | None -> ());
          note
            (match event.message with
            | Some message -> Printf.sprintf "fetched %s" (label message)
            | None -> "fetched nothing"));
      on_handled =
        (fun event ->
          note
            (Printf.sprintf "handled %s %s" (label event.message)
               (if Result.is_ok event.outcome then "ok" else "failed")));
      on_marked =
        (fun event ->
          r.marked := (label event.message, event.processed_position) :: !(r.marked);
          note
            (if event.woken = [] then Printf.sprintf "marked %s" (label event.message)
             else
               Printf.sprintf "marked %s woke %s" (label event.message)
                 (woken event.woken)));
      on_failed =
        (fun event ->
          note
            (Printf.sprintf "failed %s %d%s" (label event.message) event.attempts
               (if event.parked then " parked" else "")));
      on_dispatched =
        (fun event ->
          note
            (match event.outcome with
            | Ok Outcome.Processed -> "dispatched a message"
            | Ok (Outcome.Failed _) -> "dispatched a failure"
            | Ok Outcome.Set_aside -> "dispatched a set-aside"
            | Ok Outcome.Nothing -> "dispatched nothing"
            | Error _ -> "rolled back"));
      on_unparked =
        (fun event -> note (Printf.sprintf "unparked %s" (label event.message)));
      on_resolved =
        (fun event ->
          note
            (if event.woken = [] then Printf.sprintf "resolved %s" (label event.message)
             else
               Printf.sprintf "resolved %s woke %s" (label event.message)
                 (woken event.woken)));
    } )

(* The observer sees the protocol the model in verify/tla/Inbox.tla is
   written in: a message received with its order of arrival, or ignored as a
   duplicate; a row set aside to wait for its dependency, and woken by the
   mark of that dependency; the subscriber's outcome; the mark with its order
   of processing; the close of the transaction, a recorded attempt when the
   subscriber failed, and the message again afterwards. *)
let test_the_observer_sees_the_protocol env uri () =
  let r, observer = recorder () in
  with_fixture ~name:"observed" ~observer env uri @@ fun f ->
  let first = message "a" 1 in
  let second = Message.depending_on (message "b" 1) [ Message.identity first ] in
  let _, subscriber = collector () in
  (* The dependent arrives first and is set aside until the other is
     processed. *)
  publish f [ second; first ];
  Alcotest.check outcome "set aside" Outcome.Set_aside (dispatch f subscriber);
  Alcotest.check outcome "a@1" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "b@1" Outcome.Processed (dispatch f subscriber);
  Alcotest.check outcome "nothing" Outcome.Nothing (dispatch f subscriber);
  (* The same identity again is not a step. *)
  publish f [ first ];
  (* The subscriber fails once: nothing is marked, the message comes again. *)
  publish f [ message "c" 1 ];
  let attempts = ref 0 in
  let flaky _ _ =
    let first = !attempts = 0 in
    incr attempts;
    if first then Error (Failure.transient "the first attempt fails on purpose")
    else Ok ()
  in
  Alcotest.check outcome "failed once"
    (Outcome.Failed { attempts = 1; parked = false })
    (dispatch f flaky);
  Alcotest.check outcome "then processed" Outcome.Processed (dispatch f flaky);
  Alcotest.(check (list string))
    "the protocol"
    [
      "received b@1 stored";
      "received a@1 stored";
      "waiting b@1 for a@1";
      "dispatched a set-aside";
      "fetched a@1";
      "handled a@1 ok";
      "marked a@1 woke b@1";
      "dispatched a message";
      "fetched b@1";
      "handled b@1 ok";
      "marked b@1";
      "dispatched a message";
      "fetched nothing";
      "dispatched nothing";
      "received a@1 duplicate";
      "received c@1 stored";
      "fetched c@1";
      "handled c@1 failed";
      "failed c@1 1";
      "dispatched a failure";
      "fetched c@1";
      "handled c@1 ok";
      "marked c@1";
      "dispatched a message";
    ]
    (List.rev !(r.events));
  (* Arrival order and processing order are what the sequences say. *)
  let received =
    List.rev_map
      (fun (_, receipt) ->
        Option.map (fun (r : Observer.receipt) -> r.received_position) receipt)
      !(r.received)
  in
  let position i = Option.get (List.nth received i) in
  Alcotest.(check int) "four receipts" 4 (List.length received);
  Alcotest.(check bool) "b before a" true (position 0 < position 1);
  Alcotest.(check (option int)) "the duplicate has none" None (List.nth received 2);
  Alcotest.(check bool) "a before c" true (position 1 < position 3);
  (* Every fetch ran under a snapshot that saw the transaction which stored
     the row it returned: visibility, as the dispatcher saw it. *)
  let saw = List.rev !(r.fetches_saw_their_row) in
  Alcotest.(check int) "a@1, b@1, c@1 twice" 4 (List.length saw);
  Alcotest.(check bool) "each visible" true (List.for_all Fun.id saw);
  let marked = List.rev !(r.marked) in
  Alcotest.(check (list string))
    "marked in order" [ "a@1"; "b@1"; "c@1" ] (List.map fst marked);
  let rec ascending = function
    | (_, a) :: ((_, b) :: _ as rest) -> a < b && ascending rest
    | _ -> true
  in
  Alcotest.(check bool) "processed positions ascend" true (ascending marked)

(* A message whose dependency never arrives waits out of the queue, costing
   the walk nothing, and with max_wait set is parked with the dependency
   named (ADR-0005). *)
let test_a_dependency_that_never_arrives_parks_the_message_after_max_wait env uri () =
  with_fixture ~name:"expire" ~max_wait:0.3 env uri @@ fun f ->
  let cause = message "user" 5 in
  let dependent = Message.depending_on (message "order" 1) [ Message.identity cause ] in
  publish f [ dependent ];
  let seen, subscriber = collector () in
  Alcotest.check outcome "set aside" Outcome.Set_aside (dispatch f subscriber);
  Alcotest.(check int)
    "one waiting" 1
    (count f
       (Printf.sprintf "SELECT count(*) FROM %s WHERE waiting_for IS NOT NULL" f.table));
  Alcotest.check outcome "still waiting" Outcome.Nothing (dispatch f subscriber);
  sleep f 0.35;
  Alcotest.check outcome "expired and parked" Outcome.Nothing (dispatch f subscriber);
  Alcotest.(check (list string)) "parked" [ "order@1" ] (parked f);
  let last_error = Option.value (List.hd (parked_messages f)).last_error ~default:"" in
  Alcotest.(check bool)
    (Printf.sprintf "the dependency named: %s" last_error)
    true
    (contains ~sub:"never arrived" last_error);
  Alcotest.(check (list string)) "no effects" [] (seen ());
  (* The dependency arrives after all: unparked, the message waits again,
     and is woken by the dependency's mark. *)
  publish f [ cause ];
  Alcotest.(check bool) "unparked" true (unpark f dependent);
  drain f subscriber;
  Alcotest.(check (list string))
    "the cause, then the dependent" [ "user@5"; "order@1" ] (seen ())

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

(* A parked message must not be silent when no observer is attached: an
   attempt is a warning, a parking an error, on the inbox's own source. *)
let test_a_failed_attempt_and_a_parking_are_logged env uri () =
  with_fixture ~name:"logged" ~trace:false ~retries:(Retries.up_to 2) env uri @@ fun f ->
  publish f [ message "a" 1 ];
  let _, subscriber = failing_on "a@1" in
  let logs =
    logged (fun () ->
        ignore (dispatch f subscriber);
        ignore (dispatch f subscriber))
  in
  Alcotest.(check bool)
    "the attempt is a warning" true
    (said logs ~src:"ascetic_ddd.inbox" ~level:Logs.Warning
       ~text:"attempt 1 on tenant-1/orders.Order/");
  Alcotest.(check bool)
    "the parking is an error" true
    (said logs ~src:"ascetic_ddd.inbox" ~level:Logs.Error
       ~text:"parked after 2 failed attempts: a@1 is poison")

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (test env uri) in
  [
    case "a received message is processed once in its transaction"
      test_a_received_message_is_processed_once_in_its_transaction;
    case "receiving the same message twice stores it once"
      test_receiving_the_same_message_twice_stores_it_once;
    case "a message_id is stored once" test_a_message_id_is_stored_once;
    case "messages are processed in order of arrival"
      test_messages_are_processed_in_order_of_arrival;
    case "a message waits for its causal dependencies"
      test_a_message_waits_for_its_causal_dependencies;
    case "a failing subscriber leaves the message unprocessed"
      test_a_failing_subscriber_leaves_the_message_unprocessed;
    case "a poison message is parked after its attempts and the partition flows"
      test_a_poison_message_is_parked_after_its_attempts_and_the_partition_flows;
    case "a message in backoff holds its partition"
      test_a_message_in_backoff_holds_its_partition;
    case "an unparked message is tried again" test_an_unparked_message_is_tried_again;
    case "a resolved message releases its dependents"
      test_a_resolved_message_releases_its_dependents;
    case "a held slot is passed by" test_a_held_slot_is_passed_by;
    case "slots share the streams without gaps or overlap"
      test_slots_share_the_streams_without_gaps_or_overlap;
    case "run processes until shutdown" test_run_processes_until_shutdown;
    case "two slots are worked at once" test_two_slots_are_worked_at_once;
    case "a wait set while its dependency arrives and is marked elsewhere is woken"
      test_a_wait_set_while_its_dependency_arrives_and_is_marked_elsewhere_is_woken;
    case "a wait set while its dependency is being marked elsewhere is woken"
      test_a_wait_set_while_its_dependency_is_being_marked_elsewhere_is_woken;
    case "loops over slots with dependencies across them lose no wake"
      test_loops_over_slots_with_dependencies_across_them_lose_no_wake;
    case "messages of one stream are never processed at once"
      test_messages_of_one_stream_are_never_processed_at_once;
    case "a permanent failure parks the message at once"
      test_a_permanent_failure_parks_the_message_at_once;
    case "a loop outlives a lost connection" test_a_loop_outlives_a_lost_connection;
    case "a defect stops the loops" test_a_defect_stops_the_loops;
    case "two setups at once agree" test_two_setups_at_once_agree;
    case "the observer sees the protocol" test_the_observer_sees_the_protocol;
    case "a dependency that never arrives parks the message after max_wait"
      test_a_dependency_that_never_arrives_parks_the_message_after_max_wait;
    case "a failed attempt and a parking are logged"
      test_a_failed_attempt_and_a_parking_are_logged;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] inbox integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Pg_inbox" [ ("integration", cases env uri) ]
