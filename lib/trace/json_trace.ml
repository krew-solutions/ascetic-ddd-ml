(** Records every event as a JSON object, in order. *)

module Outbox_message = Ascetic_outbox.Outbox_message
module Outbox_observer = Ascetic_outbox.Outbox_observer
module Inbox_message = Ascetic_inbox.Inbox_message
module Inbox_observer = Ascetic_inbox.Inbox_observer
module Causal_dependency = Ascetic_inbox.Causal_dependency
module Snapshot = Ascetic_inbox.Snapshot
module Outcome = Ascetic_inbox.Outcome

type t = { mutable lines : Yojson.Safe.t list (* newest first *) }

(** An empty recorder. *)
let create () = { lines = [] }

(** What was recorded so far, in order. *)
let lines t = List.rev t.lines

let record t line = t.lines <- line :: t.lines

(** Writes the lines to [path], one JSON object per line, creating the directory if
    needed. *)
let write_to t path =
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let out = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out out)
    (fun () ->
      List.iter
        (fun line ->
          output_string out (Yojson.Safe.to_string line);
          output_char out '\n')
        (lines t))

(* Fields in name order, as the Rust recorder writes them, so that the two
   ports' traces read alike. *)
let obj fields : Yojson.Safe.t =
  `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) fields)

let string s : Yojson.Safe.t = `String s
let int n : Yojson.Safe.t = `Int n
let bool b : Yojson.Safe.t = `Bool b
let option f = function None -> `Null | Some v -> f v
let list f items : Yojson.Safe.t = `List (List.map f items)

let outbox_id (message : Outbox_message.t) : Yojson.Safe.t =
  match message.metadata with
  | `Assoc fields -> Option.value (List.assoc_opt "message_id" fields) ~default:`Null
  | _ -> `Null

(* A row's identity as one string. *)
let inbox_id message = Causal_dependency.to_string (Inbox_message.identity message)

let snapshot_json (snapshot : Snapshot.t) =
  obj
    [
      ("xmin", int snapshot.xmin);
      ("xmax", int snapshot.xmax);
      ("xip", list int snapshot.in_progress);
    ]

(** The recorder as an outbox observer, for [Pg_outbox.create ~observer]. *)
let outbox_observer t : 'e Outbox_observer.t =
  let side = ("observer", string "outbox") in
  {
    on_published =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "published");
               ("message_id", outbox_id event.message);
               ("uri", string event.message.uri);
               ("xid", int event.receipt.transaction_id);
               ("position", int event.receipt.position);
             ]));
    on_fetched =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "fetched");
               ("group", string event.group);
               ("slot", option int event.slot);
               ("slots", int event.slots);
               ("horizon", int event.horizon);
               ("limit", int event.limit);
               ("messages", list outbox_id event.messages);
             ]));
    on_handled =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "handled");
               ("group", string event.group);
               ("slot", int event.slot);
               ("message_id", outbox_id event.message);
               ("ok", bool (Result.is_ok event.outcome));
             ]));
    on_acked =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "acked");
               ("group", string event.group);
               ("slot", int event.slot);
               ("xid", int event.position.transaction_id);
               ("position", int event.position.offset);
             ]));
    on_dispatched =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "dispatched");
               ("group", string event.group);
               ("slot", option int event.slot);
               ( "outcome",
                 string
                   (match event.outcome with
                   | Ok true -> "batch"
                   | Ok false -> "nothing"
                   | Error _ -> "rolled_back") );
             ]));
  }

(** The recorder as an inbox observer, for [Pg_inbox.create ~observer]. *)
let inbox_observer t : Inbox_observer.t =
  let side = ("observer", string "inbox") in
  {
    on_received =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "received");
               ("id", string (inbox_id event.message));
               ("message_id", option string (Inbox_message.message_id event.message));
               ( "deps",
                 list
                   (fun d -> string (Causal_dependency.to_string d))
                   (Inbox_message.causal_dependencies event.message) );
               ( "received_position",
                 option
                   (fun (r : Inbox_observer.receipt) -> int r.received_position)
                   event.receipt );
               ( "xid",
                 option
                   (fun (r : Inbox_observer.receipt) -> int r.transaction_id)
                   event.receipt );
               ( "slot",
                 option (fun (r : Inbox_observer.receipt) -> int r.slot) event.receipt );
             ]));
    on_waiting =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "waiting");
               ("slot", int event.slot);
               ("id", string (inbox_id event.message));
               ("dependency", string (Causal_dependency.to_string event.dependency));
               ("snapshot", snapshot_json event.snapshot);
             ]));
    on_fetched =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "fetched");
               ("slot", option int event.slot);
               ("slots", int event.slots);
               ("id", option (fun m -> string (inbox_id m)) event.message);
               ("snapshot", snapshot_json event.snapshot);
             ]));
    on_handled =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "handled");
               ("slot", int event.slot);
               ("id", string (inbox_id event.message));
               ("ok", bool (Result.is_ok event.outcome));
             ]));
    on_marked =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "marked");
               ("slot", int event.slot);
               ("id", string (inbox_id event.message));
               ("processed_position", int event.processed_position);
               ("woken", list (fun m -> string (inbox_id m)) event.woken);
             ]));
    on_expired =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "expired");
               ("ids", list (fun m -> string (inbox_id m)) event.messages);
             ]));
    on_failed =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "failed");
               ("slot", int event.slot);
               ("id", string (inbox_id event.message));
               ("attempts", int event.attempts);
               ("parked", bool event.parked);
               ("permanent", bool (Ascetic_inbox.Failure.is_permanent event.failure));
               ("retry_after_ms", int (int_of_float (event.retry_after *. 1000.0)));
               ("error", string (Ascetic_inbox.Failure.message event.failure));
             ]));
    on_dispatched =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "dispatched");
               ("slot", option int event.slot);
               ( "outcome",
                 string
                   (match event.outcome with
                   | Ok Outcome.Processed -> "processed"
                   | Ok (Outcome.Failed _) -> "failed"
                   | Ok Outcome.Set_aside -> "set_aside"
                   | Ok Outcome.Nothing -> "nothing"
                   | Error _ -> "rolled_back") );
             ]));
    on_unparked =
      (fun event ->
        record t
          (obj
             [
               side; ("event", string "unparked"); ("id", string (inbox_id event.message));
             ]));
    on_resolved =
      (fun event ->
        record t
          (obj
             [
               side;
               ("event", string "resolved");
               ("id", string (inbox_id event.message));
               ("processed_position", int event.processed_position);
               ("woken", list (fun m -> string (inbox_id m)) event.woken);
             ]));
  }
