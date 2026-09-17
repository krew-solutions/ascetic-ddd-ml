module Session = Ascetic_session_caqti.Caqti_session
module Session_pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Transient = Ascetic_session_caqti.Transient
module Error = Outbox_error
module Observer = Outbox_observer

type 'e subscriber = Outbox_message.t -> (unit, 'e) result

let default_batch_size = 100

(* ------------------------------------------------------------------------ *)
(* Errors                                                                    *)

(* A value that could not be encoded or decoded is a defect of the caller's
   types or of the stored data; anything else the driver reports is the
   database's, with the driver's verdict on whether it is of the moment. *)
let of_caqti (error : Caqti_error.t) =
  match error with
  | `Encode_rejected _ | `Encode_failed _ | `Decode_rejected _ ->
      Error.Malformed (Caqti_error.show error)
  | _ -> Error.Database (Transient.driver_error error)

(* A driver call returns its error, or raises one of the client library's on
   a connection whose server is gone: either way the adapter gets an error. *)
let caqti (call : unit -> ('a, [< Caqti_error.t ]) result) =
  Transient.protect
    ~raised:(fun reason -> Error.Database reason)
    (fun () ->
      Result.map_error (fun error -> of_caqti (error :> Caqti_error.t)) (call ()))

let session_error error = Error.Session error
let ( let* ) = Result.bind

(* [xid8] has no driver type; it travels as decimal text and is unsigned.
   An [int] holds every value PostgreSQL will assign in practice: the epoch
   counter would have to wrap a billion times to reach the sign bit. *)
let transaction_id text =
  match int_of_string_opt text with
  | Some id -> Ok id
  | None -> Error (Error.Malformed (Printf.sprintf "transaction id `%s`" text))

let json_of_text text =
  match Yojson.Safe.from_string text with
  | json -> Ok json
  | exception Yojson.Json_error reason ->
      Error (Error.Malformed (Printf.sprintf "metadata `%s`: %s" text reason))

(* ------------------------------------------------------------------------ *)
(* The statements                                                             *)

(* Built once per outbox: a request is prepared by the driver and cached per
   connection under its own identity, so one built per call would prepare
   the same statement again and again. *)
module Requests = struct
  open Caqti_request.Infix
  open Caqti_type

  type message = int * string * string * string * string * Ptime.t option
  (* position, transaction_id::text, uri, payload, metadata::text, created_at *)

  type t = {
    lock : (string, int, [ `One ]) Caqti_request.t;
    ddl : string list;
    pinned : (unit, int, [ `One | `Zero ]) Caqti_request.t;
    publish : (string * string * string, string * int, [ `One ]) Caqti_request.t;
    ensure_positions : (string * string, unit, [ `Zero ]) Caqti_request.t;
    read_positions :
      (string * string, string * int, [ `Many | `One | `Zero ]) Caqti_request.t;
    fetch :
      ( string * string * string * int,
        int option * message option * (string * int * bool),
        [ `Many | `One | `Zero ] )
      Caqti_request.t;
    ack : (string * string * int * int * string, int, [ `One | `Zero ]) Caqti_request.t;
    move_all : (string * string * int * string, unit, [ `Zero ]) Caqti_request.t;
  }

  let make ~outbox ~offsets ~slots =
    let outbox = Identifier.to_string outbox and offsets = Identifier.to_string offsets in
    let sprintf = Printf.sprintf in
    {
      (* One transaction under an advisory lock on the table's name: two
         processes setting the table up at once would otherwise race in
         CREATE TABLE IF NOT EXISTS and both write the cut. *)
      lock =
        (string ->! int) "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1))) AS l";
      ddl =
        [
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  \"position\" BIGSERIAL,\n\
            \  \"uri\" VARCHAR(255) NOT NULL,\n\
            \  \"payload\" BYTEA NOT NULL,\n\
            \  \"metadata\" JSONB NOT NULL,\n\
            \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
            \  \"transaction_id\" xid8 NOT NULL,\n\
            \  \"slot\" SMALLINT GENERATED ALWAYS AS ((hashtext(\"uri\") & 2147483647) \
             %% %d) STORED,\n\
            \  PRIMARY KEY (\"transaction_id\", \"position\")\n\
             )"
            outbox slots;
          sprintf
            "CREATE INDEX IF NOT EXISTS %s_slot_idx ON %s (\"slot\", \"transaction_id\", \
             \"position\")"
            outbox outbox;
          sprintf
            "CREATE INDEX IF NOT EXISTS %s_uri_position_idx ON %s (\"uri\" \
             varchar_pattern_ops, \"transaction_id\", \"position\")"
            outbox outbox;
          sprintf
            "CREATE UNIQUE INDEX IF NOT EXISTS %s_message_id_uniq ON %s \
             (((metadata->>'message_id')::uuid))"
            outbox outbox;
          sprintf "CREATE TABLE IF NOT EXISTS %s_meta (\"slots\" INTEGER NOT NULL)" outbox;
          sprintf
            "INSERT INTO %s_meta (\"slots\") SELECT %d WHERE NOT EXISTS (SELECT 1 FROM \
             %s_meta)"
            outbox slots outbox;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  \"consumer_group\" VARCHAR(255) NOT NULL,\n\
            \  \"uri\" VARCHAR(255) NOT NULL DEFAULT '',\n\
            \  \"slot\" SMALLINT NOT NULL,\n\
            \  \"offset_acked\" BIGINT NOT NULL DEFAULT 0,\n\
            \  \"last_processed_transaction_id\" xid8 NOT NULL DEFAULT '0',\n\
            \  \"updated_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
            \  PRIMARY KEY (\"consumer_group\", \"uri\", \"slot\")\n\
             )"
            offsets;
        ];
      pinned = (unit ->? int) (sprintf "SELECT slots FROM %s_meta" outbox);
      publish =
        (t3 string octets string ->! t2 string int)
          (sprintf
             "INSERT INTO %s (uri, payload, metadata, transaction_id) VALUES ($1, $2, \
              $3::jsonb, pg_current_xact_id()) RETURNING transaction_id::text, \
              \"position\""
             outbox);
      (* The position rows of a selection, one per slot, must exist before a
         fetch can lock one: created at the first contact. *)
      ensure_positions =
        (t2 string string ->. unit)
          (sprintf
             "INSERT INTO %s (consumer_group, uri, slot) SELECT $1, $2, s FROM \
              generate_series(0, (SELECT slots FROM %s_meta) - 1) AS s ON CONFLICT DO \
              NOTHING"
             offsets outbox);
      read_positions =
        (t2 string string ->* t2 string int)
          (sprintf
             "SELECT last_processed_transaction_id::text, offset_acked FROM %s WHERE \
              consumer_group = $1 AND uri = $2 ORDER BY slot"
             offsets);
      (* One statement that takes the slot of the selection least recently
         served among those with visible work, committed rows past the
         slot's position, below the snapshot's xmin, matching the selection's
         URI, locks its position row, FOR UPDATE SKIP LOCKED, so that a slot
         another dispatcher holds is passed by, and reads that slot's batch
         under the same snapshot. The outer join makes the statement return a
         row even when no slot had work, so that the horizon is always known.

         "Past the position" is the row comparison (transaction_id, position)
         > (last, offset): the same predicate as "a later transaction, or the
         same one at a later position", but one the planner takes as the
         start of the index range. Whether a slot has work is asked as "its
         first row past the position, or none", ordered and limited to one,
         rather than as EXISTS: with one slot the planner answered EXISTS
         with a bitmap over the whole tail. *)
      fetch =
        (t4 string string string int
        ->* t3 (option int)
              (option (t6 int string string octets string (option ptime)))
              (t3 string int bool))
          (sprintf
             "WITH taken AS (\n\
             \  SELECT o.slot, o.offset_acked, o.last_processed_transaction_id\n\
             \  FROM %s o\n\
             \  WHERE o.consumer_group = $1 AND o.uri = $2\n\
             \    AND (SELECT m.\"position\" FROM %s m\n\
             \         WHERE m.slot = o.slot\n\
             \           AND (m.transaction_id, m.\"position\") > \
              (o.last_processed_transaction_id, o.offset_acked)\n\
             \           AND m.transaction_id < pg_snapshot_xmin(pg_current_snapshot())\n\
             \           AND ($2 = '' OR m.uri = $2 OR m.uri LIKE $3)\n\
             \         ORDER BY m.transaction_id, m.\"position\"\n\
             \         LIMIT 1) IS NOT NULL\n\
             \  ORDER BY o.updated_at\n\
             \  LIMIT 1\n\
             \  FOR UPDATE OF o SKIP LOCKED\n\
              )\n\
              SELECT t.slot, m.\"position\", m.transaction_id::text, m.uri, m.payload, \
              m.metadata::text, m.created_at,\n\
             \       pg_snapshot_xmin(pg_current_snapshot())::text,\n\
             \       (SELECT slots FROM %s_meta),\n\
             \       EXISTS (SELECT 1 FROM %s WHERE consumer_group = $1 AND uri = $2)\n\
              FROM (SELECT 1) AS one\n\
              LEFT JOIN taken t ON true\n\
              LEFT JOIN LATERAL (\n\
             \  SELECT \"position\", transaction_id, uri, payload, metadata, created_at\n\
             \  FROM %s\n\
             \  WHERE slot = t.slot\n\
             \    AND (transaction_id, \"position\") > (t.last_processed_transaction_id, \
              t.offset_acked)\n\
             \    AND transaction_id < pg_snapshot_xmin(pg_current_snapshot())\n\
             \    AND ($2 = '' OR uri = $2 OR uri LIKE $3)\n\
             \  ORDER BY transaction_id, \"position\"\n\
             \  LIMIT $4\n\
              ) AS m ON true\n\
              ORDER BY m.transaction_id, m.\"position\""
             offsets outbox outbox offsets outbox);
      (* Moves a slot's position: the row is the one the fetch locked. *)
      ack =
        (t5 string string int int string ->? int)
          (sprintf
             "UPDATE %s SET offset_acked = $4, last_processed_transaction_id = \
              $5::text::xid8, updated_at = CURRENT_TIMESTAMP WHERE consumer_group = $1 \
              AND uri = $2 AND slot = $3 RETURNING 1"
             offsets);
      move_all =
        (t4 string string int string ->. unit)
          (sprintf
             "UPDATE %s SET offset_acked = $3, last_processed_transaction_id = \
              $4::text::xid8, updated_at = CURRENT_TIMESTAMP WHERE consumer_group = $1 \
              AND uri = $2"
             offsets);
    }
end

type 'e t = {
  pool : Session_pool.t;
  observer : 'e Observer.t;
  outbox_table : Identifier.t;
  batch_size : int;
  slots : int;
  requests : Requests.t;
}

let create ?(observer = Observer.none) ?outbox_table ?offsets_table
    ?(batch_size = default_batch_size) ?(slots = 1) pool =
  let outbox_table =
    Option.value outbox_table ~default:(Identifier.of_string_exn "outbox")
  in
  let offsets_table =
    Option.value offsets_table ~default:(Identifier.of_string_exn "outbox_offsets")
  in
  let slots = max 1 (min slots 32767) in
  {
    pool;
    observer;
    outbox_table;
    batch_size = max 1 batch_size;
    slots;
    requests = Requests.make ~outbox:outbox_table ~offsets:offsets_table ~slots;
  }

let slots t = t.slots

(* ------------------------------------------------------------------------ *)
(* The store                                                                 *)

let connection session = Session.connection session

let setup t session =
  let requests = t.requests in
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val connection tx) in
      let* _ =
        caqti (fun () -> C.find requests.lock (Identifier.to_string t.outbox_table))
      in
      let* () =
        List.fold_left
          (fun acc sql ->
            let* () = acc in
            let open Caqti_request.Infix in
            caqti (fun () ->
                C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) ()))
          (Ok ()) requests.ddl
      in
      let* pinned = caqti (fun () -> C.find_opt requests.pinned ()) in
      match pinned with
      | Some pinned when pinned = t.slots -> Ok ()
      | Some pinned ->
          Error
            (Error.Malformed
               (Printf.sprintf
                  "table `%s` is cut into %d slots, this outbox asks for %d; the number \
                   is fixed for the life of the table"
                  (Identifier.to_string t.outbox_table)
                  pinned t.slots))
      | None ->
          Error
            (Error.Malformed
               (Printf.sprintf "table `%s` has no cut recorded"
                  (Identifier.to_string t.outbox_table))))

let publish t session (message : Outbox_message.t) =
  let module C = (val connection session) in
  let* transaction, position =
    caqti (fun () ->
        C.find t.requests.publish
          (message.uri, message.payload, Yojson.Safe.to_string message.metadata))
  in
  let* transaction_id = transaction_id transaction in
  t.observer.on_published { message; receipt = { transaction_id; position } };
  Ok ()

let ensure_positions t session ~group ~uri =
  let module C = (val connection session) in
  caqti (fun () -> C.exec t.requests.ensure_positions (group, uri))

let read_positions t session ~group ~uri =
  let module C = (val connection session) in
  let* rows = caqti (fun () -> C.collect_list t.requests.read_positions (group, uri)) in
  List.fold_right
    (fun (transaction, offset) acc ->
      let* acc = acc in
      let* transaction_id = transaction_id transaction in
      Ok ({ Position.transaction_id; offset } :: acc))
    rows (Ok [])

(* What one fetch read, all in one statement, because under READ COMMITTED
   every statement takes a snapshot of its own: the slot whose position row
   the statement locked, or none when no slot of the selection had visible
   work; the rows of that slot past its position, in (transaction_id,
   position) order; the visibility horizon of the statement,
   pg_snapshot_xmin; how many slots the table is cut into; and whether the
   selection has position rows at all, so that the first contact can create
   them. *)
type batch = {
  slot : int option;
  messages : Outbox_message.t list;
  horizon : int;
  slots : int;
  known : bool;
}

let message_of (position, transaction, uri, payload, metadata, created_at) =
  let* transaction_id = transaction_id transaction in
  let* metadata = json_of_text metadata in
  Ok
    {
      Outbox_message.uri;
      payload;
      metadata;
      created_at;
      position = Some position;
      transaction_id = Some transaction_id;
    }

let fetch t session ~group ~uri =
  let module C = (val connection session) in
  let* rows =
    caqti (fun () ->
        C.collect_list t.requests.fetch (group, uri, uri ^ "/%", t.batch_size))
  in
  match rows with
  | [] -> Error (Error.Malformed "a fetch returned no row at all")
  | (slot, _, (horizon, slots, known)) :: _ ->
      let* horizon = transaction_id horizon in
      let* messages =
        List.fold_right
          (fun (_, row, _) acc ->
            let* acc = acc in
            match row with
            | None -> Ok acc
            | Some row ->
                let* message = message_of row in
                Ok (message :: acc))
          rows (Ok [])
      in
      Ok { slot; messages; horizon; slots; known }

let ack t session ~group ~uri ~slot (position : Position.t) =
  let module C = (val connection session) in
  let* moved =
    caqti (fun () ->
        C.find_opt t.requests.ack
          (group, uri, slot, position.offset, string_of_int position.transaction_id))
  in
  match moved with
  | Some _ -> Ok ()
  | None ->
      Error
        (Error.Malformed
           (Printf.sprintf "the position of slot %d of `%s` is gone" slot group))

let move_all t session ~group ~uri (position : Position.t) =
  let module C = (val connection session) in
  caqti (fun () ->
      C.exec t.requests.move_all
        (group, uri, position.offset, string_of_int position.transaction_id))

(* ------------------------------------------------------------------------ *)
(* The dispatcher                                                             *)

let dispatch t (selection : Selection.t) (subscriber : 'e subscriber) =
  let group = selection.consumer_group and uri = selection.uri in
  let observer = t.observer in
  (* The error travels with the slot the transaction had taken, so that the
     observer learns which batch rolled back. *)
  let before_slot error = (None, error) in
  let lift error = before_slot (Error.Session error) in
  let outcome =
    Session_pool.session t.pool ~lift (fun session ->
        Session.atomic session ~lift (fun tx ->
            let* batch = Result.map_error before_slot (fetch t tx ~group ~uri) in
            let* batch =
              if batch.known then Ok batch
              else
                (* the first contact of this selection: its position rows *)
                let* () =
                  Result.map_error before_slot (ensure_positions t tx ~group ~uri)
                in
                Result.map_error before_slot (fetch t tx ~group ~uri)
            in
            observer.on_fetched
              {
                group;
                slot = batch.slot;
                slots = batch.slots;
                horizon = batch.horizon;
                limit = t.batch_size;
                messages = batch.messages;
              };
            match (batch.slot, List.rev batch.messages) with
            | Some slot, last :: _ ->
                let in_slot error = (Some slot, error) in
                let rec handle = function
                  | [] -> Ok ()
                  | message :: rest -> (
                      let handled = subscriber message in
                      observer.on_handled { group; slot; message; outcome = handled };
                      match handled with
                      | Ok () -> handle rest
                      | Error error -> Error (in_slot (Error.Subscriber error)))
                in
                let* () = handle batch.messages in
                let acked =
                  {
                    Position.transaction_id = Option.value last.transaction_id ~default:0;
                    offset = Option.value last.position ~default:0;
                  }
                in
                let* () = Result.map_error in_slot (ack t tx ~group ~uri ~slot acked) in
                observer.on_acked { group; slot; position = acked };
                Ok (Some slot)
            | _ -> Ok None))
  in
  let slot, outcome =
    match outcome with
    | Ok slot -> (slot, Ok (Option.is_some slot))
    | Error (slot, error) -> (slot, Error error)
  in
  observer.on_dispatched { group; slot; outcome };
  outcome

let run t ~clock ?(loops = Loops.default) ~shutdown selection subscriber =
  let stop, resolve_stop = Eio.Promise.create () in
  let stopped () = Eio.Promise.is_resolved stop in
  let defect = ref None in
  let pause seconds =
    Eio.Fiber.first
      (fun () -> Eio.Time.Mono.sleep clock seconds)
      (fun () -> Eio.Promise.await stop)
  in
  (* failures met in a row, for the length of the pause *)
  let rec loop failures =
    if not (stopped ()) then
      match dispatch t selection subscriber with
      | Ok true -> loop 0
      | Ok false ->
          pause loops.poll_interval;
          loop 0
      | Error error
        when match error with Subscriber _ -> true | _ -> Error.is_transient error ->
          let failures = failures + 1 in
          pause (Loops.pause_after loops failures);
          loop failures
      | Error error ->
          if Option.is_none !defect then defect := Some error;
          ignore (Eio.Promise.try_resolve resolve_stop ())
  in
  let finished, resolve_finished = Eio.Promise.create () in
  Eio.Fiber.both
    (fun () ->
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve resolve_finished ())
        (fun () ->
          Eio.Fiber.all (List.init (max 1 loops.concurrency) (fun _ () -> loop 0))))
    (fun () ->
      Eio.Fiber.first
        (fun () -> Eio.Promise.await shutdown)
        (fun () -> Eio.Promise.await finished);
      ignore (Eio.Promise.try_resolve resolve_stop ()));
  match !defect with None -> Ok () | Some error -> Error error

let positions t session (selection : Selection.t) =
  read_positions t session ~group:selection.consumer_group ~uri:selection.uri

let set_position t session (selection : Selection.t) position =
  let group = selection.consumer_group and uri = selection.uri in
  let* () = ensure_positions t session ~group ~uri in
  move_all t session ~group ~uri position
