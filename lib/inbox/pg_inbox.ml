module Session = Ascetic_session_caqti.Caqti_session
module Session_pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Transient = Ascetic_session_caqti.Transient
module Error = Inbox_error
module Observer = Inbox_observer

type subscriber = Session.t -> Inbox_message.t -> (unit, Failure.t) result

(* ------------------------------------------------------------------------ *)
(* Errors                                                                    *)

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
   An [int] holds every value PostgreSQL will assign in practice. *)
let transaction_id text =
  match int_of_string_opt text with
  | Some id -> Ok id
  | None -> Error (Error.Malformed (Printf.sprintf "not a transaction id: `%s`" text))

let snapshot text =
  Result.map_error (fun reason -> Error.Malformed reason) (Snapshot.of_string text)

let json_of_text text =
  match Yojson.Safe.from_string text with
  | json -> Ok json
  | exception Yojson.Json_error reason ->
      Error (Error.Malformed (Printf.sprintf "json `%s`: %s" text reason))

(* ------------------------------------------------------------------------ *)
(* The statements                                                             *)

(* Built once per inbox: a request is prepared by the driver and cached per
   connection under its own identity. *)
module Requests = struct
  open Caqti_request.Infix
  open Caqti_type

  (* The columns of a row, in the order [message_of] reads them. *)
  let columns =
    "tenant_id, stream_type, stream_id::text, stream_position, uri, payload, \
     metadata::text, received_position, processed_position, attempts, last_error, \
     waiting_for::text"

  (* The rows a dispatcher may take: neither processed, parked nor waiting. *)
  let queue = "processed_position IS NULL AND parked_at IS NULL AND waiting_for IS NULL"

  type row =
    (string * string * string * int * string * string)
    * (string option * int * int option * int * string option * string option)

  let row =
    t2
      (t6 string string string int string octets)
      (t6 (option string) int (option int) int (option string) (option string))

  type identity = string * string * string * int

  let identity = t4 string string string int

  type t = {
    lock_table : (string, int, [ `One ]) Caqti_request.t;
    ddl : string list;
    pin : (int * string, unit, [ `Zero ]) Caqti_request.t;
    pinned : (unit, int * string, [ `One | `Zero ]) Caqti_request.t;
    publish :
      ( string * string * string * int * string * string * string option,
        string * int * int,
        [ `One | `Zero ] )
      Caqti_request.t;
    take : (unit, int option * string * int, [ `One ]) Caqti_request.t;
    head_of : (int, (row * bool) option * string, [ `One ]) Caqti_request.t;
    is_processed : (identity, int, [ `One | `Zero ]) Caqti_request.t;
    lock_identity : (string * string, int, [ `One ]) Caqti_request.t;
    set_waiting : (identity * string * identity, int, [ `One | `Zero ]) Caqti_request.t;
    expire : (float, row, [ `Many | `One | `Zero ]) Caqti_request.t;
    mark :
      (identity * string, int * row option, [ `Many | `One | `Zero ]) Caqti_request.t;
    record_failure :
      (identity * (string * float * int * bool), int * bool, [ `One ]) Caqti_request.t;
    parked : (unit, row, [ `Many | `One | `Zero ]) Caqti_request.t;
    unpark : (identity, int, [ `One | `Zero ]) Caqti_request.t;
    resolve :
      (identity * string, int * row option, [ `Many | `One | `Zero ]) Caqti_request.t;
  }

  let make ~table ~sequence ~(partition : Partition_key.t) ~slots =
    let table = Identifier.to_string table and sequence = Identifier.to_string sequence in
    let sprintf = Printf.sprintf in
    let by_identity =
      "tenant_id = $1 AND stream_type = $2 AND stream_id = $3::jsonb AND stream_position \
       = $4"
    in
    {
      lock_table =
        (string ->! int) "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1))) AS l";
      ddl =
        [
          sprintf "CREATE SEQUENCE IF NOT EXISTS %s" sequence;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  tenant_id varchar(128) NOT NULL,\n\
            \  stream_type varchar(128) NOT NULL,\n\
            \  stream_id jsonb NOT NULL,\n\
            \  stream_position integer NOT NULL,\n\
            \  uri varchar(255) NOT NULL,\n\
            \  payload bytea NOT NULL,\n\
            \  metadata jsonb NULL,\n\
            \  received_position bigint NOT NULL DEFAULT nextval('%s'),\n\
            \  processed_position bigint NULL,\n\
            \  attempts integer NOT NULL DEFAULT 0,\n\
            \  last_error text NULL,\n\
            \  next_attempt_at timestamptz NULL,\n\
            \  parked_at timestamptz NULL,\n\
            \  waiting_for jsonb NULL,\n\
            \  waiting_since timestamptz NULL,\n\
            \  slot smallint GENERATED ALWAYS AS ((hashtext(%s) & 2147483647) %% %d) \
             STORED,\n\
            \  CONSTRAINT %s_pk PRIMARY KEY (tenant_id, stream_type, stream_id, \
             stream_position)\n\
             )"
            table sequence partition.sql_expression slots table;
          sprintf
            "CREATE INDEX IF NOT EXISTS %s__head_idx ON %s (slot, received_position) \
             WHERE %s"
            table table queue;
          sprintf
            "CREATE INDEX IF NOT EXISTS %s__waiting_idx ON %s (waiting_for) WHERE \
             waiting_for IS NOT NULL"
            table table;
          sprintf
            "CREATE INDEX IF NOT EXISTS %s__waiting_since_idx ON %s (waiting_since) \
             WHERE waiting_for IS NOT NULL"
            table table;
          sprintf
            "CREATE UNIQUE INDEX IF NOT EXISTS %s__message_id_uniq ON %s \
             (((metadata->>'message_id')::uuid))"
            table table;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s_meta (slots integer NOT NULL, partition_key \
             text NOT NULL)"
            table;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s_slots (slot smallint PRIMARY KEY, served_at \
             timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP)"
            table;
          sprintf
            "INSERT INTO %s_slots (slot) SELECT s FROM generate_series(0, %d - 1) AS s \
             ON CONFLICT DO NOTHING"
            table slots;
        ];
      pin =
        (t2 int string ->. unit)
          (sprintf
             "INSERT INTO %s_meta (slots, partition_key) SELECT $1::integer, $2 WHERE \
              NOT EXISTS (SELECT 1 FROM %s_meta)"
             table table);
      pinned =
        (unit ->? t2 int string)
          (sprintf "SELECT slots, partition_key FROM %s_meta" table);
      publish =
        (t7 string string string int string octets (option string) ->? t3 string int int)
          (sprintf
             "INSERT INTO %s (tenant_id, stream_type, stream_id, stream_position, uri, \
              payload, metadata) VALUES ($1, $2, $3::jsonb, $4, $5, $6, $7::jsonb) ON \
              CONFLICT (tenant_id, stream_type, stream_id, stream_position) DO NOTHING \
              RETURNING pg_current_xact_id()::text, received_position, slot"
             table);
      (* Takes a slot: the one least recently served among those whose head,
         the oldest row of the queue in the slot, is due, locking its row in
         <table>_slots for the length of the transaction, FOR UPDATE SKIP
         LOCKED, so that a slot another dispatcher holds is passed by. A slot
         whose head waits for its backoff is not taken: the head holds the
         slot, and the others go on. The head itself is read by [head_of], a
         statement of its own: a statement's snapshot predates the lock it
         takes, and the previous holder of the slot may commit between the
         two. *)
      take =
        (unit ->! t3 (option int) string int)
          (sprintf
             "WITH taken AS (\n\
             \  SELECT s.slot FROM %s_slots s\n\
             \  WHERE (SELECT r.next_attempt_at IS NULL OR r.next_attempt_at <= \
              CURRENT_TIMESTAMP\n\
             \         FROM %s r\n\
             \         WHERE r.slot = s.slot AND %s\n\
             \         ORDER BY r.received_position LIMIT 1) IS TRUE\n\
             \  ORDER BY s.served_at\n\
             \  LIMIT 1\n\
             \  FOR UPDATE OF s SKIP LOCKED\n\
              ), touched AS (\n\
             \  UPDATE %s_slots SET served_at = CURRENT_TIMESTAMP WHERE slot IN (SELECT \
              slot FROM taken)\n\
              )\n\
              SELECT t.slot, pg_current_snapshot()::text, (SELECT slots FROM %s_meta)\n\
              FROM (SELECT 1) AS one\n\
              LEFT JOIN taken t ON true"
             table table queue table table);
      (* The head of a slot the dispatcher holds: the oldest row of its queue,
         locked, with whether it still waits for its backoff, and the
         statement's snapshot; no row when the slot's queue is empty. *)
      head_of =
        (int ->! t2 (option (t2 row bool)) string)
          (sprintf
             "WITH r AS (\n\
             \  SELECT %s,\n\
             \         next_attempt_at IS NOT NULL AND next_attempt_at > \
              CURRENT_TIMESTAMP AS deferred\n\
             \  FROM %s\n\
             \  WHERE slot = $1 AND %s\n\
             \  ORDER BY received_position ASC\n\
             \  LIMIT 1\n\
             \  FOR UPDATE\n\
              )\n\
              SELECT r.*, pg_current_snapshot()::text AS snapshot\n\
              FROM (SELECT 1) AS one\n\
              LEFT JOIN r ON true"
             columns table queue);
      is_processed =
        (identity ->? int)
          (sprintf "SELECT 1 FROM %s WHERE %s AND processed_position IS NOT NULL LIMIT 1"
             table by_identity);
      (* The lock that orders a wait for an identity against its mark
         (ADR-0008): a transaction-level advisory lock, keyed by the table and
         the identity's canonical jsonb text, the one expression for every
         side. Held to the end of the transaction, so a transaction takes it
         last and takes nothing else after it: that is what rules cycles out. *)
      lock_identity =
        (t2 string string ->! int)
          "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1), \
           hashtext($2::jsonb::text))) AS l";
      (* Sets the row aside to wait for the dependency unless that dependency
         has a committed mark by now: one statement, under the lock, so the
         check and the wait are one step as in the model. *)
      set_waiting =
        (t3 identity string identity ->? int)
          (sprintf
             "UPDATE %s SET waiting_for = $5::jsonb, waiting_since = CURRENT_TIMESTAMP \
              WHERE %s AND NOT EXISTS (SELECT 1 FROM %s d WHERE d.tenant_id = $6 AND \
              d.stream_type = $7 AND d.stream_id = $8::jsonb AND d.stream_position = $9 \
              AND d.processed_position IS NOT NULL) RETURNING 1"
             table by_identity table);
      (* Parks the rows that have waited longer than max_wait, naming the
         dependency in their last_error. *)
      expire =
        (float ->* row)
          (sprintf
             "UPDATE %s SET parked_at = CURRENT_TIMESTAMP, last_error = 'dependency ' || \
              waiting_for::text || ' never arrived', waiting_for = NULL, waiting_since = \
              NULL WHERE ctid IN (SELECT ctid FROM %s WHERE waiting_for IS NOT NULL AND \
              waiting_since < CURRENT_TIMESTAMP - make_interval(secs => $1) FOR UPDATE \
              SKIP LOCKED) RETURNING %s"
             table table columns);
      (* Marks the message processed and, in the same statement, puts back
         into the queue every row that waited for it. *)
      mark =
        (t2 identity string ->* t2 int (option row))
          (sprintf
             "WITH marked AS (UPDATE %s SET processed_position = nextval('%s') WHERE %s \
              RETURNING processed_position), woken AS (UPDATE %s SET waiting_for = NULL, \
              waiting_since = NULL WHERE waiting_for = $5::jsonb RETURNING %s) SELECT \
              marked.processed_position, woken.* FROM marked LEFT JOIN woken ON true"
             table sequence by_identity table columns);
      (* Records a failed attempt: one more, the error, when the row is due
         again, and whether that was the last attempt, or the failure was
         permanent. *)
      record_failure =
        (t2 identity (t4 string float int bool) ->! t2 int bool)
          (sprintf
             "UPDATE %s SET attempts = attempts + 1, last_error = $5, next_attempt_at = \
              CURRENT_TIMESTAMP + make_interval(secs => $6), parked_at = CASE WHEN \
              $8::boolean OR ($7::integer > 0 AND attempts + 1 >= $7::integer) THEN \
              CURRENT_TIMESTAMP END WHERE %s RETURNING attempts, parked_at IS NOT NULL"
             table by_identity);
      parked =
        (unit ->* row)
          (sprintf
             "SELECT %s FROM %s WHERE parked_at IS NOT NULL ORDER BY received_position"
             columns table);
      unpark =
        (identity ->? int)
          (sprintf
             "UPDATE %s SET parked_at = NULL, attempts = 0, next_attempt_at = NULL WHERE \
              %s AND parked_at IS NOT NULL RETURNING 1"
             table by_identity);
      (* Marks a parked row processed by hand and wakes what waited for it,
         in one statement; no row when the row was not parked. *)
      resolve =
        (t2 identity string ->* t2 int (option row))
          (sprintf
             "WITH resolved AS (UPDATE %s SET parked_at = NULL, processed_position = \
              nextval('%s') WHERE %s AND parked_at IS NOT NULL RETURNING \
              processed_position), woken AS (UPDATE %s SET waiting_for = NULL, \
              waiting_since = NULL WHERE waiting_for = $5::jsonb AND EXISTS (SELECT 1 \
              FROM resolved) RETURNING %s) SELECT resolved.processed_position, woken.* \
              FROM resolved LEFT JOIN woken ON true"
             table sequence by_identity table columns);
    }
end

type t = {
  pool : Session_pool.t;
  observer : Observer.t;
  table : Identifier.t;
  partition : Partition_key.t;
  retries : Retries.t;
  max_wait : float option;
  slots : int;
  requests : Requests.t;
}

let create ?(observer = Observer.none) ?table ?sequence
    ?(partition = Partition_key.by_uri) ?(slots = 1) ?(retries = Retries.unlimited)
    ?max_wait pool =
  let table = Option.value table ~default:(Identifier.of_string_exn "inbox") in
  let sequence =
    Option.value sequence
      ~default:(Identifier.of_string_exn "inbox_received_position_seq")
  in
  let slots = max 1 (min slots 32767) in
  {
    pool;
    observer;
    table;
    partition;
    retries;
    max_wait;
    slots;
    requests = Requests.make ~table ~sequence ~partition ~slots;
  }

let with_retries t retries = { t with retries }
let with_max_wait t max_wait = { t with max_wait = Some max_wait }
let slots t = t.slots

(* ------------------------------------------------------------------------ *)
(* Rows                                                                       *)

let identity (message : Inbox_message.t) =
  ( message.tenant_id,
    message.stream_type,
    Yojson.Safe.to_string message.stream_id,
    message.stream_position )

let identity_of (dependency : Causal_dependency.t) =
  ( dependency.tenant_id,
    dependency.stream_type,
    Yojson.Safe.to_string dependency.stream_id,
    dependency.stream_position )

let identity_json dependency =
  Yojson.Safe.to_string (Causal_dependency.to_json dependency)

let message_of
    ( (tenant_id, stream_type, stream_id, stream_position, uri, payload),
      (metadata, received_position, processed_position, attempts, last_error, waiting_for)
    ) =
  let* stream_id = json_of_text stream_id in
  let* metadata =
    match metadata with
    | None -> Ok None
    | Some text -> Result.map Option.some (json_of_text text)
  in
  let* waiting_for =
    match waiting_for with
    | None -> Ok None
    | Some text ->
        let* json = json_of_text text in
        Ok (Causal_dependency.of_json json)
  in
  Ok
    {
      Inbox_message.tenant_id;
      stream_type;
      stream_id;
      stream_position;
      uri;
      payload;
      metadata;
      received_position = Some received_position;
      processed_position;
      attempts;
      last_error;
      waiting_for;
    }

let messages_of rows =
  List.fold_right
    (fun row acc ->
      let* acc = acc in
      let* message = message_of row in
      Ok (message :: acc))
    rows (Ok [])

let by_arrival (messages : Inbox_message.t list) =
  List.sort
    (fun (a : Inbox_message.t) (b : Inbox_message.t) ->
      compare a.received_position b.received_position)
    messages

(* The woken rows of a mark or a resolve: the rows of the statement whose
   row columns are not the outer join's nulls; oldest first. *)
let woken_of rows =
  let* woken = messages_of (List.filter_map snd rows) in
  Ok (by_arrival woken)

(* ------------------------------------------------------------------------ *)
(* The store                                                                 *)

let connection session = Session.connection session

let setup t session =
  let requests = t.requests in
  let table = Identifier.to_string t.table in
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val connection tx) in
      let* _ = caqti (fun () -> C.find requests.lock_table table) in
      let* () =
        List.fold_left
          (fun acc sql ->
            let* () = acc in
            let open Caqti_request.Infix in
            caqti (fun () ->
                C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) ()))
          (Ok ()) requests.ddl
      in
      let* () =
        caqti (fun () -> C.exec requests.pin (t.slots, t.partition.sql_expression))
      in
      let* pinned = caqti (fun () -> C.find_opt requests.pinned ()) in
      match pinned with
      | Some (slots, key) when slots = t.slots && key = t.partition.sql_expression ->
          Ok ()
      | Some (slots, key) ->
          Error
            (Error.Malformed
               (Printf.sprintf
                  "table `%s` is cut into %d slots by `%s`, this inbox asks for %d by \
                   `%s`; both are fixed for the life of the table"
                  table slots key t.slots t.partition.sql_expression))
      | None ->
          Error (Error.Malformed (Printf.sprintf "table `%s` has no cut recorded" table)))

let publish t (message : Inbox_message.t) =
  let* receipt =
    Session_pool.session t.pool ~lift:session_error (fun session ->
        Session.atomic session ~lift:session_error (fun tx ->
            let module C = (val connection tx) in
            let* row =
              caqti (fun () ->
                  C.find_opt t.requests.publish
                    ( message.tenant_id,
                      message.stream_type,
                      Yojson.Safe.to_string message.stream_id,
                      message.stream_position,
                      message.uri,
                      message.payload,
                      Option.map Yojson.Safe.to_string message.metadata ))
            in
            match row with
            | None -> Ok None
            | Some (transaction, received_position, slot) ->
                let* transaction_id = transaction_id transaction in
                Ok (Some { Observer.transaction_id; received_position; slot })))
  in
  t.observer.on_received { message; receipt };
  Ok ()

(* What taking a slot returned: the slot whose row the statement locked, or
   none when no slot had a due head; the statement's snapshot, read in the
   same statement because under READ COMMITTED every statement takes a
   snapshot of its own; and how many slots the table is cut into. *)
type taken = { slot : int option; snapshot : Snapshot.t; slots : int }

let take t session =
  let module C = (val connection session) in
  let* slot, snapshot_text, slots = caqti (fun () -> C.find t.requests.take ()) in
  let* snapshot = snapshot snapshot_text in
  Ok { slot; snapshot; slots }

(* What one look at the head of a held slot returned: the row, with whether
   it still waits for its backoff, or no row; and the statement's snapshot. *)
type step = { row : (Inbox_message.t * bool) option; snapshot : Snapshot.t }

let head_of t session slot =
  let module C = (val connection session) in
  let* row, snapshot_text = caqti (fun () -> C.find t.requests.head_of slot) in
  let* snapshot = snapshot snapshot_text in
  let* row =
    match row with
    | None -> Ok None
    | Some (row, deferred) ->
        let* message = message_of row in
        Ok (Some (message, deferred))
  in
  Ok { row; snapshot }

let is_processed t session dependency =
  let module C = (val connection session) in
  let* row =
    caqti (fun () -> C.find_opt t.requests.is_processed (identity_of dependency))
  in
  Ok (Option.is_some row)

(* The first causal dependency of the message without a committed mark. *)
let first_unprocessed_dependency t session message =
  let rec first = function
    | [] -> Ok None
    | dependency :: rest ->
        let* processed = is_processed t session dependency in
        if processed then first rest else Ok (Some dependency)
  in
  first (Inbox_message.causal_dependencies message)

let lock_identity t session dependency =
  let module C = (val connection session) in
  let* _ =
    caqti (fun () ->
        C.find t.requests.lock_identity
          (Identifier.to_string t.table, identity_json dependency))
  in
  Ok ()

(* Returns whether the row was set aside. *)
let set_waiting_unless_processed t session message dependency =
  let module C = (val connection session) in
  let* changed =
    caqti (fun () ->
        C.find_opt t.requests.set_waiting
          (identity message, identity_json dependency, identity_of dependency))
  in
  Ok (Option.is_some changed)

let expire_waiting t session max_wait =
  let module C = (val connection session) in
  let* rows = caqti (fun () -> C.collect_list t.requests.expire max_wait) in
  let* expired = messages_of rows in
  Ok (by_arrival expired)

let mark_processed t session message =
  let module C = (val connection session) in
  let* rows =
    caqti (fun () ->
        C.collect_list t.requests.mark
          (identity message, identity_json (Inbox_message.identity message)))
  in
  match rows with
  | [] -> Error (Error.Malformed "the row to mark is gone")
  | (processed_position, _) :: _ ->
      let* woken = woken_of rows in
      Ok (processed_position, woken)

let record_failure t session (message : Inbox_message.t) failure ~retry_after =
  let module C = (val connection session) in
  caqti (fun () ->
      C.find t.requests.record_failure
        ( identity message,
          ( Failure.message failure,
            retry_after,
            t.retries.max_attempts,
            Failure.is_permanent failure ) ))

let parked_rows t session =
  let module C = (val connection session) in
  let* rows = caqti (fun () -> C.collect_list t.requests.parked ()) in
  messages_of rows

let unpark_row t session message =
  let module C = (val connection session) in
  let* changed = caqti (fun () -> C.find_opt t.requests.unpark (identity message)) in
  Ok (Option.is_some changed)

let resolve_row t session message =
  let module C = (val connection session) in
  let* rows =
    caqti (fun () ->
        C.collect_list t.requests.resolve
          (identity message, identity_json (Inbox_message.identity message)))
  in
  match rows with
  | [] -> Ok None
  | (processed_position, _) :: _ ->
      let* woken = woken_of rows in
      Ok (Some (processed_position, woken))

(* ------------------------------------------------------------------------ *)
(* The dispatcher                                                             *)

let dispatch t (subscriber : subscriber) =
  let observer = t.observer in
  let* () =
    match t.max_wait with
    | None -> Ok ()
    | Some max_wait ->
        (* A statement of its own, outside the dispatch transaction: a
           transaction holding expired rows must never wait for a lock a
           marker holds (ADR-0008). *)
        let* expired =
          Session_pool.session t.pool ~lift:session_error (fun session ->
              expire_waiting t session max_wait)
        in
        if expired <> [] then observer.on_expired { messages = expired };
        Ok ()
  in
  (* The error travels with the slot the transaction held, so that the
     observer learns which slot's work rolled back. *)
  let before_slot error = (None, error) in
  let lift error = before_slot (Error.Session error) in
  let outcome =
    Session_pool.session t.pool ~lift (fun session ->
        Session.atomic session ~lift (fun tx ->
            let* taken = Result.map_error before_slot (take t tx) in
            match taken.slot with
            | None ->
                observer.on_fetched
                  {
                    slot = None;
                    slots = taken.slots;
                    message = None;
                    snapshot = taken.snapshot;
                  };
                Ok (None, Outcome.Nothing)
            | Some slot -> (
                let in_slot error = (Some slot, error) in
                let* step = Result.map_error in_slot (head_of t tx slot) in
                let snapshot = step.snapshot in
                match step.row with
                | Some (_, true) | None ->
                    (* the previous holder changed the head after the take
                       chose the slot: nothing due in it now *)
                    observer.on_fetched
                      { slot = Some slot; slots = taken.slots; message = None; snapshot };
                    Ok (Some slot, Outcome.Nothing)
                | Some (message, false) -> (
                    let* dependency =
                      Result.map_error in_slot (first_unprocessed_dependency t tx message)
                    in
                    match dependency with
                    | Some dependency ->
                        let* () =
                          Result.map_error in_slot (lock_identity t tx dependency)
                        in
                        let* set =
                          Result.map_error in_slot
                            (set_waiting_unless_processed t tx message dependency)
                        in
                        if not set then
                          (* marked between the check and the lock: nothing to
                             wait for, and the lock is the one this transaction
                             may hold; the next call takes the head again *)
                          Ok (Some slot, Outcome.Nothing)
                        else begin
                          observer.on_waiting { slot; message; dependency; snapshot };
                          Ok (Some slot, Outcome.Set_aside)
                        end
                    | None -> (
                        observer.on_fetched
                          {
                            slot = Some slot;
                            slots = taken.slots;
                            message = Some message;
                            snapshot;
                          };
                        (* The subscriber runs in a savepoint of its own: a
                           failure rolls its writes back and the transaction
                           goes on to record the attempt. *)
                        let attempt =
                          Session.atomic tx
                            ~lift:(fun error -> `Broke (Error.Session error))
                            (fun inner ->
                              Result.map_error
                                (fun failure -> `Declined failure)
                                (subscriber inner message))
                        in
                        let* failure =
                          match attempt with
                          | Ok () -> Ok None
                          | Error (`Declined failure) -> Ok (Some failure)
                          | Error (`Broke error) -> Error (in_slot error)
                        in
                        observer.on_handled
                          {
                            slot;
                            message;
                            outcome =
                              (match failure with None -> Ok () | Some f -> Error f);
                          };
                        match failure with
                        | None ->
                            let* () =
                              if t.slots > 1 then
                                (* with one slot every dispatcher serializes on
                                   it, and no wait can race the mark *)
                                Result.map_error in_slot
                                  (lock_identity t tx (Inbox_message.identity message))
                              else Ok ()
                            in
                            let* processed_position, woken =
                              Result.map_error in_slot (mark_processed t tx message)
                            in
                            observer.on_marked
                              { slot; message; processed_position; woken };
                            Ok (Some slot, Outcome.Processed)
                        | Some failure ->
                            let retry_after = t.retries.backoff (message.attempts + 1) in
                            let* attempts, parked =
                              Result.map_error in_slot
                                (record_failure t tx message failure ~retry_after)
                            in
                            (* The log is the observer everyone has: a parked
                               message must not be silent when no observer of
                               ours is attached. An attempt is expected and
                               repeats; parking needs a person. *)
                            let identity =
                              Causal_dependency.to_string (Inbox_message.identity message)
                            in
                            if Failure.is_permanent failure then
                              Log.err (fun m ->
                                  m
                                    "inbox: %s parked, the subscriber's verdict is \
                                     permanent: %s"
                                    identity (Failure.message failure))
                            else if parked then
                              Log.err (fun m ->
                                  m "inbox: %s parked after %d failed attempts: %s"
                                    identity attempts (Failure.message failure))
                            else
                              Log.warn (fun m ->
                                  m "inbox: attempt %d on %s failed, next in %gs: %s"
                                    attempts identity retry_after
                                    (Failure.message failure));
                            observer.on_failed
                              { slot; message; failure; attempts; parked; retry_after };
                            Ok (Some slot, Outcome.Failed { attempts; parked }))))))
  in
  let slot, outcome =
    match outcome with
    | Ok (slot, outcome) -> (slot, Ok outcome)
    | Error (slot, error) -> (slot, Error error)
  in
  observer.on_dispatched { slot; outcome };
  outcome

let run t ~clock ?(loops = Loops.default) ~shutdown subscriber =
  let stop, resolve_stop = Eio.Promise.create () in
  let stopped () = Eio.Promise.is_resolved stop in
  let defect = ref None in
  let pause seconds =
    Eio.Fiber.first
      (fun () -> Eio.Time.Mono.sleep clock seconds)
      (fun () -> Eio.Promise.await stop)
  in
  (* errors of the moment met in a row, for the length of the pause *)
  let rec loop passing =
    if not (stopped ()) then
      match dispatch t subscriber with
      | Ok (Outcome.Processed | Outcome.Failed _ | Outcome.Set_aside) -> loop 0
      | Ok Outcome.Nothing ->
          pause loops.poll_interval;
          loop 0
      | Error error when Error.is_transient error ->
          let passing = passing + 1 in
          let wait = Loops.pause_after loops passing in
          Log.warn (fun m ->
              m "inbox: a loop met an error of the moment, waiting %gs: %a" wait Error.pp
                error);
          pause wait;
          loop passing
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

let parked t session = parked_rows t session

let unpark t session message =
  let* unparked = unpark_row t session message in
  if unparked then t.observer.on_unparked { message };
  Ok unparked

let resolve t session message =
  let* resolved =
    Session.atomic session ~lift:session_error (fun tx ->
        let* () = lock_identity t tx (Inbox_message.identity message) in
        resolve_row t tx message)
  in
  (match resolved with
  | Some (processed_position, woken) ->
      t.observer.on_resolved { message; processed_position; woken }
  | None -> ());
  Ok (Option.is_some resolved)
