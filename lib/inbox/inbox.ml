(** PostgreSQL implementation of the Transactional Inbox pattern.

    Stores incoming integration messages in a single table keyed by
    [(tenant_id, stream_type, stream_id, stream_position)], guaranteeing
    idempotency on duplicate submissions. The dispatcher picks the
    earliest-received message whose [causal_dependencies] are already
    processed, runs the subscriber inside a transaction, and stamps the
    row with the next [processed_position].

    See [init.sql] in this directory for the schema and the [README.md]
    for usage. *)

module Uow = Ascetic_unit_of_work.Caqti_unit_of_work
module Provider = Ascetic_unit_of_work.Caqti_connection_provider

type uow = Uow.t

type subscriber = uow -> Inbox_message.t -> (unit, string) result

type t = {
  provider : Provider.t;
  table : string;
  sequence : string;
  partition : Partition_strategy.t;
}

let create ?(table = "inbox") ?(sequence = "inbox_received_position_seq")
    ?(partition = Partition_strategy.uri) ~provider () =
  { provider; table; sequence; partition }

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let caqti_err err = Format.asprintf "%a" Caqti_error.pp err

let exec (module C : Caqti_eio.CONNECTION) req param =
  match C.exec req param with
  | Ok () -> Ok ()
  | Error err -> Error (caqti_err err)

let find_opt (module C : Caqti_eio.CONNECTION) req param =
  match C.find_opt req param with
  | Ok v -> Ok v
  | Error err -> Error (caqti_err err)

let json_to_string j = Yojson.Safe.to_string j

let json_of_string s =
  try Yojson.Safe.from_string s with _ -> `Null

(* -------------------------------------------------------------------------- *)
(* publish — own transaction                                                  *)
(* -------------------------------------------------------------------------- *)

let insert_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "INSERT INTO %s (tenant_id, stream_type, stream_id, stream_position, \
       uri, payload, metadata) \
       VALUES (?, ?, ?::jsonb, ?, ?, ?::jsonb, ?::jsonb) \
       ON CONFLICT (tenant_id, stream_type, stream_id, stream_position) \
       DO NOTHING"
      t.table
  in
  (t7 string string string int string string (option string) ->. unit) sql

let publish t (msg : Inbox_message.t) =
  Provider.with_connection t.provider (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let req = insert_request t in
      let metadata_text = Option.map json_to_string msg.metadata in
      match C.start () with
      | Error e -> Error (caqti_err e)
      | Ok () -> (
          let result =
            exec conn req
              ( msg.tenant_id,
                msg.stream_type,
                json_to_string msg.stream_id,
                msg.stream_position,
                msg.uri,
                json_to_string msg.payload,
                metadata_text )
          in
          match result with
          | Error e ->
              let _ = C.rollback () in
              Error e
          | Ok () -> (
              match C.commit () with
              | Ok () -> Ok ()
              | Error e ->
                  let _ = C.rollback () in
                  Error (caqti_err e))))

(* -------------------------------------------------------------------------- *)
(* fetch + dependency check + mark_processed                                  *)
(* -------------------------------------------------------------------------- *)

let row_type =
  let open Caqti_type in
  (* tenant_id, stream_type, stream_id::text, stream_position, uri,
     payload::text, metadata::text option, received_position,
     processed_position option *)
  t9 string string string int string string (option string) int64
    (option int64)

let row_to_message
    ( tenant_id,
      stream_type,
      stream_id_text,
      stream_position,
      uri,
      payload_text,
      metadata_text,
      received_position,
      processed_position ) : Inbox_message.t =
  {
    tenant_id;
    stream_type;
    stream_id = json_of_string stream_id_text;
    stream_position;
    uri;
    payload = json_of_string payload_text;
    metadata = Option.map json_of_string metadata_text;
    received_position = Some received_position;
    processed_position;
  }

let fetch_unprocessed_request t ~partition_active =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let select_cols =
    Printf.sprintf
      "SELECT tenant_id, stream_type, stream_id::text, stream_position, \
       uri, payload::text, metadata::text, received_position, \
       processed_position FROM %s WHERE processed_position IS NULL"
      t.table
  in
  let lock_clause =
    "ORDER BY received_position ASC LIMIT 1 OFFSET ? \
     FOR UPDATE SKIP LOCKED"
  in
  if partition_active then
    let module P = (val t.partition : Partition_strategy.S) in
    let partition_filter =
      Printf.sprintf "AND hashtext(%s) %% ? = ?" P.sql_expression
    in
    let sql =
      Printf.sprintf "%s %s %s" select_cols partition_filter lock_clause
    in
    `Partition ((t3 int int int ->? row_type) sql)
  else
    let sql = Printf.sprintf "%s %s" select_cols lock_clause in
    `No_partition ((int ->? row_type) sql)

let fetch_unprocessed_message t conn ~offset ~worker_id ~num_workers =
  match fetch_unprocessed_request t ~partition_active:(num_workers > 1) with
  | `No_partition req ->
      Result.map (Option.map row_to_message) (find_opt conn req offset)
  | `Partition req ->
      Result.map
        (Option.map row_to_message)
        (find_opt conn req (num_workers, worker_id, offset))

let dependency_processed_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "SELECT 1 FROM %s WHERE tenant_id = ? AND stream_type = ? \
       AND stream_id = ?::jsonb AND stream_position = ? \
       AND processed_position IS NOT NULL LIMIT 1"
      t.table
  in
  (t4 string string string int ->? int) sql

let is_dependency_processed t conn (dep : Causal_dependency.t) =
  let req = dependency_processed_request t in
  match
    find_opt conn req
      ( dep.tenant_id,
        dep.stream_type,
        json_to_string dep.stream_id,
        dep.stream_position )
  with
  | Ok (Some _) -> Ok true
  | Ok None -> Ok false
  | Error e -> Error e

let are_dependencies_satisfied t conn (msg : Inbox_message.t) =
  let deps = Inbox_message.causal_dependencies msg in
  let rec check = function
    | [] -> Ok true
    | d :: rest -> (
        match is_dependency_processed t conn d with
        | Error e -> Error e
        | Ok false -> Ok false
        | Ok true -> check rest)
  in
  check deps

let rec fetch_next_processable t conn ~offset ~worker_id ~num_workers =
  match fetch_unprocessed_message t conn ~offset ~worker_id ~num_workers with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some msg) -> (
      match are_dependencies_satisfied t conn msg with
      | Error e -> Error e
      | Ok true -> Ok (Some msg)
      | Ok false ->
          fetch_next_processable t conn ~offset:(offset + 1) ~worker_id
            ~num_workers)

let mark_processed_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "UPDATE %s SET processed_position = nextval('%s') \
       WHERE tenant_id = ? AND stream_type = ? \
       AND stream_id = ?::jsonb AND stream_position = ?"
      t.table t.sequence
  in
  (t4 string string string int ->. unit) sql

let mark_processed t conn (msg : Inbox_message.t) =
  let req = mark_processed_request t in
  exec conn req
    ( msg.tenant_id,
      msg.stream_type,
      json_to_string msg.stream_id,
      msg.stream_position )

(* -------------------------------------------------------------------------- *)
(* Transaction wrapper (rollback on Result Error AND on exception)            *)
(* -------------------------------------------------------------------------- *)

let begin_tx (module C : Caqti_eio.CONNECTION) =
  match C.start () with
  | Ok () -> Ok ()
  | Error err -> Error (caqti_err err)

let commit_tx (module C : Caqti_eio.CONNECTION) =
  match C.commit () with
  | Ok () -> Ok ()
  | Error err -> Error (caqti_err err)

let rollback_tx (module C : Caqti_eio.CONNECTION) =
  let _ = C.rollback () in
  ()

let in_transaction conn f =
  match begin_tx conn with
  | Error e -> Error e
  | Ok () ->
      let result =
        try f conn
        with exn ->
          rollback_tx conn;
          raise exn
      in
      match result with
      | Error e ->
          rollback_tx conn;
          Error e
      | Ok v -> (
          match commit_tx conn with
          | Ok () -> Ok v
          | Error e ->
              rollback_tx conn;
              Error e)

(* -------------------------------------------------------------------------- *)
(* dispatch                                                                   *)
(* -------------------------------------------------------------------------- *)

let dispatch ?(worker_id = 0) ?(num_workers = 1) (t : t) (subscriber : subscriber)
    =
  Provider.with_connection t.provider (fun conn ->
      in_transaction conn (fun conn ->
          let uow = Uow.of_connection conn in
          match
            fetch_next_processable t conn ~offset:0 ~worker_id ~num_workers
          with
          | Error e -> Error e
          | Ok None -> Ok false
          | Ok (Some msg) -> (
              match subscriber uow msg with
              | Error e -> Error e
              | Ok () ->
                  Result.map (fun () -> true) (mark_processed t conn msg))))

(* -------------------------------------------------------------------------- *)
(* run                                                                        *)
(* -------------------------------------------------------------------------- *)

let run ?(process_id = 0) ?(num_processes = 1) ?(concurrency = 1)
    ?(poll_interval = 1.0) ?(stop = fun () -> false) (t : t) ~clock
    (subscriber : subscriber) =
  let effective_total = num_processes * concurrency in
  let worker_loop local_id =
    let effective_id = (process_id * concurrency) + local_id in
    let rec loop () =
      if stop () then ()
      else
        match
          dispatch ~worker_id:effective_id ~num_workers:effective_total t
            subscriber
        with
        | Ok true -> loop ()
        | Ok false ->
            if not (stop ()) then Eio.Time.Mono.sleep clock poll_interval;
            if not (stop ()) then loop ()
        | Error _ ->
            if not (stop ()) then Eio.Time.Mono.sleep clock poll_interval;
            if not (stop ()) then loop ()
    in
    loop ()
  in
  if concurrency <= 1 then worker_loop 0
  else Eio.Fiber.all (List.init concurrency (fun i () -> worker_loop i))

(* -------------------------------------------------------------------------- *)
(* setup / cleanup                                                            *)
(* -------------------------------------------------------------------------- *)

let setup (t : t) (uow : uow) =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let exec_sql sql =
    let req = (unit ->. unit) sql in
    exec uow req ()
  in
  let create_seq =
    Printf.sprintf "CREATE SEQUENCE IF NOT EXISTS %s" t.sequence
  in
  let create_tbl =
    Printf.sprintf
      "CREATE TABLE IF NOT EXISTS %s (\n\
      \  tenant_id varchar(128) NOT NULL,\n\
      \  stream_type varchar(128) NOT NULL,\n\
      \  stream_id jsonb NOT NULL,\n\
      \  stream_position integer NOT NULL,\n\
      \  uri varchar(60) NOT NULL,\n\
      \  payload jsonb NOT NULL,\n\
      \  metadata jsonb NULL,\n\
      \  received_position bigint NOT NULL UNIQUE \
       DEFAULT nextval('%s'),\n\
      \  processed_position bigint NULL,\n\
      \  CONSTRAINT %s_pk PRIMARY KEY \
       (tenant_id, stream_type, stream_id, stream_position)\n\
       )"
      t.table t.sequence t.table
  in
  let received_idx =
    Printf.sprintf
      "CREATE INDEX IF NOT EXISTS %s__received_position_idx \
       ON %s (received_position)"
      t.table t.table
  in
  let processed_idx =
    Printf.sprintf
      "CREATE INDEX IF NOT EXISTS %s__processed_position_idx \
       ON %s (processed_position) WHERE processed_position IS NULL"
      t.table t.table
  in
  let event_id_uniq =
    Printf.sprintf
      "CREATE UNIQUE INDEX IF NOT EXISTS %s__event_id_uniq \
       ON %s (((metadata->>'event_id')::uuid))"
      t.table t.table
  in
  let ( let* ) = Result.bind in
  let* () = exec_sql create_seq in
  let* () = exec_sql create_tbl in
  let* () = exec_sql received_idx in
  let* () = exec_sql processed_idx in
  let* () = exec_sql event_id_uniq in
  Ok ()

let cleanup (_ : t) (_ : uow) = Ok ()

(* -------------------------------------------------------------------------- *)
(* Iter — effect-handler async generator yielding (uow, message) pairs        *)
(* -------------------------------------------------------------------------- *)

module Iter = struct
  type _ Effect.t += Yield_msg : (uow * Inbox_message.t) -> unit Effect.t

  type status =
    | Yielded of uow * Inbox_message.t
        * (unit, status) Effect.Deep.continuation
    | Finished

  type state =
    | Initial of (unit -> status)
    | Suspended of (unit, status) Effect.Deep.continuation
    | Closed

  type iter = { mutable state : state }

  exception Closed_iterator

  let body t ~clock ~poll_interval ~stop () =
    let rec poll () =
      if stop () then ()
      else
        let had_message = ref false in
        let _ =
          Provider.with_connection t.provider (fun conn ->
              in_transaction conn (fun conn ->
                  let uow = Uow.of_connection conn in
                  match
                    fetch_next_processable t conn ~offset:0 ~worker_id:0
                      ~num_workers:1
                  with
                  | Error _ -> Ok ()
                  | Ok None -> Ok ()
                  | Ok (Some msg) ->
                      had_message := true;
                      Effect.perform (Yield_msg (uow, msg));
                      Result.map (fun () -> ()) (mark_processed t conn msg)))
        in
        if (not !had_message) && not (stop ()) then
          Eio.Time.Mono.sleep clock poll_interval;
        poll ()
    in
    poll ()

  let start ?(poll_interval = 1.0) ?(stop = fun () -> false) ~clock t =
    let body = body t ~clock ~poll_interval ~stop in
    let resume () =
      let open Effect.Deep in
      match_with body ()
        {
          retc = (fun () -> Finished);
          exnc = raise;
          effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Yield_msg (uow, m) ->
                  Some
                    (fun (k : (a, status) continuation) ->
                      Yielded (uow, m, k))
              | _ -> None);
        }
    in
    { state = Initial resume }

  let next iter =
    match iter.state with
    | Closed -> None
    | Initial resume -> (
        match resume () with
        | Yielded (uow, m, k) ->
            iter.state <- Suspended k;
            Some (uow, m)
        | Finished ->
            iter.state <- Closed;
            None)
    | Suspended k -> (
        match Effect.Deep.continue k () with
        | Yielded (uow, m, k') ->
            iter.state <- Suspended k';
            Some (uow, m)
        | Finished ->
            iter.state <- Closed;
            None)

  let close iter =
    (match iter.state with
    | Suspended k -> (
        try ignore (Effect.Deep.discontinue k Closed_iterator)
        with _ -> ())
    | _ -> ());
    iter.state <- Closed

  let iter ?poll_interval ?stop ~clock t f =
    let it = start ?poll_interval ?stop ~clock t in
    let rec loop () =
      match next it with
      | None -> ()
      | Some (uow, m) -> f uow m; loop ()
    in
    Fun.protect ~finally:(fun () -> close it) loop
end
