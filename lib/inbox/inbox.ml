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
module Error = Inbox_port.Error
module Kind = Ascetic_unit_of_work.Caqti_error_kind

type uow = Uow.t

type 'e subscriber = uow -> Inbox_message.t -> (unit, 'e) result

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

let of_caqti err =
  let reason = Caqti_error.show err in
  match Kind.of_error err with
  | Kind.Connection -> Error.Connection reason
  | Kind.Request -> Error.Request reason
  | Kind.Malformed -> Error.Malformed reason

(* The error of an operation that has no subscriber, as the error of any
   operation: its [Subscriber] case is uninhabited. *)
type nothing = |

let lift : nothing Error.t -> 'e Error.t = function
  | Error.Connection reason -> Error.Connection reason
  | Error.Request reason -> Error.Request reason
  | Error.Malformed reason -> Error.Malformed reason
  | Error.Subscriber _ -> .

(* Acquiring a connection and using it are two layers of the provider's
   result; an acquisition failure becomes [Connection] here. *)
let with_connection t f =
  match Provider.with_connection t.provider f with
  | Error acquire -> Error (of_caqti acquire)
  | Ok outcome -> outcome

let exec (module C : Caqti_eio.CONNECTION) req param =
  match C.exec req param with
  | Ok () -> Ok ()
  | Error err -> Error (of_caqti err)

let find_opt (module C : Caqti_eio.CONNECTION) req param =
  match C.find_opt req param with
  | Ok v -> Ok v
  | Error err -> Error (of_caqti err)

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
       VALUES (?, ?, ?::jsonb, ?, ?, ?, ?::jsonb) \
       ON CONFLICT (tenant_id, stream_type, stream_id, stream_position) \
       DO NOTHING"
      t.table
  in
  (t7 string string string int string octets (option string) ->. unit) sql

let publish t (msg : Inbox_message.t) =
  with_connection t (fun conn ->
      let module C = (val conn : Caqti_eio.CONNECTION) in
      let req = insert_request t in
      let metadata_text = Option.map json_to_string msg.metadata in
      match C.start () with
      | Error e -> Error (of_caqti e)
      | Ok () -> (
          let result =
            exec conn req
              ( msg.tenant_id,
                msg.stream_type,
                json_to_string msg.stream_id,
                msg.stream_position,
                msg.uri,
                msg.payload,
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
                  Error (of_caqti e))))

(* -------------------------------------------------------------------------- *)
(* fetch + dependency check + mark_processed                                  *)
(* -------------------------------------------------------------------------- *)

let row_type =
  let open Caqti_type in
  (* tenant_id, stream_type, stream_id::text, stream_position, uri,
     payload, metadata::text option, received_position,
     processed_position option *)
  t9 string string string int string octets (option string) int64
    (option int64)

let row_to_message
    ( tenant_id,
      stream_type,
      stream_id_text,
      stream_position,
      uri,
      payload,
      metadata_text,
      received_position,
      processed_position ) : Inbox_message.t =
  {
    tenant_id;
    stream_type;
    stream_id = json_of_string stream_id_text;
    stream_position;
    uri;
    payload;
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
       uri, payload, metadata::text, received_position, \
       processed_position FROM %s WHERE processed_position IS NULL"
      t.table
  in
  let lock_clause =
    "ORDER BY received_position ASC LIMIT 1 OFFSET ? \
     FOR UPDATE SKIP LOCKED"
  in
  if partition_active then
    let module P = (val t.partition : Partition_strategy.S) in
    (* [hashtext] is a signed [int4] and [%] keeps the sign of its dividend,
       so a negative hash would match no worker and its messages would never
       be processed; clearing the sign bit keeps the remainder in
       [0, num_workers). *)
    let partition_filter =
      Printf.sprintf "AND (hashtext(%s) & 2147483647) %% ? = ?" P.sql_expression
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
  | Error err -> Error (of_caqti err)

let commit_tx (module C : Caqti_eio.CONNECTION) =
  match C.commit () with
  | Ok () -> Ok ()
  | Error err -> Error (of_caqti err)

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

let dispatch ?(worker_id = 0) ?(num_workers = 1) (t : t) (subscriber : 'e subscriber)
    =
  with_connection t (fun conn ->
      in_transaction conn (fun conn ->
          let uow = Uow.of_connection conn in
          match
            fetch_next_processable t conn ~offset:0 ~worker_id ~num_workers
          with
          | Error e -> Error e
          | Ok None -> Ok false
          | Ok (Some msg) -> (
              match subscriber uow msg with
              | Error e -> Error (Error.Subscriber e)
              | Ok () ->
                  Result.map (fun () -> true) (mark_processed t conn msg))))

(* -------------------------------------------------------------------------- *)
(* run                                                                        *)
(* -------------------------------------------------------------------------- *)

(* The first error of any loop stops every loop and is returned: retrying
   is the caller's policy, not the dispatcher's. A loop in the middle of a
   message finishes it first; a loop asleep between messages wakes at
   once. *)
let run ?(process_id = 0) ?(num_processes = 1) ?(concurrency = 1)
    ?(poll_interval = 1.0) ?(stop = fun () -> false) (t : t) ~clock
    (subscriber : 'e subscriber) =
  let effective_total = num_processes * concurrency in
  let failed, resolve_failed = Eio.Promise.create () in
  let stopped () = stop () || Eio.Promise.is_resolved failed in
  let pause () =
    Eio.Fiber.first
      (fun () -> Eio.Time.Mono.sleep clock poll_interval)
      (fun () -> ignore (Eio.Promise.await failed))
  in
  let worker_loop local_id =
    let effective_id = (process_id * concurrency) + local_id in
    let rec loop () =
      if stopped () then ()
      else
        match
          dispatch ~worker_id:effective_id ~num_workers:effective_total t
            subscriber
        with
        | Ok true -> loop ()
        | Ok false ->
            if not (stopped ()) then pause ();
            if not (stopped ()) then loop ()
        | Error e -> ignore (Eio.Promise.try_resolve resolve_failed e)
    in
    loop ()
  in
  (if concurrency <= 1 then worker_loop 0
   else Eio.Fiber.all (List.init concurrency (fun i () -> worker_loop i)));
  match Eio.Promise.peek failed with None -> Ok () | Some e -> Error e

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
      \  uri varchar(255) NOT NULL,\n\
      \  payload bytea NOT NULL,\n\
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
  let message_id_uniq =
    Printf.sprintf
      "CREATE UNIQUE INDEX IF NOT EXISTS %s__message_id_uniq \
       ON %s (((metadata->>'message_id')::uuid))"
      t.table t.table
  in
  let ( let* ) = Result.bind in
  let* () = exec_sql create_seq in
  let* () = exec_sql create_tbl in
  let* () = exec_sql received_idx in
  let* () = exec_sql processed_idx in
  let* () = exec_sql message_id_uniq in
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
    | Finished of (unit, nothing Error.t) result

  type state =
    | Initial of (unit -> status)
    | Suspended of (unit, status) Effect.Deep.continuation
    | Closed

  type iter = { mutable state : state }

  exception Closed_iterator

  (* The generator body. A database failure at any step, from fetching to
     marking a yielded message processed, ends the body with that error;
     nothing is turned into "no messages". *)
  let body t ~clock ~poll_interval ~stop () =
    let ( let* ) = Result.bind in
    let rec poll () =
      if stop () then Ok ()
      else
        let* had_message =
          with_connection t (fun conn ->
              in_transaction conn (fun conn ->
                  let uow = Uow.of_connection conn in
                  let* fetched =
                    fetch_next_processable t conn ~offset:0 ~worker_id:0
                      ~num_workers:1
                  in
                  match fetched with
                  | None -> Ok false
                  | Some msg ->
                      Effect.perform (Yield_msg (uow, msg));
                      let* () = mark_processed t conn msg in
                      Ok true))
        in
        if (not had_message) && not (stop ()) then
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
          retc = (fun outcome -> Finished outcome);
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

  let settle iter = function
    | Yielded (uow, m, k) ->
        iter.state <- Suspended k;
        Ok (Some (uow, m))
    | Finished outcome ->
        iter.state <- Closed;
        Result.map_error lift (Result.map (fun () -> None) outcome)

  let next iter =
    match iter.state with
    | Closed -> Ok None
    | Initial resume -> settle iter (resume ())
    | Suspended k -> settle iter (Effect.Deep.continue k ())

  let close iter =
    (match iter.state with
    | Suspended k -> (
        try ignore (Effect.Deep.discontinue k Closed_iterator)
        with _ -> ())
    | _ -> ());
    iter.state <- Closed

  (* [close] in [finally] rolls the open transaction back when the
     subscriber declines a message, as it does on a raise. *)
  let iter ?poll_interval ?stop ~clock t (subscriber : 'e subscriber) =
    let it = start ?poll_interval ?stop ~clock t in
    let rec loop () =
      match next it with
      | Ok None -> Ok ()
      | Ok (Some (uow, m)) -> (
          match subscriber uow m with
          | Ok () -> loop ()
          | Error e -> Error (Error.Subscriber e))
      | Error e -> Error e
    in
    Fun.protect ~finally:(fun () -> close it) loop
end
