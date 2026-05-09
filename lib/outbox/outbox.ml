(** PostgreSQL implementation of the Transactional Outbox pattern.

    Uses [transaction_id] ([xid8]) together with [position] ([BIGSERIAL])
    to provide a total ordering across concurrent transactions:

    - Within a transaction, messages are ordered by [position];
    - Across transactions, by [transaction_id], filtered by
      [pg_snapshot_xmin(pg_current_snapshot())] so that only fully committed
      transactions are visible.

    See [init.sql] for schema details and design rationale. *)

module Uow = Ascetic_unit_of_work.Caqti_unit_of_work

type uow = Uow.t

type subscriber = Outbox_message.t -> (unit, string) result

type t = {
  provider : Ascetic_unit_of_work.Caqti_connection_provider.t;
  outbox_table : string;
  offsets_table : string;
  batch_size : int;
}

let create ?(outbox_table = "outbox") ?(offsets_table = "outbox_offsets")
    ?(batch_size = 100) ~provider () =
  { provider; outbox_table; offsets_table; batch_size }

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

let collect_list (module C : Caqti_eio.CONNECTION) req param =
  match C.collect_list req param with
  | Ok v -> Ok v
  | Error err -> Error (caqti_err err)

let json_to_string j = Yojson.Safe.to_string j

let json_of_string s =
  try Yojson.Safe.from_string s with _ -> `Assoc []

(* -------------------------------------------------------------------------- *)
(* publish                                                                    *)
(* -------------------------------------------------------------------------- *)

let publish_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "INSERT INTO %s (uri, payload, metadata, transaction_id) \
       VALUES (?, ?::jsonb, ?::jsonb, pg_current_xact_id())"
      t.outbox_table
  in
  (t3 string string string ->. unit) sql

let publish t (uow : Uow.t) (msg : Outbox_message.t) =
  let req = publish_request t in
  exec uow req
    (msg.uri, json_to_string msg.payload, json_to_string msg.metadata)

(* -------------------------------------------------------------------------- *)
(* Consumer groups / offsets                                                  *)
(* -------------------------------------------------------------------------- *)

let ensure_consumer_group_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "INSERT INTO %s (consumer_group, uri, offset_acked, \
       last_processed_transaction_id) VALUES (?, ?, 0, '0') \
       ON CONFLICT DO NOTHING"
      t.offsets_table
  in
  (t2 string string ->. unit) sql

let ensure_consumer_group t (uow : Uow.t) ~consumer_group ~uri =
  exec uow (ensure_consumer_group_request t) (consumer_group, uri)

let upsert_offset_request t =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "INSERT INTO %s (consumer_group, uri, offset_acked, \
       last_processed_transaction_id, updated_at) \
       VALUES (?, ?, ?, ?::xid8, CURRENT_TIMESTAMP) \
       ON CONFLICT (consumer_group, uri) DO UPDATE SET \
       offset_acked = EXCLUDED.offset_acked, \
       last_processed_transaction_id = EXCLUDED.last_processed_transaction_id, \
       updated_at = EXCLUDED.updated_at"
      t.offsets_table
  in
  (t4 string string int64 string ->. unit) sql

let ack_message t (uow : Uow.t) ~consumer_group ~uri ~transaction_id ~position =
  exec uow (upsert_offset_request t)
    (consumer_group, uri, position, transaction_id)

let get_position ?(consumer_group = "") ?(uri = "") t (uow : Uow.t) =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let sql =
    Printf.sprintf
      "SELECT last_processed_transaction_id::text, offset_acked \
       FROM %s WHERE consumer_group = ? AND uri = ?"
      t.offsets_table
  in
  let req = (t2 string string ->? t2 string int64) sql in
  match find_opt uow req (consumer_group, uri) with
  | Ok None -> Ok ("0", 0L)
  | Ok (Some (txid, off)) -> Ok (txid, off)
  | Error e -> Error e

let set_position t (uow : Uow.t) ~consumer_group ~uri ~transaction_id ~offset =
  exec uow (upsert_offset_request t)
    (consumer_group, uri, offset, transaction_id)

(* -------------------------------------------------------------------------- *)
(* Fetching                                                                   *)
(* -------------------------------------------------------------------------- *)

(* The fetch query has 4 shapes depending on whether a URI prefix filter
   and/or worker partitioning are active. Each shape gets its own static
   parameter type so Caqti can validate the bindings. *)

let row_type =
  let open Caqti_type in
  (* position, transaction_id::text, uri, payload::text, metadata::text, created_at *)
  t6 int64 string string string string ptime

let row_to_message (position, txid, uri, payload, metadata, created_at) :
    Outbox_message.t =
  {
    uri;
    payload = json_of_string payload;
    metadata = json_of_string metadata;
    created_at = Some created_at;
    position = Some position;
    transaction_id = Some txid;
  }

let base_select t =
  Printf.sprintf
    "SELECT * FROM (\n\
    \  WITH last_processed AS (\n\
    \    SELECT offset_acked, last_processed_transaction_id\n\
    \    FROM %s\n\
    \    WHERE consumer_group = ? AND uri = ?\n\
    \    FOR UPDATE\n\
    \  )\n\
    \  SELECT \"position\", transaction_id::text, uri, payload::text, \
     metadata::text, created_at\n\
    \  FROM %s\n\
    \  WHERE (\n\
    \    (transaction_id = (SELECT last_processed_transaction_id FROM \
     last_processed)\n\
    \     AND \"position\" > (SELECT offset_acked FROM last_processed))\n\
    \    OR\n\
    \    (transaction_id > (SELECT last_processed_transaction_id FROM \
     last_processed))\n\
    \  )\n\
    \  AND transaction_id < pg_snapshot_xmin(pg_current_snapshot())"
    t.offsets_table t.outbox_table

let order_clause t =
  Printf.sprintf
    "\n) AS messages\nORDER BY transaction_id ASC, \"position\" ASC\n\
     LIMIT %d"
    t.batch_size

let fetch_messages t (uow : Uow.t) ~consumer_group ~uri ~worker_id ~num_workers
    =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let base = base_select t in
  let tail = order_clause t in
  match (uri = "", num_workers > 1) with
  | true, false ->
      let sql = base ^ tail in
      let req = (t2 string string ->* row_type) sql in
      Result.map (List.map row_to_message)
        (collect_list uow req (consumer_group, uri))
  | false, false ->
      let sql =
        base ^ "\n  AND (uri = ? OR uri LIKE ?)" ^ tail
      in
      let req = (t4 string string string string ->* row_type) sql in
      let prefix = uri ^ "/%" in
      Result.map (List.map row_to_message)
        (collect_list uow req (consumer_group, uri, uri, prefix))
  | true, true ->
      let sql =
        base ^ "\n  AND hashtext(uri) % ? = ?" ^ tail
      in
      let req = (t4 string string int int ->* row_type) sql in
      Result.map (List.map row_to_message)
        (collect_list uow req (consumer_group, uri, num_workers, worker_id))
  | false, true ->
      let sql =
        base ^ "\n  AND (uri = ? OR uri LIKE ?)\n  AND hashtext(uri) % ? = ?"
        ^ tail
      in
      let req =
        (t6 string string string string int int ->* row_type) sql
      in
      let prefix = uri ^ "/%" in
      Result.map (List.map row_to_message)
        (collect_list uow req
           (consumer_group, uri, uri, prefix, num_workers, worker_id))

(* -------------------------------------------------------------------------- *)
(* Transactional helpers                                                      *)
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

let dispatch ?(consumer_group = "") ?(uri = "") ?(worker_id = 0)
    ?(num_workers = 1) (t : t) (subscriber : subscriber) =
  let effective_group =
    if num_workers > 1 then Printf.sprintf "%s:%d" consumer_group worker_id
    else consumer_group
  in
  (* Ensure the consumer group row exists before we try to FOR UPDATE it. *)
  let ensure_result =
    Ascetic_unit_of_work.Caqti_connection_provider.with_connection t.provider (fun conn ->
        let uow = Uow.of_connection conn in
        ensure_consumer_group t uow ~consumer_group:effective_group ~uri)
  in
  match ensure_result with
  | Error e -> Error e
  | Ok () ->
      Ascetic_unit_of_work.Caqti_connection_provider.with_connection t.provider (fun conn ->
          in_transaction conn (fun conn ->
              let uow = Uow.of_connection conn in
              match
                fetch_messages t uow ~consumer_group:effective_group ~uri
                  ~worker_id ~num_workers
              with
              | Error e -> Error e
              | Ok [] -> Ok false
              | Ok messages -> (
                  let rec process = function
                    | [] -> Ok ()
                    | m :: rest -> (
                        match subscriber m with
                        | Error e -> Error e
                        | Ok () -> process rest)
                  in
                  match process messages with
                  | Error e -> Error e
                  | Ok () ->
                      let last =
                        List.nth messages (List.length messages - 1)
                      in
                      let txid =
                        match last.transaction_id with
                        | Some s -> s
                        | None -> "0"
                      in
                      let pos =
                        match last.position with Some p -> p | None -> 0L
                      in
                      Result.map
                        (fun () -> true)
                        (ack_message t uow ~consumer_group:effective_group
                           ~uri ~transaction_id:txid ~position:pos))))

(* -------------------------------------------------------------------------- *)
(* run                                                                       *)
(* -------------------------------------------------------------------------- *)

let run ?(consumer_group = "") ?(uri = "") ?(process_id = 0)
    ?(num_processes = 1) ?(concurrency = 1) ?(poll_interval = 1.0)
    ?(stop = fun () -> false) (t : t) ~clock (subscriber : subscriber) =
  let effective_total = num_processes * concurrency in
  let worker_loop local_id =
    let effective_id = (process_id * concurrency) + local_id in
    let rec loop () =
      if stop () then ()
      else
        match
          dispatch ~consumer_group ~uri ~worker_id:effective_id
            ~num_workers:effective_total t subscriber
        with
        | Ok true -> loop ()
        | Ok false ->
            (* No work: sleep before polling again, but bail out early
               if [stop] becomes true mid-sleep. *)
            if not (stop ()) then Eio.Time.Mono.sleep clock poll_interval;
            if not (stop ()) then loop ()
        | Error _ ->
            if not (stop ()) then Eio.Time.Mono.sleep clock poll_interval;
            if not (stop ()) then loop ()
    in
    loop ()
  in
  if concurrency <= 1 then worker_loop 0
  else
    Eio.Fiber.all
      (List.init concurrency (fun i () -> worker_loop i))

(* -------------------------------------------------------------------------- *)
(* setup / cleanup                                                            *)
(* -------------------------------------------------------------------------- *)

let setup (t : t) (uow : Uow.t) =
  let open Caqti_request.Infix in
  let open Caqti_type in
  let exec_sql sql =
    let req = (unit ->. unit) sql in
    exec uow req ()
  in
  let outbox_ddl =
    Printf.sprintf
      "CREATE TABLE IF NOT EXISTS %s (\n\
      \  \"position\" BIGSERIAL,\n\
      \  \"uri\" VARCHAR(255) NOT NULL,\n\
      \  \"payload\" JSONB NOT NULL,\n\
      \  \"metadata\" JSONB NOT NULL,\n\
      \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
      \  \"transaction_id\" xid8 NOT NULL,\n\
      \  PRIMARY KEY (\"transaction_id\", \"position\")\n\
       )"
      t.outbox_table
  in
  let position_idx =
    Printf.sprintf
      "CREATE INDEX IF NOT EXISTS %s_position_idx ON %s (\"position\")"
      t.outbox_table t.outbox_table
  in
  let uri_idx =
    Printf.sprintf "CREATE INDEX IF NOT EXISTS %s_uri_idx ON %s (\"uri\")"
      t.outbox_table t.outbox_table
  in
  let event_id_uniq =
    Printf.sprintf
      "CREATE UNIQUE INDEX IF NOT EXISTS %s_event_id_uniq \
       ON %s (((metadata->>'event_id')::uuid))"
      t.outbox_table t.outbox_table
  in
  let offsets_ddl =
    Printf.sprintf
      "CREATE TABLE IF NOT EXISTS %s (\n\
      \  \"consumer_group\" VARCHAR(255) NOT NULL,\n\
      \  \"uri\" VARCHAR(255) NOT NULL DEFAULT '',\n\
      \  \"offset_acked\" BIGINT NOT NULL DEFAULT 0,\n\
      \  \"last_processed_transaction_id\" xid8 NOT NULL DEFAULT '0',\n\
      \  \"updated_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
      \  PRIMARY KEY (\"consumer_group\", \"uri\")\n\
       )"
      t.offsets_table
  in
  let ( let* ) = Result.bind in
  let* () = exec_sql outbox_ddl in
  let* () = exec_sql position_idx in
  let* () = exec_sql uri_idx in
  let* () = exec_sql event_id_uniq in
  let* () = exec_sql offsets_ddl in
  Ok ()

let cleanup (_ : t) (_ : Uow.t) = Ok ()

(* -------------------------------------------------------------------------- *)
(* Iter — async-generator-style iterator using OCaml 5 effect handlers.        *)
(*                                                                            *)
(* Mirrors the Python [Outbox.__aiter__] coroutine: a single batch is fetched *)
(* inside a transaction, each message is yielded to the caller in turn, and  *)
(* its ack runs after control returns to the iterator. The transaction lives *)
(* for the whole batch — same shape as [dispatch], just with per-message ack *)
(* instead of a single ack at the end.                                       *)
(* -------------------------------------------------------------------------- *)

module Iter = struct
  type _ Effect.t += Yield_msg : Outbox_message.t -> unit Effect.t

  type status =
    | Yielded of Outbox_message.t * (unit, status) Effect.Deep.continuation
    | Finished

  type state =
    | Initial of (unit -> status)
    | Suspended of (unit, status) Effect.Deep.continuation
    | Closed

  type iter = { mutable state : state }

  exception Closed_iterator

  let body t ~consumer_group ~uri ~clock ~poll_interval ~stop () =
    let _ =
      Ascetic_unit_of_work.Caqti_connection_provider.with_connection t.provider (fun conn ->
          let uow = Uow.of_connection conn in
          ensure_consumer_group t uow ~consumer_group ~uri)
    in
    let rec poll () =
      if stop () then ()
      else
        let had_messages = ref false in
        let _ =
          Ascetic_unit_of_work.Caqti_connection_provider.with_connection t.provider (fun conn ->
              in_transaction conn (fun conn ->
                  let uow = Uow.of_connection conn in
                  match
                    fetch_messages t uow ~consumer_group ~uri ~worker_id:0
                      ~num_workers:1
                  with
                  | Error _ -> Ok ()
                  | Ok [] -> Ok ()
                  | Ok messages ->
                      had_messages := true;
                      List.iter
                        (fun (m : Outbox_message.t) ->
                          Effect.perform (Yield_msg m);
                          let txid =
                            Option.value m.transaction_id ~default:"0"
                          in
                          let pos =
                            Option.value m.position ~default:0L
                          in
                          ignore
                            (ack_message t uow ~consumer_group ~uri
                               ~transaction_id:txid ~position:pos))
                        messages;
                      Ok ()))
        in
        if (not !had_messages) && not (stop ()) then
          Eio.Time.Mono.sleep clock poll_interval;
        poll ()
    in
    poll ()

  let start ?(consumer_group = "") ?(uri = "") ?(poll_interval = 1.0)
      ?(stop = fun () -> false) ~clock t =
    let body = body t ~consumer_group ~uri ~clock ~poll_interval ~stop in
    let resume () =
      let open Effect.Deep in
      match_with body ()
        {
          retc = (fun () -> Finished);
          exnc = raise;
          effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Yield_msg m ->
                  Some
                    (fun (k : (a, status) continuation) -> Yielded (m, k))
              | _ -> None);
        }
    in
    { state = Initial resume }

  let next iter =
    match iter.state with
    | Closed -> None
    | Initial resume -> (
        match resume () with
        | Yielded (m, k) ->
            iter.state <- Suspended k;
            Some m
        | Finished ->
            iter.state <- Closed;
            None)
    | Suspended k -> (
        match Effect.Deep.continue k () with
        | Yielded (m, k') ->
            iter.state <- Suspended k';
            Some m
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

  let iter ?consumer_group ?uri ?poll_interval ?stop ~clock t f =
    let it = start ?consumer_group ?uri ?poll_interval ?stop ~clock t in
    let rec loop () =
      match next it with None -> () | Some m -> f m; loop ()
    in
    Fun.protect ~finally:(fun () -> close it) loop
end
