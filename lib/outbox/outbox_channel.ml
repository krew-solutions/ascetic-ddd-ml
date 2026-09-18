module Session = Ascetic_session_caqti.Caqti_session
module Message = Ascetic_bus.Message
module Bus_error = Ascetic_bus.Bus_error
module Bus_uri = Ascetic_bus.Bus_uri
module Transactional = Ascetic_bus.Transactional

let scheme = "outbox"
let ( let* ) = Result.bind

let text bytes =
  if String.is_valid_utf_8 bytes then Ok bytes
  else Error (Bus_error.Transport "the outbox keeps headers and keys as UTF-8 text")

let show error = Outbox_error.to_string Ascetic_bus.Failure.to_string error

(* The metadata of a wire message: one string field per header. *)
let metadata_of message =
  let* fields =
    List.fold_right
      (fun (name, value) fields ->
        let* fields = fields in
        let* value = text value in
        Ok ((name, `String value) :: fields))
      (Message.headers message) (Ok [])
  in
  Ok (`Assoc fields)

(* The wire message of a row: payload, key from the destination, headers from
   the metadata plus the destination itself. *)
let wire_of (row : Outbox_message.t) =
  let message = Message.with_header (Message.make row.payload) "destination" row.uri in
  let message =
    match Bus_uri.key row.uri with
    | Some key -> Message.with_key message key
    | None -> message
  in
  match row.metadata with
  | `Assoc fields ->
      List.fold_left
        (fun message (name, value) ->
          let value =
            match value with `String text -> text | other -> Yojson.Safe.to_string other
          in
          Message.with_header message name value)
        message fields
  | _ -> message

let producer outbox ~destination ~encode =
  Transactional.Producer.make
    {
      publish =
        (fun session message ->
          let* destination =
            match (Bus_uri.key destination, Message.key message) with
            | None, Some key ->
                let* key = text key in
                Ok (destination ^ "/" ^ key)
            | _ -> Ok destination
          in
          let* metadata = metadata_of message in
          Result.map_error
            (fun error -> Bus_error.Transport (show error))
            (Pg_outbox.publish outbox session
               (Outbox_message.make ~uri:destination ~payload:(Message.payload message)
                  ~metadata)));
    }
    ~encode

let adapter ~sw ~clock ?(loops = Loops.default) outbox : Ascetic_bus.Adapter.t =
  {
    consumer =
      (fun ~uri:_ ~group ->
        Ok
          {
            subscribe =
              (fun handler ->
                let stop, resolve_stop = Eio.Promise.create () in
                let subscriber row = handler (wire_of row) in
                let rec dispatching () =
                  match
                    Pg_outbox.run outbox ~clock ~loops ~shutdown:stop
                      (Selection.group group) subscriber
                  with
                  | Ok () -> ()
                  | Error error ->
                      Log.warn (fun m ->
                          m "outbox[%s]: dispatch failed, retrying: %s" group (show error));
                      Eio.Fiber.first
                        (fun () -> Eio.Time.Mono.sleep clock loops.poll_interval)
                        (fun () -> Eio.Promise.await stop);
                      if not (Eio.Promise.is_resolved stop) then dispatching ()
                in
                Eio.Fiber.fork_daemon ~sw (fun () ->
                    dispatching ();
                    `Stop_daemon);
                Ok
                  (Ascetic_bus.Subscription.make (fun () ->
                       ignore (Eio.Promise.try_resolve resolve_stop ()))));
          });
    producer =
      (fun ~uri ->
        Error
          (Bus_error.Transport
             (Printf.sprintf
                "`%s`: the outbox publishes only inside a transaction; use \
                 Outbox_channel.producer"
                uri)));
  }
