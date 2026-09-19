module Session = Ascetic_session_caqti.Caqti_session
module Message = Ascetic_bus.Message
module Bus_error = Ascetic_bus.Bus_error
module Bus_uri = Ascetic_bus.Bus_uri
module Transactional = Ascetic_bus.Transactional

let scheme = "inbox"
let ( let* ) = Result.bind

(* The headers that are columns of the inbox: its identity, and the channel the
   message was sent to, which the outbox stamps. *)
let tenant_id = "tenant_id"
let stream_type = "stream_type"
let stream_id = "stream_id"
let stream_position = "stream_position"
let destination = "destination"
let columns = [ tenant_id; stream_type; stream_id; stream_position; destination ]
let malformed reason = Bus_error.Transport reason

let text bytes =
  if String.is_valid_utf_8 bytes then Ok bytes
  else Error (malformed "the inbox keeps headers and keys as UTF-8 text")

let json_of_text text =
  match Yojson.Safe.from_string text with
  | `Float f when not (Float.is_finite f) -> None
  | json -> Some json
  | exception Yojson.Json_error _ -> None

(* A header's text as a metadata field: structured again when it is a JSON
   array or object, text otherwise. *)
let structured text : Yojson.Safe.t =
  match json_of_text text with
  | Some ((`List _ | `Assoc _) as json) -> json
  | _ -> `String text

(* A metadata value as header text: a string as it is, anything else as JSON. *)
let header_text : Yojson.Safe.t -> string = function
  | `String text -> text
  | other -> Yojson.Safe.to_string other

(* The row a wire message becomes, published to the channel. *)
let inbox_message_of ~channel message =
  let column name =
    match Message.header message name with
    | Some value -> text value
    | None -> Error (malformed (Printf.sprintf "the inbox needs a `%s` header" name))
  in
  let* id = column stream_id in
  let id = Option.value (json_of_text id) ~default:(`String id) in
  let* position = column stream_position in
  let* position =
    Option.to_result
      ~none:
        (malformed (Printf.sprintf "`%s` is not an integer: %s" stream_position position))
      (int_of_string_opt position)
  in
  let* uri =
    match Message.header message destination with
    | Some destination -> text destination
    | None -> (
        let* key =
          match Message.key message with
          | Some key -> Result.map Option.some (text key)
          | None -> Ok (Bus_uri.key channel)
        in
        match key with
        | Some key -> Ok (Bus_uri.without_key channel ^ "/" ^ key)
        | None -> Ok channel)
  in
  let* metadata =
    List.fold_right
      (fun (name, value) fields ->
        let* fields = fields in
        if List.mem name columns then Ok fields
        else
          let* value = text value in
          Ok ((name, structured value) :: fields))
      (Message.headers message) (Ok [])
  in
  let* tenant = column tenant_id in
  let* kind = column stream_type in
  Ok
    (Inbox_message.with_metadata
       (Inbox_message.make ~tenant_id:tenant ~stream_type:kind ~stream_id:id
          ~stream_position:position ~uri ~payload:(Message.payload message))
       (`Assoc metadata))

(* The wire message of a row: payload, key from the URI, the columns and the
   metadata as headers. *)
let wire_of (row : Inbox_message.t) =
  let header name value message = Message.with_header message name value in
  let message =
    Message.make row.payload
    |> header tenant_id row.tenant_id
    |> header stream_type row.stream_type
    |> header stream_id (header_text row.stream_id)
    |> header stream_position (string_of_int row.stream_position)
    |> header destination row.uri
  in
  let message =
    match Bus_uri.key row.uri with
    | Some key -> Message.with_key message key
    | None -> message
  in
  match row.metadata with
  | Some (`Assoc fields) ->
      List.fold_left
        (fun message (name, value) -> header name (header_text value) message)
        message fields
  | _ -> message

let adapter inbox : Ascetic_bus.Adapter.t =
  {
    consumer =
      (fun ~uri ~group:_ ->
        Error
          (malformed
             (Printf.sprintf
                "`%s`: the inbox hands the handler its transaction; use \
                 Inbox_channel.consumer"
                uri)));
    producer =
      (fun ~uri ->
        Ok
          {
            publish =
              (fun message ->
                let* row = inbox_message_of ~channel:uri message in
                Result.map_error
                  (fun error -> malformed (Inbox_error.to_string error))
                  (Pg_inbox.publish inbox row));
          });
  }

let consumer ~sw ~clock ?(loops = Loops.default) inbox ~decode =
  Transactional.Consumer.make
    {
      subscribe =
        (fun handler ->
          (* A handler's error is a failure of the moment, unless the bus
             carries the one verdict it knows: permanent, from a stage or a
             handler that can tell, and the message is parked at once. *)
          let subscriber tx row =
            match handler tx (wire_of row) with
            | Ok () -> Ok ()
            | Error (Ascetic_bus.Failure.Permanent reason) ->
                Error (Failure.Permanent reason)
            | Error (Ascetic_bus.Failure.Transient reason) ->
                Error (Failure.Transient reason)
          in
          (* Told to stop, [run] lets its loops finish the message they have
             in hand, mark it, commit and give the connection back, and
             returns; cancelling the subscription waits for that. *)
          let rec processing ~stop =
            match Pg_inbox.run inbox ~clock ~loops ~shutdown:stop subscriber with
            | Ok () -> ()
            | Error error ->
                Log.warn (fun m ->
                    m "inbox: processing failed, retrying: %s"
                      (Inbox_error.to_string error));
                Eio.Fiber.first
                  (fun () -> Eio.Time.Mono.sleep clock loops.poll_interval)
                  (fun () -> Eio.Promise.await stop);
                if not (Eio.Promise.is_resolved stop) then processing ~stop
          in
          Ok (Ascetic_bus.Handling.loop ~sw processing));
    }
    ~decode
