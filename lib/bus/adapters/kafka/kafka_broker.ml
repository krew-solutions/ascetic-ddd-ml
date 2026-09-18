module Adapter = Ascetic_bus.Adapter
module Message = Ascetic_bus.Message
module Failure = Ascetic_bus.Failure
module Bus_error = Ascetic_bus.Bus_error
module Bus_uri = Ascetic_bus.Bus_uri
module Subscription = Ascetic_bus.Subscription
module Log = Ascetic_bus.Log

type t = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  brokers : string list;
  security : Kafka.Security.t;
  properties : (string * string) list;
  offset_reset : Kafka.Consumer.offset_reset;
  send_timeout : float;
  retry_after : float;
  (* one driver handle for every producer of the broker, made on first use *)
  mutable producer : Kafka.Producer.t option;
}

let create ~sw ~clock ?(security = Kafka.Security.default) ?(properties = [])
    ?(offset_reset = Kafka.Consumer.Latest) ?(send_timeout = 30.0) ?(retry_after = 1.0)
    ~brokers () =
  {
    sw;
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    brokers;
    security;
    properties;
    offset_reset;
    send_timeout;
    retry_after;
    producer = None;
  }

let transport error = Bus_error.Transport (Kafka.Error.to_string error)
let ( let* ) = Result.bind

(* The wire message of a record: payload, key and headers as they are. A
   tombstone is an empty payload, a header without a value an empty one. *)
let wire_of (received : Kafka.Consumer.message) =
  let message =
    Message.make
      (match received.value with Some value -> Bytes.to_string value | None -> "")
  in
  let message =
    match received.key with
    | Some key -> Message.with_key message (Bytes.to_string key)
    | None -> message
  in
  List.fold_left
    (fun message (name, value) ->
      Message.with_header message name (Option.value value ~default:""))
    message received.headers

(* The delivery fiber of one subscription. *)
let deliver t ~uri ~group consumer (handler : Adapter.handler) =
  (* A handler that fails is retried until it succeeds: the partition waits,
     which is what keeps its order. *)
  let rec handle message =
    match handler message with
    | Ok () -> ()
    | Error (Failure.Permanent _ as failure) ->
        Log.warn (fun m ->
            m "kafka[%s/%s]: handler failed for good, skipping: %a" uri group Failure.pp
              failure)
    | Error failure ->
        Log.warn (fun m ->
            m "kafka[%s/%s]: handler failed, retrying: %a" uri group Failure.pp failure);
        Eio.Time.sleep t.clock t.retry_after;
        handle message
    | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
    | exception exn ->
        Log.warn (fun m ->
            m "kafka[%s/%s]: handler raised, skipping: %s" uri group
              (Printexc.to_string exn))
  in
  let rec next () =
    match Kafka.Consumer.fetch consumer with
    | Error Kafka.Error.Destroy -> ()
    | Error error ->
        Log.warn (fun m ->
            m "kafka[%s/%s]: receiving failed: %s" uri group (Kafka.Error.to_string error));
        Eio.Time.sleep t.clock t.retry_after;
        next ()
    | Ok received ->
        handle (wire_of received);
        (match Kafka.Consumer.commit consumer received with
        | Ok () -> ()
        | Error error ->
            Log.warn (fun m ->
                m "kafka[%s/%s]: committing the offset failed: %s" uri group
                  (Kafka.Error.to_string error)));
        next ()
  in
  next ()

let consumer t ~uri ~group : (Adapter.consumer, Bus_error.t) result =
  let* topic = Bus_uri.channel uri in
  let* consumer =
    Result.map_error transport
      (Kafka.Consumer.create ~clock:t.clock
         {
           brokers = t.brokers;
           group_id = group;
           topics = [ topic ];
           offset_reset = t.offset_reset;
           auto_commit = false;
           security = t.security;
           properties = t.properties;
         }
         ~sw:t.sw)
  in
  (* the delivery fiber, while a handler is attached *)
  let delivery = ref None in
  let stop () =
    Option.iter (fun context -> Eio.Cancel.cancel context Exit) !delivery;
    delivery := None
  in
  Ok
    {
      Adapter.subscribe =
        (fun handler ->
          (* a previous delivery is stopped first *)
          stop ();
          Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
              (try
                 Eio.Cancel.sub (fun context ->
                     delivery := Some context;
                     deliver t ~uri ~group consumer handler)
               with Eio.Cancel.Cancelled _ -> ());
              `Stop_daemon);
          let mine = !delivery in
          Ok
            (Subscription.make (fun () ->
                 Option.iter (fun context -> Eio.Cancel.cancel context Exit) mine;
                 if !delivery == mine then delivery := None)));
    }

let shared_producer t =
  match t.producer with
  | Some producer -> Ok producer
  | None ->
      let* producer =
        Result.map_error transport
          (Kafka.Producer.create
             {
               brokers = t.brokers;
               delivery_mode = Kafka.Producer.At_least_once;
               linger_ms = None;
               security = t.security;
               properties = t.properties;
             }
             ~sw:t.sw)
      in
      t.producer <- Some producer;
      Ok producer

let producer t ~uri : (Adapter.producer, Bus_error.t) result =
  let* topic = Bus_uri.channel uri in
  let* producer = shared_producer t in
  (* the key the producer's URI carries; a message without one gets it *)
  let uri_key = Bus_uri.key uri in
  Ok
    {
      Adapter.publish =
        (fun message ->
          let key =
            match Message.key message with Some key -> Some key | None -> uri_key
          in
          let delivered =
            Kafka.Producer.produce_await producer ~topic
              ~value:(Some (Bytes.of_string (Message.payload message)))
              ?key:(Option.map Bytes.of_string key)
              ~headers:
                (List.map
                   (fun (name, value) -> (name, Some value))
                   (Message.headers message))
              ()
          in
          match
            Eio.Time.with_timeout t.clock t.send_timeout (fun () ->
                Ok (Eio.Promise.await delivered))
          with
          | Ok (Ok ()) -> Ok ()
          | Ok (Error error) -> Error (transport error)
          | Error `Timeout ->
              Error
                (Bus_error.Transport
                   (Printf.sprintf "the broker did not acknowledge `%s` in %gs" topic
                      t.send_timeout)));
    }

let adapter t : Adapter.t = { consumer = consumer t; producer = producer t }
