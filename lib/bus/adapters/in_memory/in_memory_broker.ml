module Adapter = Ascetic_bus.Adapter
module Message = Ascetic_bus.Message
module Bus_error = Ascetic_bus.Bus_error
module Bus_uri = Ascetic_bus.Bus_uri
module Subscription = Ascetic_bus.Subscription
module Handling = Ascetic_bus.Handling
module Failure = Ascetic_bus.Failure
module Log = Ascetic_bus.Log

type topic = {
  queue : Message.t Eio.Stream.t;
  (* a group joins with no handler; subscribing gives it one, with the count
     of its calls in flight, which cancelling waits for *)
  groups : (string, (Adapter.handler * Handling.t) option) Hashtbl.t;
}

type t = {
  sw : Eio.Switch.t;
  capacity : int;
  topics : (string, topic) Hashtbl.t;
  (* the registries are shared with the delivery fibers, and with other
     domains if the application has them *)
  mutex : Eio.Mutex.t;
}

let default_capacity = 1024

let create ~sw ?(capacity = default_capacity) () =
  {
    sw;
    capacity = max 1 capacity;
    topics = Hashtbl.create 16;
    mutex = Eio.Mutex.create ();
  }

let locked t f = Eio.Mutex.use_rw ~protect:true t.mutex f

(* The delivery fiber of one topic. A handler's failure, or a handler that
   raises, loses its message, not the topic; a cancellation is not the
   handler's and goes through. *)
let deliver t uri topic =
  let rec loop () =
    let message = Eio.Stream.take topic.queue in
    let handlers =
      locked t (fun () ->
          Hashtbl.fold
            (fun group handler acc ->
              match handler with Some handler -> (group, handler) :: acc | None -> acc)
            topic.groups [])
    in
    List.iter
      (fun (group, ((handler : Adapter.handler), handling)) ->
        (* The call is admitted under the lock the handler is detached under,
           and only if it is still attached: a subscription cancelled since
           the handlers were read is not called, and one cancelled from now
           on waits for this call. *)
        let attached =
          locked t (fun () ->
              match Hashtbl.find_opt topic.groups group with
              | Some (Some (_, current)) when current == handling ->
                  Handling.admit handling;
                  true
              | Some _ | None -> false)
        in
        if attached then
          match Handling.run handling (fun () -> handler message) with
          | Ok () -> ()
          | Error failure ->
              Log.warn (fun m ->
                  m "in-memory[%s/%s]: handler failed: %a" uri group Failure.pp failure)
          | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
          | exception exn ->
              Log.warn (fun m ->
                  m "in-memory[%s/%s]: handler raised: %s" uri group
                    (Printexc.to_string exn)))
      handlers;
    loop ()
  in
  loop ()

(* The topic for the URI, its channel, whatever key the URI carries, started
   on first use. *)
let topic t uri =
  let uri = Bus_uri.without_key uri in
  let topic, started =
    locked t (fun () ->
        match Hashtbl.find_opt t.topics uri with
        | Some topic -> (topic, false)
        | None ->
            let topic =
              { queue = Eio.Stream.create t.capacity; groups = Hashtbl.create 4 }
            in
            Hashtbl.replace t.topics uri topic;
            (topic, true))
  in
  if started then Eio.Fiber.fork_daemon ~sw:t.sw (fun () -> deliver t uri topic);
  topic

let consumer t ~uri ~group : (Adapter.consumer, Bus_error.t) result =
  let topic = topic t uri in
  let joined =
    locked t (fun () ->
        if Hashtbl.mem topic.groups group then false
        else begin
          Hashtbl.replace topic.groups group None;
          true
        end)
  in
  if not joined then
    Error (Bus_error.Already_in_group { uri = Bus_uri.without_key uri; group })
  else
    Ok
      {
        subscribe =
          (fun handler ->
            let handling = Handling.create () in
            locked t (fun () ->
                Hashtbl.replace topic.groups group (Some (handler, handling)));
            Ok
              (Subscription.make
                 ~quiesce:(fun () -> Handling.quiesce handling)
                 (fun () ->
                   locked t (fun () ->
                       (* a later subscription of the group is not this one's
                          to detach *)
                       match Hashtbl.find_opt topic.groups group with
                       | Some (Some (_, current)) when current == handling ->
                           Hashtbl.replace topic.groups group None
                       | Some _ | None -> ()))));
      }

let producer t ~uri : (Adapter.producer, Bus_error.t) result =
  let topic = topic t uri in
  (* the key the producer's URI carries; a message without one gets it *)
  let key = Bus_uri.key uri in
  Ok
    {
      publish =
        (fun message ->
          let message =
            match (key, Message.key message) with
            | Some key, None -> Message.with_key message key
            | _ -> message
          in
          (* waits while the topic's queue is full *)
          Eio.Stream.add topic.queue message;
          Ok ());
    }

let adapter t : Adapter.t = { consumer = consumer t; producer = producer t }
