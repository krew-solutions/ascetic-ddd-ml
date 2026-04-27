(** Host that processes routing slip messages for one activity.

    Forward messages drive [do_work]; backward messages drive [compensate].
    After each step the host forwards the slip to the next address through
    a user-supplied callback (typically a message bus publisher). *)

type send_callback = string -> Routing_slip.t -> unit

type t = {
  factory : Saga_types.factory;
  send : send_callback;
}

let create ~(factory : Saga_types.factory) ~(send : send_callback) : t =
  { factory; send }

let send_to (host : t) (uri_opt : string option) (rs : Routing_slip.t) : unit =
  match uri_opt with
  | None -> ()
  | Some uri -> host.send uri rs

let process_forward_message (host : t) (rs : Routing_slip.t) : unit =
  if not (Routing_slip.is_completed rs) then begin
    if Routing_slip.process_next rs then
      send_to host (Routing_slip.progress_uri rs) rs
    else
      send_to host (Routing_slip.compensation_uri rs) rs
  end

let process_backward_message (host : t) (rs : Routing_slip.t) : unit =
  if Routing_slip.is_in_progress rs then begin
    if Routing_slip.undo_last rs then
      send_to host (Routing_slip.compensation_uri rs) rs
    else
      send_to host (Routing_slip.progress_uri rs) rs
  end

let accept_message (host : t) ~(uri : string) (rs : Routing_slip.t) : bool =
  let activity = host.factory () in
  if String.equal (Activity.compensation_queue_address activity) uri then begin
    process_backward_message host rs;
    true
  end
  else if String.equal (Activity.work_item_queue_address activity) uri then begin
    process_forward_message host rs;
    true
  end
  else false
