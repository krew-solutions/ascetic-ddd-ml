(** The routing slip — the document that flows through the saga.

    A routing slip carries:
    - a queue of pending work items (forward path);
    - a stack of completed work logs (backward path).

    All transaction context lives in the slip itself, which means a slip can
    be serialized and shipped between distributed services. *)

exception Invalid_operation of string

type t = Saga_types.routing_slip = {
  mutable completed_work_logs : Saga_types.work_log list;
  mutable next_work_items : Saga_types.work_item list;
}

let create ?(work_items : Work_item.t list = []) () : t =
  { completed_work_logs = []; next_work_items = work_items }

let is_completed (rs : t) : bool =
  match rs.next_work_items with
  | [] -> true
  | _ -> false

let is_in_progress (rs : t) : bool =
  match rs.completed_work_logs with
  | [] -> false
  | _ -> true

let process_next (rs : t) : bool =
  match rs.next_work_items with
  | [] -> raise (Invalid_operation "No more work items to process")
  | current :: rest ->
    rs.next_work_items <- rest;
    let activity = Work_item.activity current in
    match
      try Activity.do_work activity current
      with _ -> None
    with
    | Some log ->
      rs.completed_work_logs <- rs.completed_work_logs @ [ log ];
      true
    | None -> false

let progress_uri (rs : t) : string option =
  match rs.next_work_items with
  | [] -> None
  | item :: _ ->
    let activity = Work_item.activity item in
    Some (Activity.work_item_queue_address activity)

let compensation_uri (rs : t) : string option =
  match List.rev rs.completed_work_logs with
  | [] -> None
  | last :: _ ->
    let activity = Work_log.activity last in
    Some (Activity.compensation_queue_address activity)

let undo_last (rs : t) : bool =
  match List.rev rs.completed_work_logs with
  | [] -> raise (Invalid_operation "No work to undo")
  | last :: rest_rev ->
    rs.completed_work_logs <- List.rev rest_rev;
    let activity = Work_log.activity last in
    Activity.compensate activity last rs

let completed_work_logs (rs : t) : Work_log.t list = rs.completed_work_logs

let pending_work_items (rs : t) : Work_item.t list = rs.next_work_items
