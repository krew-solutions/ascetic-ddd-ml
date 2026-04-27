(** Saga activity.

    Each activity encapsulates two operations:
    - [do_work]: performs the actual business operation;
    - [compensate]: reverses the operation if the saga fails.

    Activities are executed by [Activity_host] and their results are tracked
    in the [Routing_slip]. *)

type t = Saga_types.activity = {
  activity_name : string;
  do_work : Saga_types.work_item -> Saga_types.work_log option;
  compensate : Saga_types.work_log -> Saga_types.routing_slip -> bool;
  work_item_queue_address : string;
  compensation_queue_address : string;
}

type factory = Saga_types.factory

let create
      ~name
      ~do_work
      ~compensate
      ~work_item_queue_address
      ~compensation_queue_address
  : t =
  {
    activity_name = name;
    do_work;
    compensate;
    work_item_queue_address;
    compensation_queue_address;
  }

let name (a : t) : string = a.activity_name

let do_work (a : t) (wi : Saga_types.work_item) : Saga_types.work_log option =
  a.do_work wi

let compensate
      (a : t)
      (wl : Saga_types.work_log)
      (rs : Saga_types.routing_slip)
  : bool =
  a.compensate wl rs

let work_item_queue_address (a : t) : string = a.work_item_queue_address

let compensation_queue_address (a : t) : string = a.compensation_queue_address
