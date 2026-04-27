(** Internal mutually-recursive types for the saga module.

    [Activity], [Work_item], [Work_log] and [Routing_slip] reference each
    other, so their definitions must live together. Each public module then
    re-exports the relevant nominal type and provides constructors and
    accessors. *)

type value = Yojson.Safe.t

type arguments = (string * value) list
type result_args = (string * value) list

type activity = {
  activity_name : string;
  do_work : work_item -> work_log option;
  compensate : work_log -> routing_slip -> bool;
  work_item_queue_address : string;
  compensation_queue_address : string;
}

and factory = unit -> activity

and work_item = {
  wi_factory : factory;
  wi_arguments : arguments;
}

and work_log = {
  wl_factory : factory;
  wl_result : result_args;
}

and routing_slip = {
  mutable completed_work_logs : work_log list;
  mutable next_work_items : work_item list;
}
