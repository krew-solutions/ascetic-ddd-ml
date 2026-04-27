(** Record of completed work from an activity.

    Stores the activity factory and its result, enabling compensation to be
    performed later if the saga needs to be rolled back. *)

type t = Saga_types.work_log = {
  wl_factory : Saga_types.factory;
  wl_result : Saga_types.result_args;
}

let create ~(activity : Activity.t) ~(result : Work_result.t) : t =
  let factory : Saga_types.factory = fun () -> activity in
  { wl_factory = factory; wl_result = result }

let create_with_factory
      ~(factory : Saga_types.factory)
      ~(result : Work_result.t)
  : t =
  { wl_factory = factory; wl_result = result }

let factory (w : t) : Saga_types.factory = w.wl_factory

let result (w : t) : Work_result.t = w.wl_result

let activity (w : t) : Activity.t = w.wl_factory ()
