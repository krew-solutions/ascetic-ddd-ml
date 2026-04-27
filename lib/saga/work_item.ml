(** A unit of work to be processed by a specific activity.

    Carries the activity factory and the arguments needed to perform the
    work. *)

type t = Saga_types.work_item = {
  wi_factory : Saga_types.factory;
  wi_arguments : Saga_types.arguments;
}

let create ~(factory : Saga_types.factory) ~(arguments : Work_item_arguments.t)
  : t =
  { wi_factory = factory; wi_arguments = arguments }

let factory (w : t) : Saga_types.factory = w.wi_factory

let arguments (w : t) : Work_item_arguments.t = w.wi_arguments

let activity (w : t) : Activity.t = w.wi_factory ()
