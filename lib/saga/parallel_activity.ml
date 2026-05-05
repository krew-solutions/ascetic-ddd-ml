(** Activity that runs multiple routing slips as a fork/join group.

    Based on Section 8 of Garcia-Molina & Salem's "Sagas" (1987).

    Each branch is itself a full [Routing_slip] with its own forward and
    backward paths.

    Behaviour:
    - drives every branch routing slip concurrently (Eio fibers);
    - fail-fast: when at least one branch fails, every branch (completed or
      partial) is compensated;
    - on rollback of the surrounding saga, every branch is compensated.

    The branches are captured by a closure rather than stored in
    [Work_item_arguments] (which is required to be JSON-shaped). The
    resulting activity is therefore bound to a single saga instance and
    is not transmitted over the bus.

    Concurrency requires an Eio runtime: callers must invoke
    [Routing_slip.process_next] from within an [Eio_main.run] (or other
    Eio backend) so that [Eio.Fiber.List.map] / [Eio.Fiber.List.iter]
    can fork fibers. *)

let name = "ParallelActivity"
let work_item_queue_address = "sb://./parallel"
let compensation_queue_address = "sb://./parallelCompensation"

let execute_branch (branch : Routing_slip.t) : bool =
  let rec drive () =
    if Routing_slip.is_completed branch then true
    else if Routing_slip.process_next branch then drive ()
    else begin
      while Routing_slip.is_in_progress branch do
        let _ = Routing_slip.undo_last branch in ()
      done;
      false
    end
  in
  drive ()

let compensate_branches (branches : Routing_slip.t list) : unit =
  Eio.Fiber.List.iter
    (fun branch ->
      while Routing_slip.is_in_progress branch do
        let _ = Routing_slip.undo_last branch in ()
      done)
    branches

let make ~(branches : Routing_slip.t list) : Activity.t =
  let activity_ref : Activity.t option ref = ref None in
  let factory : Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work _wi =
    let outcomes = Eio.Fiber.List.map execute_branch branches in
    let all_succeeded = List.for_all (fun ok -> ok) outcomes in
    if all_succeeded then
      Some (Work_log.create_with_factory ~factory ~result:Work_result.empty)
    else begin
      compensate_branches branches;
      None
    end
  in
  let compensate _wl _rs =
    compensate_branches branches;
    true
  in
  let activity =
    Activity.create
      ~name
      ~do_work
      ~compensate
      ~work_item_queue_address
      ~compensation_queue_address
  in
  activity_ref := Some activity;
  activity

let work_item ~(branches : Routing_slip.t list) : Work_item.t =
  let factory : Saga_types.factory = fun () -> make ~branches in
  Work_item.create ~factory ~arguments:Work_item_arguments.empty
