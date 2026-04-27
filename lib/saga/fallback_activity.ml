(** Activity that tries alternative routing slips until one succeeds.

    Based on Section 6 ("Recovery Blocks") of Garcia-Molina & Salem's
    "Sagas" (1987).

    Each alternative is itself a full [Routing_slip] with its own forward
    and backward paths.

    Behaviour:
    - tries each alternative in order;
    - stops on the first success;
    - if an alternative fails, it compensates itself before the next is
      attempted;
    - only the successful alternative needs to be compensated when the
      surrounding saga rolls back.

    The alternatives are captured by a closure rather than stored in
    [Work_item_arguments] (which is required to be JSON-shaped). As a
    consequence, a fallback activity is bound to a single saga instance
    and is not transmitted over the bus. *)

let name = "FallbackActivity"
let work_item_queue_address = "sb://./fallback"
let compensation_queue_address = "sb://./fallbackCompensation"

let execute_alternative (alt : Routing_slip.t) : bool =
  let rec drive () =
    if Routing_slip.is_completed alt then true
    else if Routing_slip.process_next alt then drive ()
    else begin
      while Routing_slip.is_in_progress alt do
        let _ = Routing_slip.undo_last alt in ()
      done;
      false
    end
  in
  drive ()

let make ~(alternatives : Routing_slip.t list) : Activity.t =
  let succeeded : Routing_slip.t option ref = ref None in
  let activity_ref : Activity.t option ref = ref None in
  let factory : Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work _wi =
    let rec try_each = function
      | [] -> None
      | alt :: rest ->
        if execute_alternative alt then begin
          succeeded := Some alt;
          Some (Work_log.create_with_factory ~factory ~result:Work_result.empty)
        end
        else try_each rest
    in
    try_each alternatives
  in
  let compensate _wl _rs =
    (match !succeeded with
     | None -> ()
     | Some alt ->
       while Routing_slip.is_in_progress alt do
         let _ = Routing_slip.undo_last alt in ()
       done);
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

let work_item ~(alternatives : Routing_slip.t list) : Work_item.t =
  let factory : Saga_types.factory = fun () -> make ~alternatives in
  Work_item.create ~factory ~arguments:Work_item_arguments.empty
