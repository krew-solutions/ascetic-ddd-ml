(** Example activity: reserve a rental car.

    Lowest-risk step in a travel-booking saga — car reservations are
    typically the easiest to cancel. Always succeeds in this example;
    [compensate] is a no-op that reports the (otherwise-discarded)
    reservation id and continues backward. *)

let name = "ReserveCarActivity"
let work_item_queue_address = "sb://./carReservations"
let compensation_queue_address = "sb://./carCancellations"

(** Deterministic reservation-id generator, mirroring Python's
    [random.Random(2)] seed for parity in tests/examples. *)
let next_reservation_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter * 7919 mod 100_000

let make () : Activity.t =
  let activity_ref : Activity.t option ref = ref None in
  let factory : Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work _wi =
    let id = next_reservation_id () in
    let result = Work_result.of_list [ "reservationId", `Int id ] in
    Some (Work_log.create_with_factory ~factory ~result)
  in
  let compensate _wl _rs = true in
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

let factory : Saga_types.factory = fun () -> make ()

let work_item ~(arguments : Work_item_arguments.t) : Work_item.t =
  Work_item.create ~factory ~arguments
