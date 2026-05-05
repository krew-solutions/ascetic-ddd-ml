(** Example activity: reserve a hotel room.

    Moderate-risk step in a travel-booking saga — hotel reservations are
    typically cancellable until 24 hours before check-in. Always succeeds
    in this example. *)

let name = "ReserveHotelActivity"
let work_item_queue_address = "sb://./hotelReservations"
let compensation_queue_address = "sb://./hotelCancellations"

(** Deterministic reservation-id generator (parity with Python example). *)
let next_reservation_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter * 6101 mod 100_000

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
