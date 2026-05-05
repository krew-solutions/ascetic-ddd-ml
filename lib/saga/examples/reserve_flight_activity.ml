(** Example activity: reserve a flight.

    Highest-risk step in a travel-booking saga — flights typically have
    strict refund policies. The plain [make ()] always succeeds; the
    failing variant [make_failing ()] always fails (used in compensation
    demos and tests). Both report the same [Activity.name] on the wire,
    because in a real distributed deployment the receiving service would
    pick the bound implementation by what is registered in its resolver. *)

let name = "ReserveFlightActivity"
let work_item_queue_address = "sb://./flightReservations"
let compensation_queue_address = "sb://./flightCancellations"

(** Deterministic reservation-id generator (parity with Python example). *)
let next_reservation_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter * 4567 mod 100_000

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

(** Variant that always fails -- used to demonstrate the compensation path
    in [Serialization_example.run_compensation_with_serialization] and in
    the corresponding tests.

    On the wire this activity carries the same [Activity.name] as its
    well-behaved sibling: the difference between "succeeds" and "fails"
    lives only in the implementation bound by the receiving resolver. *)
let make_failing () : Activity.t =
  (* No factory/back-reference needed: [do_work] never produces a
     [Work_log], so nothing on the wire is ever bound to this variant. *)
  let do_work _wi = None in
  let compensate _wl _rs = true in
  Activity.create
    ~name
    ~do_work
    ~compensate
    ~work_item_queue_address
    ~compensation_queue_address

let factory : Saga_types.factory = fun () -> make ()
let factory_failing : Saga_types.factory = fun () -> make_failing ()

let work_item ~(arguments : Work_item_arguments.t) : Work_item.t =
  Work_item.create ~factory ~arguments

let work_item_failing ~(arguments : Work_item_arguments.t) : Work_item.t =
  Work_item.create ~factory:factory_failing ~arguments
