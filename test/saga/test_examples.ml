(** Tests for the example activities (Reserve_car/Hotel/Flight). *)

module S = Ascetic_saga
module RS = S.Routing_slip
module Args = S.Work_item_arguments
module Res = S.Work_result
module Car = S.Reserve_car_activity
module Hotel = S.Reserve_hotel_activity
module Flight = S.Reserve_flight_activity

(* Reserve_car_activity ------------------------------------------------ *)

let test_car_do_work_creates_reservation () =
  let activity = Car.make () in
  let wi =
    Car.work_item ~arguments:(Args.of_list [ "vehicleType", `String "Compact" ])
  in
  match S.Activity.do_work activity wi with
  | None -> Alcotest.fail "do_work returned None"
  | Some log ->
    Alcotest.(check bool)
      "reservationId present"
      true
      (Res.find (S.Work_log.result log) "reservationId" <> None)

let test_car_compensate_returns_true () =
  let activity = Car.make () in
  let wi =
    Car.work_item ~arguments:(Args.of_list [ "vehicleType", `String "SUV" ])
  in
  match S.Activity.do_work activity wi with
  | None -> Alcotest.fail "do_work returned None"
  | Some log ->
    Alcotest.(check bool) "compensate ok"
      true
      (S.Activity.compensate activity log (RS.create ()))

let test_car_queue_addresses () =
  let activity = Car.make () in
  Alcotest.(check string)
    "work queue"
    "sb://./carReservations"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./carCancellations"
    (S.Activity.compensation_queue_address activity)

(* Reserve_hotel_activity --------------------------------------------- *)

let test_hotel_do_work_creates_reservation () =
  let activity = Hotel.make () in
  let wi =
    Hotel.work_item ~arguments:(Args.of_list [ "roomType", `String "Suite" ])
  in
  match S.Activity.do_work activity wi with
  | None -> Alcotest.fail "do_work returned None"
  | Some log ->
    Alcotest.(check bool)
      "reservationId present"
      true
      (Res.find (S.Work_log.result log) "reservationId" <> None)

let test_hotel_queue_addresses () =
  let activity = Hotel.make () in
  Alcotest.(check string)
    "work queue"
    "sb://./hotelReservations"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./hotelCancellations"
    (S.Activity.compensation_queue_address activity)

(* Reserve_flight_activity -------------------------------------------- *)

let test_flight_do_work_creates_reservation () =
  let activity = Flight.make () in
  let wi =
    Flight.work_item
      ~arguments:(Args.of_list [ "destination", `String "DUS" ])
  in
  match S.Activity.do_work activity wi with
  | None -> Alcotest.fail "do_work returned None"
  | Some log ->
    Alcotest.(check bool)
      "reservationId present"
      true
      (Res.find (S.Work_log.result log) "reservationId" <> None)

let test_flight_queue_addresses () =
  let activity = Flight.make () in
  Alcotest.(check string)
    "work queue"
    "sb://./flightReservations"
    (S.Activity.work_item_queue_address activity);
  Alcotest.(check string)
    "compensation queue"
    "sb://./flightCancellations"
    (S.Activity.compensation_queue_address activity)

let test_failing_flight_always_returns_none () =
  let activity = Flight.make_failing () in
  let wi =
    Flight.work_item_failing
      ~arguments:(Args.of_list [ "destination", `String "DUS" ])
  in
  Alcotest.(check bool)
    "do_work returns None"
    true
    (S.Activity.do_work activity wi = None)

let test_failing_flight_inherits_queue_addresses () =
  let failing = Flight.make_failing () in
  let normal = Flight.make () in
  Alcotest.(check string)
    "work queue identical"
    (S.Activity.work_item_queue_address normal)
    (S.Activity.work_item_queue_address failing);
  Alcotest.(check string)
    "compensation queue identical"
    (S.Activity.compensation_queue_address normal)
    (S.Activity.compensation_queue_address failing)

let test_all_activities_have_canonical_name () =
  Alcotest.(check string)
    "Car.name" "ReserveCarActivity" Car.name;
  Alcotest.(check string)
    "Hotel.name" "ReserveHotelActivity" Hotel.name;
  Alcotest.(check string)
    "Flight.name" "ReserveFlightActivity" Flight.name;
  Alcotest.(check string)
    "Activity.name (Car)" "ReserveCarActivity"
    (S.Activity.name (Car.make ()));
  Alcotest.(check string)
    "Activity.name (Hotel)" "ReserveHotelActivity"
    (S.Activity.name (Hotel.make ()));
  Alcotest.(check string)
    "Activity.name (Flight)" "ReserveFlightActivity"
    (S.Activity.name (Flight.make ()));
  Alcotest.(check string)
    "Activity.name (failing Flight)" "ReserveFlightActivity"
    (S.Activity.name (Flight.make_failing ()))

(* Travel-booking integration ----------------------------------------- *)

let test_successful_travel_booking () =
  let rs =
    RS.create
      ~work_items:[
        Car.work_item ~arguments:(Args.of_list [ "vehicleType", `String "Compact" ]);
        Hotel.work_item ~arguments:(Args.of_list [ "roomType", `String "Suite" ]);
        Flight.work_item ~arguments:(Args.of_list [ "destination", `String "DUS" ]);
      ]
      ()
  in
  while not (RS.is_completed rs) do
    Alcotest.(check bool) "step ok" true (RS.process_next rs)
  done;
  Alcotest.(check int) "3 logs" 3 (List.length (RS.completed_work_logs rs))

let test_failing_flight_triggers_compensation () =
  let rs =
    RS.create
      ~work_items:[
        Car.work_item ~arguments:(Args.of_list [ "vehicleType", `String "Compact" ]);
        Hotel.work_item ~arguments:(Args.of_list [ "roomType", `String "Suite" ]);
        Flight.work_item_failing
          ~arguments:(Args.of_list [ "destination", `String "DUS" ]);
      ]
      ()
  in
  let aborted = ref false in
  while (not !aborted) && (not (RS.is_completed rs)) do
    if not (RS.process_next rs) then aborted := true
  done;
  Alcotest.(check int) "2 completed before failure" 2
    (List.length (RS.completed_work_logs rs));
  while RS.is_in_progress rs do
    let _ = RS.undo_last rs in ()
  done;
  Alcotest.(check bool) "fully compensated" false (RS.is_in_progress rs)

let () =
  Alcotest.run "Examples"
    [
      ( "Reserve_car_activity",
        [
          Alcotest.test_case "do_work" `Quick test_car_do_work_creates_reservation;
          Alcotest.test_case "compensate" `Quick test_car_compensate_returns_true;
          Alcotest.test_case "queue addresses" `Quick test_car_queue_addresses;
        ] );
      ( "Reserve_hotel_activity",
        [
          Alcotest.test_case "do_work" `Quick test_hotel_do_work_creates_reservation;
          Alcotest.test_case "queue addresses" `Quick test_hotel_queue_addresses;
        ] );
      ( "Reserve_flight_activity",
        [
          Alcotest.test_case "do_work" `Quick test_flight_do_work_creates_reservation;
          Alcotest.test_case "queue addresses" `Quick test_flight_queue_addresses;
          Alcotest.test_case "failing returns None" `Quick
            test_failing_flight_always_returns_none;
          Alcotest.test_case "failing inherits queue addresses" `Quick
            test_failing_flight_inherits_queue_addresses;
        ] );
      ( "naming",
        [
          Alcotest.test_case "all activities have canonical name" `Quick
            test_all_activities_have_canonical_name;
        ] );
      ( "travel booking integration",
        [
          Alcotest.test_case "successful booking" `Quick
            test_successful_travel_booking;
          Alcotest.test_case "failure triggers compensation" `Quick
            test_failing_flight_triggers_compensation;
        ] );
    ]
