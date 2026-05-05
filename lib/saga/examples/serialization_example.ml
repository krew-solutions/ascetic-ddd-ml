(** End-to-end demonstration of [Routing_slip] serialization.

    Shows how a saga can be paused on one service, transmitted over a
    message bus as JSON, and resumed on another. Two scenarios:

    - [run_travel_booking_with_serialization]: forward path, one
      round-trip after the first activity, then continue processing on
      the receiving side.
    - [run_compensation_with_serialization]: forward path until a
      deliberate failure, then a round-trip to a "compensation service"
      that runs the backward path.

    The functions print the wire payload so the demo doubles as
    documentation when run interactively. Tests redirect or ignore the
    output. *)

let make_orchestrator_resolver () : Activity_resolver.t =
  let mb = Activity_resolver.Map_based.empty () in
  Activity_resolver.Map_based.register
    mb ~name:Reserve_car_activity.name
    ~factory:Reserve_car_activity.factory;
  Activity_resolver.Map_based.register
    mb ~name:Reserve_hotel_activity.name
    ~factory:Reserve_hotel_activity.factory;
  Activity_resolver.Map_based.register
    mb ~name:Reserve_flight_activity.name
    ~factory:Reserve_flight_activity.factory;
  (* The failing variant is intentionally registered under the same
     canonical name as the well-behaved variant in
     [run_compensation_with_serialization], where the compensation
     service is only meant to roll back -- never re-process. The
     orchestrator-wide resolver here uses the well-behaved factory so
     that incoming forward-path messages succeed by default. Tests
     override the registration when they need the failing variant. *)
  Activity_resolver.Map_based.to_resolver mb

let transmit
      (rs : Routing_slip.t)
      (resolver : Activity_resolver.t)
  : Routing_slip.t =
  match Serializable.to_serializable rs resolver with
  | Error e -> failwith (Printf.sprintf "to_serializable failed: %s" e)
  | Ok srs ->
    let wire = Serializable.to_string srs in
    Printf.printf "---- on the wire ----\n%s\n---------------------\n" wire;
    (match Serializable.of_string wire with
     | Error e -> failwith (Printf.sprintf "of_string failed: %s" e)
     | Ok srs2 ->
       (match Serializable.from_serializable srs2 resolver with
        | Error e ->
          failwith (Printf.sprintf "from_serializable failed: %s" e)
        | Ok rs2 -> rs2))

let run_travel_booking_with_serialization () : Routing_slip.t =
  let resolver = make_orchestrator_resolver () in
  let rs =
    Routing_slip.create
      ~work_items:[
        Reserve_car_activity.work_item
          ~arguments:(Work_item_arguments.of_list [
            "vehicleType", `String "SUV";
            "pickupDate", `String "2024-01-15";
          ]);
        Reserve_hotel_activity.work_item
          ~arguments:(Work_item_arguments.of_list [
            "roomType", `String "Suite";
            "checkInDate", `String "2024-01-15";
          ]);
        Reserve_flight_activity.work_item
          ~arguments:(Work_item_arguments.of_list [
            "destination", `String "LAX";
            "flightDate", `String "2024-01-15";
          ]);
      ]
      ()
  in
  Printf.printf "\n=== Travel booking saga: process car on orchestrator ===\n";
  let _ = Routing_slip.process_next rs in
  Printf.printf "\n=== Hand off to downstream service ===\n";
  let rs = transmit rs resolver in
  Printf.printf "\n=== Resume on downstream service: hotel, then flight ===\n";
  while not (Routing_slip.is_completed rs) do
    let _ = Routing_slip.process_next rs in ()
  done;
  Printf.printf
    "Done. completed=%d, in_progress=%b\n"
    (List.length (Routing_slip.completed_work_logs rs))
    (Routing_slip.is_in_progress rs);
  rs

let run_compensation_with_serialization () : Routing_slip.t =
  (* Build a resolver where the flight activity is the failing variant,
     so the saga aborts at the third step. Both ends share this resolver
     so the wire format is identical to a real distributed deployment. *)
  let mb = Activity_resolver.Map_based.empty () in
  Activity_resolver.Map_based.register
    mb ~name:Reserve_car_activity.name
    ~factory:Reserve_car_activity.factory;
  Activity_resolver.Map_based.register
    mb ~name:Reserve_hotel_activity.name
    ~factory:Reserve_hotel_activity.factory;
  Activity_resolver.Map_based.register
    mb ~name:Reserve_flight_activity.name
    ~factory:Reserve_flight_activity.factory_failing;
  let resolver = Activity_resolver.Map_based.to_resolver mb in
  let rs =
    Routing_slip.create
      ~work_items:[
        Reserve_car_activity.work_item
          ~arguments:(Work_item_arguments.of_list [
            "vehicleType", `String "SUV";
          ]);
        Reserve_hotel_activity.work_item
          ~arguments:(Work_item_arguments.of_list [
            "roomType", `String "Suite";
          ]);
        Reserve_flight_activity.work_item_failing
          ~arguments:(Work_item_arguments.of_list [
            "destination", `String "LAX";
          ]);
      ]
      ()
  in
  Printf.printf "\n=== Compensation saga: run forward path until failure ===\n";
  let aborted = ref false in
  while (not !aborted) && (not (Routing_slip.is_completed rs)) do
    if not (Routing_slip.process_next rs) then begin
      Printf.printf "Forward step failed -- need to compensate\n";
      aborted := true
    end
  done;
  Printf.printf "\n=== Hand off to compensation service ===\n";
  let rs = transmit rs resolver in
  Printf.printf "\n=== Run backward path on compensation service ===\n";
  while Routing_slip.is_in_progress rs do
    let _ = Routing_slip.undo_last rs in ()
  done;
  Printf.printf
    "Done. completed=%d, in_progress=%b\n"
    (List.length (Routing_slip.completed_work_logs rs))
    (Routing_slip.is_in_progress rs);
  rs
