(** The port is what a use case depends on; a fake collects what it would have published,
    and the PostgreSQL adapter fits the same signature. No database needed. *)

module Outbox_port = Ascetic_outbox.Outbox_port
module Message = Ascetic_outbox.Outbox_message
module Memory = Ascetic_session_memory.Memory_session
module Memory_pool = Ascetic_session_memory.Memory_session_pool

module Fake : sig
  include Outbox_port.S with type uow = Memory.t

  val create : unit -> t
  val published : t -> string list
end = struct
  type t = string list ref
  type uow = Memory.t

  let create () = ref []
  let published t = List.rev !t

  let publish t _ (message : Message.t) =
    t := message.uri :: !t;
    Ok ()
end

(* The adapter is an instance of the port once the subscriber's error type is
   chosen: the composition root writes this. *)
module Pg :
  Outbox_port.S
    with type t = string Ascetic_outbox.Pg_outbox.t
     and type uow = Ascetic_session_caqti.Caqti_session.t = struct
  type t = string Ascetic_outbox.Pg_outbox.t
  type uow = Ascetic_session_caqti.Caqti_session.t

  let publish = Ascetic_outbox.Pg_outbox.publish
end

let _ = Pg.publish

let test_the_port_is_implementable_without_a_database () =
  let fake = Fake.create () in
  let pool = Memory_pool.create () in
  let lift e = Ascetic_outbox.Outbox_error.Session e in
  let result =
    Memory_pool.session pool ~lift (fun session ->
        Memory.atomic session ~lift (fun tx ->
            Fake.publish fake tx
              (Message.make ~uri:"kafka://orders" ~payload:"1" ~metadata:(`Assoc []))))
  in
  Alcotest.(check bool) "published" true (Result.is_ok result);
  Alcotest.(check (list string)) "collected" [ "kafka://orders" ] (Fake.published fake)

let test_the_pause_doubles_up_to_the_cap () =
  let module Loops = Ascetic_outbox.Loops in
  let loops = { Loops.concurrency = 1; poll_interval = 1.0; max_pause = 10.0 } in
  Alcotest.(check (list (float 0.001)))
    "1, 2, 4, 8, 10, 10"
    [ 1.0; 2.0; 4.0; 8.0; 10.0; 10.0 ]
    (List.map (Loops.pause_after loops) [ 1; 2; 3; 4; 5; 6 ])

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "Outbox port"
    [
      ( "port",
        [
          Alcotest.test_case "the port is implementable without a database" `Quick
            test_the_port_is_implementable_without_a_database;
          Alcotest.test_case "the pause doubles up to the cap" `Quick
            test_the_pause_doubles_up_to_the_cap;
        ] );
    ]
