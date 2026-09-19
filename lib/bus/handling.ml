(* The state is under a mutex of the standard library, not of Eio: the
   sections touch memory and nothing else, so no fiber waits inside one, and a
   call that ends by a cancellation must still be counted out, which a lock
   that can be cancelled would not let it do. A waiter is a promise, resolved
   when the count comes to nothing: a wake-up cannot be missed. *)
type t = {
  mutex : Mutex.t;
  mutable in_flight : int;
  mutable waiting : unit Eio.Promise.u list;
  inside : unit Eio.Fiber.key;
}

let create () =
  {
    mutex = Mutex.create ();
    in_flight = 0;
    waiting = [];
    inside = Eio.Fiber.create_key ();
  }

let admit t = Mutex.protect t.mutex (fun () -> t.in_flight <- t.in_flight + 1)

let leave t =
  let woken =
    Mutex.protect t.mutex (fun () ->
        t.in_flight <- t.in_flight - 1;
        if t.in_flight > 0 then []
        else begin
          let waiting = t.waiting in
          t.waiting <- [];
          waiting
        end)
  in
  List.iter (fun waiter -> Eio.Promise.resolve waiter ()) woken

let run t call =
  Fun.protect
    ~finally:(fun () -> leave t)
    (fun () -> Eio.Fiber.with_binding t.inside () call)

let call t f =
  admit t;
  run t f

let quiesce t =
  if Option.is_none (Eio.Fiber.get t.inside) then
    let idle =
      Mutex.protect t.mutex (fun () ->
          if t.in_flight = 0 then None
          else begin
            let idle, resolve = Eio.Promise.create () in
            t.waiting <- resolve :: t.waiting;
            Some idle
          end)
    in
    Option.iter Eio.Promise.await idle

let loop ~sw body =
  let t = create () in
  let stop, resolve_stop = Eio.Promise.create () in
  (* Admitted here, not in the daemon: a cancel that comes before the daemon
     has run must wait for it all the same. *)
  admit t;
  (match
     Eio.Fiber.fork_daemon ~sw (fun () ->
         run t (fun () -> body ~stop);
         `Stop_daemon)
   with
  | () -> ()
  | exception exn ->
      (* The switch was over already: there is no loop. *)
      let bt = Printexc.get_raw_backtrace () in
      leave t;
      Printexc.raise_with_backtrace exn bt);
  Subscription.make
    ~quiesce:(fun () -> quiesce t)
    (fun () -> ignore (Eio.Promise.try_resolve resolve_stop ()))
