(* The detaching is taken by one canceller; whoever finds it taken waits for
   it to be over before it waits for the calls in flight. Without that wait a
   second cancel could find nothing in flight and return while the first was
   still waiting for the lock to detach under, the handler still attached
   (verify/tla/CancelNoWaitForDetach.cfg). *)
type state = Attached of (unit -> unit) | Detaching of unit Eio.Promise.t | Detached
type t = { state : state Atomic.t; quiesce : unit -> unit }

let make ?(quiesce = ignore) detach = { state = Atomic.make (Attached detach); quiesce }

let rec detach t =
  match Atomic.get t.state with
  | Detached -> ()
  | Detaching over ->
      Eio.Promise.await over;
      detach t
  | Attached run as attached ->
      let over, resolve = Eio.Promise.create () in
      if Atomic.compare_and_set t.state attached (Detaching over) then
        begin match run () with
        | () ->
            Atomic.set t.state Detached;
            Eio.Promise.resolve resolve ()
        | exception exn ->
            (* Not detached: cut short, by a cancellation while it waited for
               a lock, say. The next cancel tries again. *)
            let bt = Printexc.get_raw_backtrace () in
            Atomic.set t.state attached;
            Eio.Promise.resolve resolve ();
            Printexc.raise_with_backtrace exn bt
        end
      else detach t

let cancel t =
  detach t;
  t.quiesce ()
