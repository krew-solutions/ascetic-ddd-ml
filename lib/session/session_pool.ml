(** A source of sessions.

    A pool hands out a session for the length of a scope and takes the connection back
    afterwards. A session scope is not a transaction: call {!Session.S.atomic} for that.
    The pool is used at the edge, by the composition root or an adapter that serves a
    request; application code receives the session. *)

module type S = sig
  type t
  type session

  val session :
    t -> lift:(Session_error.t -> 'e) -> (session -> ('a, 'e) result) -> ('a, 'e) result
  (** Runs the scope with a session taken from the pool, then returns the connection,
      whether the scope returns or raises. A connection that cannot be taken is
      [Session_error.Acquire], carried by [lift]. *)
end

(** The session scope around an acquired session, written once for every pool: the
    observer sees it start and end. *)
let run ~(observer : Session_observer.t) session scope =
  let event : Session_observer.scope = { depth = 0; kind = Session } in
  observer.on_scope_started event;
  match scope session with
  | outcome ->
      observer.on_scope_ended event
        (if Result.is_ok outcome then Session_observer.Succeeded
         else Session_observer.Failed);
      outcome
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      observer.on_scope_ended event Session_observer.Failed;
      Printexc.raise_with_backtrace exn bt
