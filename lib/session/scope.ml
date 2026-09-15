(** The scope algorithm, written once over a backend that issues the statements. The
    PostgreSQL session and the in-memory session are both instances of {!Make}; what they
    share is everything that makes a scope correct, and what differs is only where the
    statements go. *)

module type BACKEND = sig
  type conn

  val begin_ : conn -> (unit, string) result
  val commit : conn -> (unit, string) result
  val rollback : conn -> (unit, string) result
  val savepoint : conn -> string -> (unit, string) result
  val release : conn -> string -> (unit, string) result
  val rollback_to : conn -> string -> (unit, string) result

  val discard : conn -> unit
  (** The connection may not be used again: its transaction is in an unknown state. A
      pooled connection is disconnected so that the pool drops it. *)
end

module Make (B : BACKEND) = struct
  type shared = {
    conn : B.conn;
    observer : Session_observer.t;
    mutable savepoints : int;
        (** Names come from a counter shared by the whole session tree, not from the
            depth: two sibling scopes must not get the same name. *)
    mutable abandoned : string option;
  }

  type t = { shared : shared; depth : int; mutable scope_open : bool }

  let of_conn ?(observer = Session_observer.none) conn =
    {
      shared = { conn; observer; savepoints = 0; abandoned = None };
      depth = 0;
      scope_open = false;
    }

  let conn t = t.shared.conn
  let depth t = t.depth
  let is_abandoned t = Option.is_some t.shared.abandoned

  let abandon shared reason =
    if Option.is_none shared.abandoned then begin
      shared.abandoned <- Some reason;
      try B.discard shared.conn with _ -> ()
    end

  (* Runs under protection from cancellation: a scope that was cancelled
     must still leave the connection clean, and the rollback is itself a
     suspension point that the cancellation would otherwise cut short. A
     rollback that fails or raises leaves the transaction in an unknown
     state: the session is abandoned. *)
  let roll_back shared savepoint =
    Eio.Cancel.protect (fun () ->
        match
          match savepoint with
          | None -> B.rollback shared.conn
          | Some name -> B.rollback_to shared.conn name
        with
        | Ok () -> ()
        | Error reason -> abandon shared reason
        | exception exn -> abandon shared (Printexc.to_string exn))

  (* A statement that raises rather than returns, cancellation landing inside
     a driver call, leaves the transaction in an unknown state: the session is
     abandoned before the exception goes on. *)
  let abandoning shared statement =
    match statement () with
    | outcome -> outcome
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        abandon shared (Printexc.to_string exn);
        Printexc.raise_with_backtrace exn bt

  let run t ~lift scope =
    let shared = t.shared in
    let depth = t.depth + 1 in
    let savepoint =
      if t.depth = 0 then None
      else begin
        shared.savepoints <- shared.savepoints + 1;
        Some (Printf.sprintf "sp%d" shared.savepoints)
      end
    in
    let kind =
      match savepoint with
      | None -> Session_observer.Transaction
      | Some _ -> Session_observer.Savepoint
    in
    let event : Session_observer.scope = { depth; kind } in
    let opened =
      abandoning shared (fun () ->
          match savepoint with
          | None -> B.begin_ shared.conn
          | Some name -> B.savepoint shared.conn name)
    in
    match opened with
    | Error reason -> Error (lift (Session_error.Begin reason))
    | Ok () -> (
        shared.observer.on_scope_started event;
        let ended outcome = shared.observer.on_scope_ended event outcome in
        let child = { shared; depth; scope_open = false } in
        let outcome =
          match scope child with
          | outcome -> outcome
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              roll_back shared savepoint;
              ended Session_observer.Failed;
              Printexc.raise_with_backtrace exn bt
        in
        match outcome with
        | Ok _ when Option.is_some shared.abandoned ->
            (* A nested scope could not be rolled back: nothing done since
               may be committed, and the connection is already discarded. *)
            ended Session_observer.Failed;
            Error (lift (Session_error.Abandoned (Option.get shared.abandoned)))
        | Ok value -> (
            let closed =
              abandoning shared (fun () ->
                  match savepoint with
                  | None -> B.commit shared.conn
                  | Some name -> B.release shared.conn name)
            in
            match closed with
            | Ok () ->
                ended Session_observer.Succeeded;
                Ok value
            | Error reason ->
                ended Session_observer.Failed;
                Error (lift (Session_error.Commit reason)))
        | Error _ as error ->
            roll_back shared savepoint;
            ended Session_observer.Failed;
            error)

  let atomic t ~lift scope =
    if t.scope_open then Error (lift Session_error.Scope_already_open)
    else
      match t.shared.abandoned with
      | Some reason -> Error (lift (Session_error.Abandoned reason))
      | None ->
          t.scope_open <- true;
          Fun.protect
            ~finally:(fun () -> t.scope_open <- false)
            (fun () -> run t ~lift scope)
end
