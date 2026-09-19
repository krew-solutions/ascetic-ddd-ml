module Session_error = Ascetic_session.Session_error
module Session_observer = Ascetic_session.Session_observer

type 'client shared = {
  client : 'client;
  observer : Rest_observer.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

type 'client t = { shared : 'client shared; depth : int; mutable scope_open : bool }

let create ?(observer = Rest_observer.none) ~clock client =
  {
    shared = { client; observer; clock :> Eio.Time.Mono.ty Eio.Resource.t };
    depth = 0;
    scope_open = false;
  }

let http t = t.shared.client
let depth t = t.depth

let run t scope =
  let scopes = t.shared.observer.scopes in
  let event : Session_observer.scope = { depth = t.depth + 1; kind = Logical } in
  scopes.on_scope_started event;
  match scope { t with depth = t.depth + 1; scope_open = false } with
  | outcome ->
      scopes.on_scope_ended event
        (if Result.is_ok outcome then Session_observer.Succeeded
         else Session_observer.Failed);
      outcome
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      scopes.on_scope_ended event Session_observer.Failed;
      Printexc.raise_with_backtrace exn bt

let atomic t ~lift scope =
  if t.scope_open then Error (lift Session_error.Scope_already_open)
  else begin
    t.scope_open <- true;
    Fun.protect ~finally:(fun () -> t.scope_open <- false) (fun () -> run t scope)
  end

let request t ~meth ~url call =
  let observer = t.shared.observer in
  let request : Rest_observer.request = { meth; url } in
  observer.on_request_started request;
  let started = Eio.Time.Mono.now t.shared.clock in
  let ended ~failed =
    let elapsed =
      Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now t.shared.clock))
      /. 1e9
    in
    observer.on_request_ended request ~elapsed ~failed
  in
  match call () with
  | outcome ->
      ended ~failed:(Result.is_error outcome);
      outcome
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      ended ~failed:true;
      Printexc.raise_with_backtrace exn bt

module Of (Client : sig
  type t
end) =
struct
  type nonrec t = Client.t t

  let atomic = atomic
end
