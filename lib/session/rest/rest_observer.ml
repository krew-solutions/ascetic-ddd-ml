(** What a REST session reports: its scopes, as every session does, and the requests it
    makes.

    One session both opens scopes and makes requests, so one observer watches both: the
    scopes through the {!Ascetic_session.Session_observer.t} it carries, the requests
    through the two signals beside it. As with every observer here, a signal is a
    function, several subscribers are {!all}, nobody is {!none}; observers are synchronous
    and must not raise. *)

module Session_observer = Ascetic_session.Session_observer

type request = {
  meth : string;  (** The HTTP method. *)
  url : string;  (** The target. *)
}
(** An outbound request. *)

type t = {
  scopes : Session_observer.t;
  on_request_started : request -> unit;  (** A request is about to be made. *)
  on_request_ended : request -> elapsed:float -> failed:bool -> unit;
      (** A request has finished: how long it took, in seconds, and whether the call
          returned an error or raised. *)
}

(** Observes nothing. *)
let none =
  {
    scopes = Session_observer.none;
    on_request_started = ignore;
    on_request_ended = (fun _ ~elapsed:_ ~failed:_ -> ());
  }

(** An observer of the scopes alone. *)
let of_scopes scopes = { none with scopes }

(** Notifies every observer, in order. *)
let all observers =
  {
    scopes = Session_observer.all (List.map (fun o -> o.scopes) observers);
    on_request_started =
      (fun request -> List.iter (fun o -> o.on_request_started request) observers);
    on_request_ended =
      (fun request ~elapsed ~failed ->
        List.iter (fun o -> o.on_request_ended request ~elapsed ~failed) observers);
  }
