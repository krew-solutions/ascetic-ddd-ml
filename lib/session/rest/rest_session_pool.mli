(** Hands out sessions over one shared HTTP client.

    An HTTP client is itself the pool of its connections and is meant to be shared, so one
    client serves every session; taking a session cannot fail. *)

type 'client t

val create : ?observer:Rest_observer.t -> clock:_ Eio.Time.Mono.t -> 'client -> 'client t
(** A pool over the client, observed by nobody unless told otherwise. *)

val session :
  'client t ->
  lift:(Ascetic_session.Session_error.t -> 'e) ->
  ('client Rest_session.t -> ('a, 'e) result) ->
  ('a, 'e) result
(** Runs the scope with a session over the client; the observer sees the session scope
    start and end. *)

val client : 'client t -> 'client
(** The shared client. *)

(** The pool over one type of client, as the port. *)
module Of (Client : sig
  type t
end) :
  Ascetic_session.Session_pool.S
    with type t = Client.t t
     and type session = Client.t Rest_session.t
