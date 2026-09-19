(** A session over an HTTP client.

    The same shape as a session over a database, with no transaction behind it: a scope
    groups work and reports itself as [Logical], and does not pretend that HTTP calls can
    be rolled back. The client is a type parameter, so this library depends on no HTTP
    library; a request is timed by wrapping the call that makes it, {!request}, which is
    why any client works and nothing is hidden.

    The client and {!request} are the capability "this session speaks HTTP": a gateway
    asks for a ['client t] where the client's type is known, which is the infrastructure
    layer; the application layer sees {!Ascetic_session.Session.S}, through {!Of}. *)

type 'client t

val create : ?observer:Rest_observer.t -> clock:_ Eio.Time.Mono.t -> 'client -> 'client t
(** A session at depth 0 over the client, observed by nobody unless told otherwise. The
    clock times the requests. *)

val atomic :
  'client t ->
  lift:(Ascetic_session.Session_error.t -> 'e) ->
  ('client t -> ('a, 'e) result) ->
  ('a, 'e) result
(** Runs the scope, reporting it to the observer: [Succeeded] when it returns [Ok],
    [Failed] when it returns [Error] or raises, in which case the exception goes on.
    Nothing is committed or rolled back. The scope receives a session of its own; opening
    a second scope on the session that opened this one is refused with
    [Session_error.Scope_already_open], as for every session. *)

val http : 'client t -> 'client
(** The client this session carries. *)

val request :
  'client t -> meth:string -> url:string -> (unit -> ('a, 'e) result) -> ('a, 'e) result
(** Times an outbound call and reports it to the observer. The call is made by the caller,
    with the client of {!http}; this only wraps it. A call that raises is reported as
    failed, and the exception goes on. *)

val depth : 'client t -> int
(** Number of scopes open around this session: 0 outside any. *)

(** The session over one type of client, as the port: what code polymorphic in the session
    is given. *)
module Of (Client : sig
  type t
end) : Ascetic_session.Session.S with type t = Client.t t
