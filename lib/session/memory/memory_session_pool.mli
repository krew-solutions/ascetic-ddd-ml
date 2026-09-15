(** Hands out in-memory sessions that share one journal. *)

type t

include
  Ascetic_session.Session_pool.S with type t := t and type session = Memory_session.t

val create :
  ?observer:Ascetic_session.Session_observer.t ->
  ?fail:(string -> string option) ->
  unit ->
  t

val journal : t -> Memory_session.Journal.t
