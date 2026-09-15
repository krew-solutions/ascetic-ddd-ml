(** Two pools acting as one: a session scope takes a session from each, left to right, and
    returns them right to left. The sessions it hands out are a pair, the handle of
    {!Composite_session.Make}. *)

module Make
    (A : Ascetic_session.Session_pool.S)
    (B : Ascetic_session.Session_pool.S) : sig
  type t = A.t * B.t

  include
    Ascetic_session.Session_pool.S
      with type t := t
       and type session = A.session * B.session
end
