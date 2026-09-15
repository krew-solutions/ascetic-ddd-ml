module Make (A : Ascetic_session.Session.S) (B : Ascetic_session.Session.S) = struct
  type t = A.t * B.t

  (* The first delegate is the outer scope: opened first, closed last. Both
     delegates carry the scope's error type through the same [lift]. *)
  let atomic (a, b) ~lift scope =
    A.atomic a ~lift (fun a -> B.atomic b ~lift (fun b -> scope (a, b)))
end
