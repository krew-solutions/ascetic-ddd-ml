module Make (A : Ascetic_session.Session_pool.S) (B : Ascetic_session.Session_pool.S) =
struct
  type t = A.t * B.t
  type session = A.session * B.session

  let session (a, b) ~lift scope =
    A.session a ~lift (fun a -> B.session b ~lift (fun b -> scope (a, b)))
end
