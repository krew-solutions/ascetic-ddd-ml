type session = Caqti_session.t

type t = {
  acquire : 'a. ((module Caqti_eio.CONNECTION) -> 'a) -> ('a, string) result;
  observer : Ascetic_session.Session_observer.t;
}

let of_connection ?(observer = Ascetic_session.Session_observer.none) conn =
  { acquire = (fun f -> Ok (f conn)); observer }

let of_pool ?(observer = Ascetic_session.Session_observer.none) pool =
  {
    acquire =
      (fun f ->
        Result.map_error Caqti_error.show
          (Caqti_eio.Pool.use (fun conn -> Ok (f conn)) pool));
    observer;
  }

let session t ~lift scope =
  match
    t.acquire (fun conn ->
        let session = Caqti_session.create ~observer:t.observer conn in
        Ascetic_session.Session_pool.run ~observer:t.observer session scope)
  with
  | Error reason -> Error (lift (Ascetic_session.Session_error.Acquire reason))
  | Ok outcome -> outcome
