type 'client t = {
  client : 'client;
  observer : Rest_observer.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

let create ?(observer = Rest_observer.none) ~clock client =
  { client; observer; clock :> Eio.Time.Mono.ty Eio.Resource.t }

let client t = t.client

let session t ~lift:_ scope =
  Ascetic_session.Session_pool.run ~observer:t.observer.scopes
    (Rest_session.create ~observer:t.observer ~clock:t.clock t.client)
    scope

module Of (Client : sig
  type t
end) =
struct
  type nonrec t = Client.t t
  type session = Client.t Rest_session.t

  let session = session
end
