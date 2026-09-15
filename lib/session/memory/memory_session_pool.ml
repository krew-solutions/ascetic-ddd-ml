type session = Memory_session.t

type t = {
  journal : Memory_session.Journal.t;
  observer : Ascetic_session.Session_observer.t;
  fail : string -> string option;
}

let create ?(observer = Ascetic_session.Session_observer.none) ?(fail = fun _ -> None) ()
    =
  { journal = Memory_session.Journal.create (); observer; fail }

let journal t = t.journal

let session t ~lift:_ scope =
  let session = Memory_session.create ~observer:t.observer ~fail:t.fail t.journal in
  Ascetic_session.Session_pool.run ~observer:t.observer session scope
