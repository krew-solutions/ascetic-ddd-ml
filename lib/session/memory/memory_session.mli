(** An in-memory session, for testing a use case without a database.

    The session records the statements of its scopes into a {!Journal} instead of
    executing them, and a repository double records its own through {!record}, so a test
    asserts on the exact sequence:

    {[
    let journal = Memory_session.Journal.create () in
    let session = Memory_session.create journal in
    let _ =
      Memory_session.atomic session ~lift:Fun.id (fun session ->
          Memory_session.record session "INSERT INTO orders (id) VALUES (7)";
          Ok ())
    in
    assert (
      Memory_session.Journal.entries journal
      = [ "BEGIN"; "INSERT INTO orders (id) VALUES (7)"; "COMMIT" ])
    ]}

    [fail] makes a statement fail with the reason it returns, to test the paths a real
    database takes rarely: a failed commit, a failed rollback. *)

module Journal : sig
  type t

  val create : unit -> t
  val entries : t -> string list
  val clear : t -> unit
end

type t

include Ascetic_session.Session.S with type t := t

val create :
  ?observer:Ascetic_session.Session_observer.t ->
  ?fail:(string -> string option) ->
  Journal.t ->
  t

val journal : t -> Journal.t

val record : t -> string -> unit
(** Records a statement as a repository would run it. *)

val depth : t -> int
val is_abandoned : t -> bool
