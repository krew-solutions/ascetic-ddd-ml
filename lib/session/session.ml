(** The session: the transaction boundary as the application layer sees it.

    This is everything the application and domain layers know about a session: an opaque
    handle and one operation, {!S.atomic}, which runs a scope inside a transaction. The
    operation is closed under itself: a nested scope receives another session of the same
    type, and opens a savepoint. Everything the infrastructure needs, the connection, the
    depth, the observer, lives in the interface of the concrete module and is reachable
    only where the type is known, such as a repository that pins
    [type uow = Caqti_session.t]. Application code polymorphic in the session type cannot
    reach any of it.

    The scope chooses its own error type. The session only asks, through [lift], how to
    carry a {!Session_error.t} in it, because opening or closing a scope can fail on its
    own. Apply it once per use case:

    {[
    let atomic = Uow.atomic ~lift:App_error.session in
    atomic session (fun session ->
        let* () = Orders.save session order in
        atomic session (fun session -> Outbox.publish session event))
    ]} *)

module type S = sig
  type t

  val atomic :
    t -> lift:(Session_error.t -> 'e) -> (t -> ('a, 'e) result) -> ('a, 'e) result
  (** Runs the scope inside a transaction: committed when the scope returns [Ok], rolled
      back when it returns [Error] or raises, in which case the exception is re-raised
      once the rollback is done. A nested call opens a savepoint, so a failing nested
      scope leaves the surrounding transaction alive.

      The scope receives a session of its own; opening a second scope on the session that
      opened this one is refused with [Session_error.Scope_already_open]. A scope cut
      short by cancellation is still rolled back; a rollback that fails abandons the
      session, see [Session_error.Abandoned]. *)
end
