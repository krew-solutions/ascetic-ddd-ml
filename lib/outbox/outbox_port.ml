(** The port: what the application layer sees.

    Publishing into the outbox, inside the caller's transaction. This is all the
    application layer needs: it publishes; dispatching is the business of a separate
    process, and lives on the adapter, {!Pg_outbox}. A test double implements this by
    collecting messages. *)

module type S = sig
  type t
  (** An outbox handle, abstract to the application layer. *)

  type uow
  (** The session of the caller's transaction. The adapter pins it to a concrete type,
      [Caqti_session.t]; the application layer treats it as opaque and passes it through.
  *)

  val publish : t -> uow -> Outbox_message.t -> (unit, 'e Outbox_error.t) result
  (** Stores the message within the transaction of [uow], so that it is committed with the
      state change or not at all. *)
end
