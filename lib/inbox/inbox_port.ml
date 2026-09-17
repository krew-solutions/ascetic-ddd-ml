(** The port: what the receiving side sees.

    This is all a message handler at the edge, a bus subscriber, a webhook, needs: it
    hands the message over and is done. Processing is the business of a separate loop, and
    lives on the adapter, {!Pg_inbox}. A test double implements this by collecting
    messages. *)

module type S = sig
  type t
  (** An inbox handle, abstract to the edge. *)

  val publish : t -> Inbox_message.t -> (unit, Inbox_error.t) result
  (** Stores the message, in a transaction of its own. A message with the identity of one
      already stored is ignored: receiving is idempotent. *)
end
