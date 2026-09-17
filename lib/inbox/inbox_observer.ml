(** Watching an inbox: what it receives and what its dispatchers do.

    The same shape as the session and outbox observers: a synchronous, infallible record
    of functions, composed with {!all}, fixed when the inbox is built. An observer
    observes; one that has to do I/O hands the event to a queue and lets a fiber of its
    own do the waiting.

    The events are the actions of the protocol model in [verify/tla/Inbox.tla], receive,
    wait, fetch, fail, expire, and the close of the dispatcher's transaction, plus the
    operator's unpark and resolve, with the steps the model folds into one made visible:
    the subscriber's outcome, the mark and what it woke.

    A dispatcher's events name the slot it holds, and nothing else: one dispatcher at a
    time works a slot, so the slot is its identity. A receipt names the transaction that
    stored a row, and every step of a walk over the table carries the {!Snapshot.t} its
    statement ran under, so that a recorded run says which rows each walk could see. *)

type receipt = {
  transaction_id : int;  (** [pg_current_xact_id()] of the storing transaction. *)
  received_position : int;  (** The row's order of arrival. *)
  slot : int;  (** The slot the row landed in, by the table's cut. *)
}
(** What the inbox knows about a row once it is stored. *)

type received = {
  message : Inbox_message.t;
  receipt : receipt option;
      (** Where it landed; [None] when a message of the same identity was already stored
          and this one was ignored. *)
}
(** A message was handed to the inbox, in a transaction of its own. *)

type waiting = {
  slot : int;
  message : Inbox_message.t;
  dependency : Causal_dependency.t;
      (** The first of its dependencies found unprocessed. *)
  snapshot : Snapshot.t;  (** What the statement that returned the row could see. *)
}
(** A dispatcher found the head of its queue depending on a message not yet processed, and
    set it aside to wait for that one, out of the queue; the transaction that marks the
    dependency processed puts it back. *)

type fetched = {
  slot : int option;
      (** The slot the dispatcher holds; [None] when no slot had a due head that nobody
          held. *)
  slots : int;  (** How many slots the table is cut into. *)
  message : Inbox_message.t option;
      (** The row about to be handed to the subscriber; [None] when no slot was taken, or
          when the slot taken had no due head once it was read after the lock. A head set
          aside for a dependency is a {!waiting}, not a fetch. *)
  snapshot : Snapshot.t;
      (** What the statement that returned the row, or found none, could see. *)
}
(** A dispatcher took a row, locked for the length of its transaction; or took nothing. *)

type handled = {
  slot : int;
  message : Inbox_message.t;
  outcome : (unit, Failure.t) result;  (** What the subscriber returned. *)
}
(** The subscriber was given the message and the dispatcher's transaction. *)

type marked = {
  slot : int;
  message : Inbox_message.t;
  processed_position : int;  (** Its order of processing. *)
  woken : Inbox_message.t list;
      (** The rows that waited for this message, back in the queue by the same statement.
      *)
}
(** The message was marked processed, inside the dispatcher's transaction: not durable
    until {!dispatched} reports a commit. *)

type expired = { messages : Inbox_message.t list  (** The rows, oldest first. *) }
(** Rows whose wait for a dependency ran out were parked, with the dependency named in
    their [last_error]. *)

type failed = {
  slot : int;
  message : Inbox_message.t;
  failure : Failure.t;  (** What the subscriber returned, with its verdict. *)
  attempts : int;  (** Failed attempts so far, this one included. *)
  parked : bool;
      (** Whether this attempt was the last: the message is parked, its attempts run out
          or the failure permanent. *)
  retry_after : float;  (** How long the message waits before it may be taken again. *)
}
(** The subscriber failed: its writes were rolled back to the savepoint and the attempt
    was recorded, inside the dispatcher's transaction. Not durable until {!dispatched}
    reports a commit. *)

type dispatched = {
  slot : int option;  (** The slot the dispatcher held; [None] when it took none. *)
  outcome : (Outcome.t, Inbox_error.t) result;
      (** [Ok]: what was committed; [Error]: nothing was, and whatever row was held is
          free again. *)
}
(** The dispatcher's transaction closed. *)

type unparked = { message : Inbox_message.t }
(** An operator gave a parked message another go. *)

type resolved = {
  message : Inbox_message.t;
  processed_position : int;
  woken : Inbox_message.t list;
      (** The rows that waited for this message, back in the queue by the same statement.
      *)
}
(** An operator marked a parked message processed, without the subscriber's effects. *)

type t = {
  on_received : received -> unit;
  on_waiting : waiting -> unit;
  on_fetched : fetched -> unit;
  on_handled : handled -> unit;
  on_marked : marked -> unit;
  on_failed : failed -> unit;
  on_expired : expired -> unit;
  on_dispatched : dispatched -> unit;
  on_unparked : unparked -> unit;
  on_resolved : resolved -> unit;
}

(** Observes nothing. *)
let none =
  {
    on_received = ignore;
    on_waiting = ignore;
    on_fetched = ignore;
    on_handled = ignore;
    on_marked = ignore;
    on_failed = ignore;
    on_expired = ignore;
    on_dispatched = ignore;
    on_unparked = ignore;
    on_resolved = ignore;
  }

(** Notifies every observer, in order. *)
let all observers =
  let each get event = List.iter (fun o -> get o event) observers in
  {
    on_received = each (fun o -> o.on_received);
    on_waiting = each (fun o -> o.on_waiting);
    on_fetched = each (fun o -> o.on_fetched);
    on_handled = each (fun o -> o.on_handled);
    on_marked = each (fun o -> o.on_marked);
    on_failed = each (fun o -> o.on_failed);
    on_expired = each (fun o -> o.on_expired);
    on_dispatched = each (fun o -> o.on_dispatched);
    on_unparked = each (fun o -> o.on_unparked);
    on_resolved = each (fun o -> o.on_resolved);
  }
