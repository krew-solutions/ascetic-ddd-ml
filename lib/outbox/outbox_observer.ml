(** Watching an outbox: what it publishes and what its dispatchers do.

    The same shape as the session observer: a synchronous, infallible record of functions,
    composed with {!all}, fixed when the outbox is built. An observer observes; one that
    has to do I/O hands the event to a queue and lets a fiber of its own do the waiting.

    The five events are the actions of the protocol model in [verify/tla/Outbox.tla]:
    publish, fetch, handle, acknowledge, and the close of the dispatcher's transaction, so
    a recording observer yields a trace the model can be checked against. A dispatcher is
    named by the slot it holds, not by who runs it: it has no other identity. The
    subscriber's error type ['e] reaches the observer as it is, so an observer of the
    application can log or count its own errors. *)

type receipt = {
  transaction_id : int;  (** [pg_current_xact_id()] of the publishing transaction. *)
  position : int;  (** The message's serial. *)
}
(** What the outbox knows about a message once it is written. *)

type published = { message : Outbox_message.t; receipt : receipt }
(** A message was written into the outbox, inside the caller's transaction. *)

type fetched = {
  group : string;
  slot : int option;
      (** The slot whose position row the statement locked; [None] when no slot of the
          selection had visible work. *)
  slots : int;  (** How many slots the table is cut into. *)
  horizon : int;
      (** The visibility horizon of the statement that read the batch, [pg_snapshot_xmin]:
          every transaction below it had ended, so every message of the batch has a
          smaller transaction id. *)
  limit : int;
      (** The most the statement would return: the batch is shorter only when nothing more
          was eligible. *)
  messages : Outbox_message.t list;
      (** The batch, in [(transaction_id, position)] order; empty when there was nothing
          to dispatch. *)
}
(** A dispatcher read a batch, or found no slot with work. *)

type 'e handled = {
  group : string;
  slot : int;
  message : Outbox_message.t;
  outcome : (unit, 'e) result;  (** What the subscriber returned. *)
}
(** The subscriber was handed a message of the batch. *)

type acked = { group : string; slot : int; position : Position.t }
(** The position of the slot moved past the batch, inside the dispatcher's transaction:
    not durable until {!dispatched} reports a commit. *)

type 'e dispatched = {
  group : string;
  slot : int option;
      (** The slot whose batch the transaction held, whether it committed or rolled back;
          [None] when no slot had work, or nothing was taken yet. *)
  outcome : (bool, 'e Outbox_error.t) result;
      (** [Ok true]: a batch was acknowledged; [Ok false]: there was nothing; [Error]: the
          batch was rolled back and will come again. *)
}
(** The dispatcher's transaction closed. *)

type 'e t = {
  on_published : published -> unit;
  on_fetched : fetched -> unit;
  on_handled : 'e handled -> unit;
  on_acked : acked -> unit;
  on_dispatched : 'e dispatched -> unit;
}

(** Observes nothing. *)
let none =
  {
    on_published = ignore;
    on_fetched = ignore;
    on_handled = ignore;
    on_acked = ignore;
    on_dispatched = ignore;
  }

(** Notifies every observer, in order. *)
let all observers =
  {
    on_published = (fun event -> List.iter (fun o -> o.on_published event) observers);
    on_fetched = (fun event -> List.iter (fun o -> o.on_fetched event) observers);
    on_handled = (fun event -> List.iter (fun o -> o.on_handled event) observers);
    on_acked = (fun event -> List.iter (fun o -> o.on_acked event) observers);
    on_dispatched = (fun event -> List.iter (fun o -> o.on_dispatched event) observers);
  }
