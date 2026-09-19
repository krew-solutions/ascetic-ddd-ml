(** Observing a session without taking part in it.

    A signal is a function, and a signal with several subscribers is the composition of
    several functions: {!all} composes observers into one value, {!none} is the neutral
    element. There is no registry to attach to or detach from; the wiring is fixed where
    the pool is built.

    Observers are synchronous and must not raise. They run on the completion path of a
    scope, so one that waited would add its latency to every transaction, and one that
    failed would turn logging into a source of business failures. An observer that has to
    do I/O hands the event to a queue and lets a fiber of its own do the waiting. *)

(** What kind of boundary a scope is. *)
type scope_kind =
  | Session  (** A connection was taken from the pool; no transaction yet. *)
  | Transaction  (** The outermost transaction: [BEGIN]. *)
  | Savepoint  (** A nested transaction: [SAVEPOINT]. *)
  | Logical
      (** A scope with no transaction behind it, as in a REST session: it groups work and
          reports itself, but nothing is committed. *)

(** How a scope ended. What happened on success depends on the kind: a transaction was
    committed, a savepoint released, a session returned. *)
type outcome = Succeeded | Failed

type scope = { depth : int; kind : scope_kind }
(** A scope: its nesting depth, 0 for the session scope and 1 for the outermost
    transaction, and its kind. *)

type t = { on_scope_started : scope -> unit; on_scope_ended : scope -> outcome -> unit }

(** Observes nothing. *)
let none = { on_scope_started = ignore; on_scope_ended = (fun _ _ -> ()) }

(** Notifies every observer, in order. *)
let all observers =
  {
    on_scope_started =
      (fun scope -> List.iter (fun o -> o.on_scope_started scope) observers);
    on_scope_ended =
      (fun scope outcome -> List.iter (fun o -> o.on_scope_ended scope outcome) observers);
  }
