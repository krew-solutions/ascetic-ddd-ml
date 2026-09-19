(** The calls of one subscription's handler that are in flight, so that
    {!Subscription.cancel} can wait for them.

    A transport calls a handler from fibers of its own. Detaching the handler stops the
    calls to come; a call already made goes on, and with it whatever it holds: a
    transaction, a connection, a message not yet acknowledged. A transport keeps one of
    these per subscription, makes every call of the handler through it, and ends its
    [cancel] with {!quiesce}: then a subscription cancelled is a handler that is not
    running and will not be run.

    From inside a call, {!quiesce} returns at once: a handler that cancels its own
    subscription would otherwise wait for itself. That a fiber is inside a call is told by
    a fiber-local mark, which the fibers a handler forks inherit. *)

type t

val create : unit -> t
(** Nothing in flight. *)

val admit : t -> unit
(** One more call is in flight. For a transport that decides to make a call under a lock
    of its own, the one it detaches handlers under: admitted there, a call is either seen
    by {!quiesce} or not made. Every [admit] is followed by one {!run}. *)

val run : t -> (unit -> 'a) -> 'a
(** Makes a call that was admitted; when it returns or raises, a cancellation included,
    the call is in flight no longer. *)

val call : t -> (unit -> 'a) -> 'a
(** {!admit}, then {!run}. *)

val quiesce : t -> unit
(** Returns when no call is in flight; at once when called from inside one. *)

val loop : sw:Eio.Switch.t -> (stop:unit Eio.Promise.t -> unit) -> Subscription.t
(** A subscription served by a loop of its own: the loop runs as a daemon fiber on [sw],
    and the whole of it is one call in flight. {!Subscription.cancel} resolves [stop] and
    waits for the loop to return, so a loop that finishes what it has in hand when it is
    told to stop is a subscription that ends in good order: the batch committed, the
    connection given back. From inside the loop, a handler cancelling itself, it resolves
    [stop] and returns. A loop cut short by its switch is over as well, and nobody is left
    waiting for it. *)
