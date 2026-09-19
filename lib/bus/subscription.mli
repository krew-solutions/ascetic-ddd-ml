(** A handle on a subscription. {!cancel} detaches the handler; a second [cancel] does
    nothing. Discarding the handle does not cancel: a subscription made at the composition
    root lives with the process, and its handle is usually discarded.

    A subscription cancelled is a handler that is not running and will not be run: the
    adapters of this library end their [cancel] by waiting for the call in flight, and a
    channel over a database for its loop to finish the batch it has in hand, committed and
    its connection given back (ADR-0016). That is what stopping in good order needs: once
    [cancel] has returned, what the handler uses may be taken down. *)

type t

val make : ?quiesce:(unit -> unit) -> (unit -> unit) -> t
(** A subscription that {!cancel} detaches by running the function, once, to its end, and
    then, every time it is called, waits on with [quiesce], which returns when no call of
    the handler is in flight; see {!Handling.quiesce}. Without one, [cancel] does not
    wait. A function that raises, or is cut short by a cancellation, has not detached: the
    next [cancel] runs it again. *)

val cancel : t -> unit
(** Detaches the handler, and returns when no call of it is in flight: it waits for a
    handler that is running, for as long as that takes. Called from inside the handler, a
    handler cancelling itself, it detaches and returns: the message in hand is the last.
    Idempotent: a second [cancel] detaches nothing, and waits like the first, for the
    detaching to be over if another fiber is still at it, and then for the calls in
    flight. The protocol is model-checked, [verify/tla/Cancel.tla]. *)
