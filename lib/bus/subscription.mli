(** A handle on a subscription. {!cancel} detaches the handler; a second [cancel] does
    nothing. Discarding the handle does not cancel: a subscription made at the composition
    root lives with the process, and its handle is usually discarded. *)

type t

val make : (unit -> unit) -> t
(** A subscription that {!cancel} detaches by running the function once. *)

val cancel : t -> unit
(** Detaches the handler. Idempotent. *)
