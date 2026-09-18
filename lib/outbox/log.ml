(** Where the outbox says what must not be silent when no observer is attached: a loop
    that waits after a failure, a dispatcher on the bus that starts again. A [Logs] source
    of its own, [ascetic_ddd.outbox], so that an application routes or silences it by
    name; the observer remains the place for everything a deployment measures. *)

let src = Logs.Src.create "ascetic_ddd.outbox" ~doc:"The transactional outbox"

include (val Logs.src_log src : Logs.LOG)
