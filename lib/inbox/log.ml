(** Where the inbox says what must not be silent when no observer is attached: a failed
    attempt, a message parked, which needs a person, a loop that waits after an error of
    the moment. A [Logs] source of its own, [ascetic_ddd.inbox], so that an application
    routes or silences it by name; the observer remains the place for everything a
    deployment measures. *)

let src = Logs.Src.create "ascetic_ddd.inbox" ~doc:"The transactional inbox"

include (val Logs.src_log src : Logs.LOG)
