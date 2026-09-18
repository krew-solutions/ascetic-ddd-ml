(** Where the bus reports what it cannot return: a message a consumer could not decode, a
    handler that failed on a transport with nobody to tell. A [Logs] source of its own, so
    that an application routes or silences it by name. *)

let src = Logs.Src.create "ascetic_ddd.bus" ~doc:"The scheme-dispatched message bus"

include (val Logs.src_log src : Logs.LOG)
