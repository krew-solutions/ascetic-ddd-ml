(** A recorder whose lines go to [$ASCETIC_DDD_TRACE_DIR/<stem>.jsonl] when it is closed;
    with the variable unset, nothing is written. A test attaches the recorder, runs, and
    closes the file in a [finally], so that a run that raises leaves what it had. *)

type t = { recorder : Json_trace.t; path : string option }

(** A recorder for the run named [stem]; the file name's prefix, before the first dash,
    tells [trace2tla.py] which model to check against: [outbox-], [inbox-] or [bridge-].
*)
let from_env stem =
  {
    recorder = Json_trace.create ();
    path =
      Option.map
        (fun dir -> Filename.concat dir (stem ^ ".jsonl"))
        (Sys.getenv_opt "ASCETIC_DDD_TRACE_DIR");
  }

(** A recorder that writes nothing: for a run with several dispatchers at once, whose
    events are logged in an order that is not the order of their commits, and which the
    trace check therefore does not cover. *)
let off () = { recorder = Json_trace.create (); path = None }

(** The recorder, to attach as an observer. *)
let recorder t = t.recorder

(** Writes the lines, if a directory was set. *)
let close t = Option.iter (Json_trace.write_to t.recorder) t.path
