(** How {!Pg_outbox.run} runs: [concurrency] loops in this process, each taking whatever
    slot has work. Processes need no identity and no count; start as many as the slots
    make useful. Durations are seconds. *)

type t = {
  concurrency : int;  (** How many loops this process runs. *)
  poll_interval : float;  (** How long a loop waits when there was nothing to dispatch. *)
  max_pause : float;
      (** The longest a loop waits after a failure, the subscriber's, or an error of the
          moment in the database, a connection lost, before it tries again: the wait
          starts at [poll_interval] and doubles with each failure in a row. *)
}

let default = { concurrency = 1; poll_interval = 1.0; max_pause = 60.0 }

(** The wait after the [n]th failure in a row, [n] from 1. *)
let pause_after t n =
  let doublings = min (max n 1 - 1) 31 in
  Float.min t.max_pause (t.poll_interval *. Float.pow 2.0 (float_of_int doublings))
