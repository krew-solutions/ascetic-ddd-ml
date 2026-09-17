(** How {!Pg_inbox.run} runs: [concurrency] loops in this process, each taking whatever
    slot has work. Processes need no identity and no count. Durations are seconds. *)

type t = {
  concurrency : int;  (** How many loops this process runs. *)
  poll_interval : float;  (** How long a loop waits when there was nothing to take. *)
  max_pause : float;
      (** The longest a loop waits after an error of the moment, a lock cycle the server
          broke, a connection lost, before it goes on: the wait starts at [poll_interval]
          and doubles with each such error in a row. *)
}

let default = { concurrency = 1; poll_interval = 1.0; max_pause = 60.0 }

(** The wait after the [n]th error of the moment in a row, [n] from 1. *)
let pause_after t n =
  let doublings = min (max n 1 - 1) 31 in
  Float.min t.max_pause (t.poll_interval *. Float.pow 2.0 (float_of_int doublings))
