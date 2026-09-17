(** What happens to a message whose subscriber fails (ADR-0004). Durations are seconds. *)

type t = {
  max_attempts : int;
      (** Failed attempts after which the message is parked; [0]: never, it is retried for
          ever. *)
  backoff : int -> float;
      (** How long the message waits after its [n]th failed attempt, [n] from 1. While it
          waits it holds its slot. *)
}

(** Unlimited attempts, no backoff: the message is taken again at the next call. The
    default. *)
let unlimited = { max_attempts = 0; backoff = (fun _ -> 0.0) }

(** Parked after [max_attempts] failures, no backoff. *)
let up_to max_attempts = { unlimited with max_attempts }

(** The same, waiting [backoff n] after the [n]th failure. *)
let with_backoff t backoff = { t with backoff }

(** [base], doubled with every failure, at most [cap]. *)
let exponential ~base ~cap attempt =
  let doublings = min (max attempt 1 - 1) 31 in
  Float.min cap (base *. Float.pow 2.0 (float_of_int doublings))
