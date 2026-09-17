(** What a subscriber says when it fails: whether trying again can help.

    [Transient] is the ordinary failure: the message is tried again after its backoff and
    parked after its attempts (ADR-0004). [Permanent] is the subscriber's verdict that no
    retry will ever succeed, a payload it cannot read, an invariant the message breaks,
    and the message is parked at once, whatever attempts it had left (ADR-0009). The text
    is what the row records as [last_error]. *)

type t = Transient of string | Permanent of string

let transient message = Transient message
let permanent message = Permanent message
let message = function Transient message | Permanent message -> message
let is_permanent = function Permanent _ -> true | Transient _ -> false
let equal (a : t) (b : t) = a = b
let pp ppf failure = Format.pp_print_string ppf (message failure)
