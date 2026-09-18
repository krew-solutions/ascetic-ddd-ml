(** Why a message was not handled, and whether trying again can help.

    [Transient] is the ordinary failure, of the moment: a transport that can, delivers the
    message again. [Permanent] is the one verdict the bus carries: a failure no retry will
    mend, a message that cannot be opened, a key that is gone. The bus knows no verdicts
    of its own; it carries this one from whoever can tell, a stage or a handler, to a
    transport that can act on it: the inbox parks such a message at once instead of
    retrying it (ADR-0009). *)

type t = Transient of string | Permanent of string

let transient message = Transient message
let permanent message = Permanent message
let message = function Transient message | Permanent message -> message
let is_permanent = function Permanent _ -> true | Transient _ -> false
let equal (a : t) (b : t) = a = b

let pp ppf = function
  | Transient message -> Format.pp_print_string ppf message
  | Permanent message -> Format.fprintf ppf "permanent: %s" message

let to_string failure = Format.asprintf "%a" pp failure
