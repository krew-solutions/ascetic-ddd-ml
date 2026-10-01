(** A string constant read as the kind of the value beside it.

    A template has the literals of RFC 9535 - a string, a number, [true], [false], [null]
    - and no others: a point in time, a date or a UUID in a template is a string,
      [@.created_at > '2026-09-01']. PostgreSQL reads an untyped parameter by the type of
      the column beside it, and so does the evaluator: a string compared with a value of a
      kind that has no literal of its own is read as that kind first,
      {!Operand.S.read_beside}. What is read is a subset of what the server reads, so that
      nothing the evaluator accepts fails on the server. A string beside a number or a
      boolean stays a string: those have literals, and a string there is the author's
      choice. ADR-0015 of the reference.

    The readers here know nothing of {!Value}: they give the number a value is made of,
    and {!Value} wraps it. *)

val point_in_time_form : string
(** The form a point in time is read in: ISO 8601, a date, or a date followed by [T] or a
    space and a time of hours and minutes, seconds, a fraction of up to six digits, then
    nothing, [Z], or an offset. The server reads more - [20260901], [Sep 1 2026],
    [yesterday] - and those are an error here. *)

val uuid_form : string
(** The form a UUID is read in: the canonical one, in either case. The server takes it
    without hyphens and in braces as well; here those are an error. *)

val point_in_time : string -> int64 option
(** [text] as microseconds since the Unix epoch, with its offset applied, in UTC without
    one; [None] if it is not of the form. *)

val calendar_date : string -> int option
(** The date [text] starts with, as days since the Unix epoch, whatever time and offset
    follow: what the server makes of it for a [date]. *)

val uuid : string -> Uuidm.t option
(** [text] as a UUID; [None] if it is not the canonical form. *)
