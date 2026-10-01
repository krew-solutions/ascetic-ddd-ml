let point_in_time_form =
  "YYYY-MM-DD, or that with THH:MM[:SS[.ffffff]] and Z or an offset"

let uuid_form = "8-4-4-4-12 hexadecimal digits"
let ( let* ) = Option.bind
let micros_per_day = 86_400_000_000L

(* The number the [len] ASCII digits of [text] at [pos] spell; [None] if they are not
   there, or are not digits. *)
let digits text pos len =
  if pos < 0 || pos + len > String.length text then None
  else
    let rec go i acc =
      if i = len then Some acc
      else
        match text.[pos + i] with
        | '0' .. '9' as c -> go (i + 1) ((acc * 10) + Char.code c - Char.code '0')
        | _ -> None
    in
    go 0 0

(* The hours, minutes, seconds and microseconds [text] spells: [HH:MM], then nothing or
   [:SS], then nothing or a fraction of one to six digits. *)
let clock text =
  let len = String.length text in
  let* hour = digits text 0 2 in
  let* minute = digits text 3 2 in
  if len < 5 || text.[2] <> ':' then None
  else if len = 5 then Some (hour, minute, 0, 0)
  else if text.[5] <> ':' then None
  else
    let* second = digits text 6 2 in
    if len = 8 then Some (hour, minute, second, 0)
    else if len < 10 || len > 15 || text.[8] <> '.' then None
    else
      let fraction = String.sub text 9 (len - 9) in
      let* micros =
        digits (fraction ^ String.make (6 - String.length fraction) '0') 0 6
      in
      Some (hour, minute, second, micros)

(* The offset in seconds: zero for nothing and for [Z]; [+HH:MM] or [-HH:MM]. *)
let offset text =
  match text with
  | "" | "Z" -> Some 0
  | _ -> (
      if String.length text <> 6 || text.[3] <> ':' then None
      else
        let* hours = digits text 1 2 in
        let* minutes = digits text 4 2 in
        let seconds = ((hours * 60) + minutes) * 60 in
        if seconds >= 86_400 then None
        else
          match text.[0] with '+' -> Some seconds | '-' -> Some (-seconds) | _ -> None)

(* Where the offset begins in a time of day: at the last of [Z], [+] and [-]. *)
let offset_at text =
  let rec go i =
    if i < 0 then None
    else match text.[i] with 'Z' | '+' | '-' -> Some i | _ -> go (i - 1)
  in
  go (String.length text - 1)

(* The date and time of day [text] spells as a POSIX time, and the microseconds of its
   fraction, which a [Ptime.t] has no room for: midnight in UTC where there is no time,
   the offset applied where there is one. [Ptime] refuses a date or a time that is not;
   the sixtieth second it would carry into the next minute, and the server refuses. *)
let parse text =
  let len = String.length text in
  let* year = digits text 0 4 in
  let* month = digits text 5 2 in
  let* day = digits text 8 2 in
  if len < 10 || text.[4] <> '-' || text.[7] <> '-' then None
  else
    let* (hour, minute, second, micros), offset =
      if len = 10 then Some ((0, 0, 0, 0), 0)
      else if text.[10] <> 'T' && text.[10] <> ' ' then None
      else
        let rest = String.sub text 11 (len - 11) in
        let clock_text, offset_text =
          match offset_at rest with
          | Some at -> (String.sub rest 0 at, String.sub rest at (String.length rest - at))
          | None -> (rest, "")
        in
        let* clock = clock clock_text in
        let* offset = offset offset_text in
        Some (clock, offset)
    in
    if second > 59 then None
    else
      let* moment =
        Ptime.of_date_time ((year, month, day), ((hour, minute, second), offset))
      in
      Some (moment, (year, month, day), micros)

(* Microseconds since the Unix epoch of a POSIX time and the fraction beside it. *)
let micros_of moment fraction =
  let days, picos = Ptime.Span.to_d_ps (Ptime.to_span moment) in
  Int64.add
    (Int64.add
       (Int64.mul (Int64.of_int days) micros_per_day)
       (Int64.div picos 1_000_000L))
    (Int64.of_int fraction)

let point_in_time text =
  let* moment, _, micros = parse text in
  Some (micros_of moment micros)

let calendar_date text =
  let* _, date, _ = parse text in
  let* midnight = Ptime.of_date date in
  Some (fst (Ptime.Span.to_d_ps (Ptime.to_span midnight)))

let is_hex = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let canonical text =
  let rec go i =
    i = 36
    || (match i with 8 | 13 | 18 | 23 -> text.[i] = '-' | _ -> is_hex text.[i])
       && go (i + 1)
  in
  String.length text = 36 && go 0

let uuid text = if canonical text then Uuidm.of_string text else None
