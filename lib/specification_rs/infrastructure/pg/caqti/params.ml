module Value = Ascetic_specification.Value

type t = T : 'a Caqti_type.t * 'a -> t
type error = Nul_in_text [@@deriving show { with_path = false }, eq]

let ( let* ) = Result.bind
let micros_per_day = 86_400_000_000L
let civil_of_days days = Value.Date.to_civil (Value.Date.of_days days)

(* PostgreSQL has no year 0: the year before 1 is 1 BC. *)
let era year = if year <= 0 then (1 - year, " BC") else (year, "")

let date days =
  let year, month, day = civil_of_days days in
  let year, era = era year in
  Printf.sprintf "%04d-%02d-%02d%s" year month day era

let timestamp micros =
  let days = Int64.div micros micros_per_day and rest = Int64.rem micros micros_per_day in
  let days, rest =
    if Int64.compare rest 0L < 0 then (Int64.pred days, Int64.add rest micros_per_day)
    else (days, rest)
  in
  let year, month, day = civil_of_days (Int64.to_int days) in
  let seconds = Int64.to_int (Int64.div rest 1_000_000L) in
  let fraction = Int64.to_int (Int64.rem rest 1_000_000L) in
  let year, era = era year in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ%s" year month day (seconds / 3600)
    (seconds / 60 mod 60)
    (seconds mod 60) fraction era

let float value =
  if Float.is_nan value then "NaN"
  else if value = Float.infinity then "Infinity"
  else if value = Float.neg_infinity then "-Infinity"
  else Printf.sprintf "%.17g" value

let text = function
  | Value.Null -> Ok None
  | Bool value -> Ok (Some (string_of_bool value))
  | Int value -> Ok (Some (Int64.to_string value))
  | Float value -> Ok (Some (float value))
  | Text value ->
      if String.contains value '\000' then Error Nul_in_text else Ok (Some value)
  | Timestamp value -> Ok (Some (timestamp (Value.Timestamp.to_micros value)))
  | Date value -> Ok (Some (date (Value.Date.to_days value)))
  | Interval value ->
      Ok (Some (Printf.sprintf "%Ld microseconds" (Value.Interval.to_micros value)))
  | Uuid value -> Ok (Some (Uuidm.to_string value))

let rec of_values = function
  | [] -> Ok (T (Caqti_type.unit, ()))
  | value :: rest ->
      let* text = text value in
      let* (T (rest_type, rest_values)) = of_values rest in
      Ok (T (Caqti_type.(t2 (option string) rest_type), (text, rest_values)))

type ('b, 'm) request = R : ('a, 'b, 'm) Caqti_request.t * 'a -> ('b, 'm) request

let request ?(oneshot = true) row mult sql params =
  let* (T (param_type, values)) = of_values params in
  let query = Caqti_query.of_string_exn sql in
  Ok (R (Caqti_request.create ~oneshot param_type row mult (fun _ -> query), values))
