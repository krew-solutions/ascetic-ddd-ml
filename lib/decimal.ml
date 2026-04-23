(** Fixed-point decimal arithmetic.

    Internal representation: integer scaled by [scale_factor] (10000).
    This gives 4 decimal places of precision. *)

let scale_factor = 10_000

type t = int [@@deriving eq, ord]

let pp fmt t =
  let abs_val = Int.abs t in
  let sign = if t < 0 then "-" else "" in
  let integer_part = abs_val / scale_factor in
  let frac_part = abs_val mod scale_factor in
  if frac_part = 0 then Format.fprintf fmt "%s%d" sign integer_part
  else
    (* Trim trailing zeros *)
    let s = Printf.sprintf "%04d" frac_part in
    let len = ref (String.length s) in
    while !len > 0 && s.[!len - 1] = '0' do
      decr len
    done;
    Format.fprintf fmt "%s%d.%s" sign integer_part (String.sub s 0 !len)

let show t = Format.asprintf "%a" pp t

(* Construction *)

let zero = 0
let one = scale_factor

let of_int n = n * scale_factor

let of_float f = Float.to_int (Float.round (f *. Float.of_int scale_factor))

let of_string s =
  try
    match String.split_on_char '.' s with
    | [ int_part ] ->
        let n = int_of_string int_part in
        Some (n * scale_factor)
    | [ int_part; frac_part ] ->
        let negative = String.length int_part > 0 && int_part.[0] = '-' in
        let abs_int = Int.abs (int_of_string int_part) in
        (* Pad or truncate frac to 4 digits *)
        let frac_str =
          if String.length frac_part >= 4 then String.sub frac_part 0 4
          else
            frac_part
            ^ String.make (4 - String.length frac_part) '0'
        in
        let frac = int_of_string frac_str in
        let raw = (abs_int * scale_factor) + frac in
        Some (if negative then -raw else raw)
    | _ -> None
  with _ -> None

let of_string_exn s =
  match of_string s with
  | Some d -> d
  | None -> invalid_arg (Printf.sprintf "Decimal.of_string_exn: %S" s)

(* Arithmetic *)

let add a b = a + b
let sub a b = a - b

let mul a b = a * b / scale_factor

let div a b =
  if b = 0 then raise Division_by_zero;
  a * scale_factor / b

let neg t = -t
let abs t = Int.abs t
let min a b = Int.min a b
let max a b = Int.max a b

let clamp_non_negative t = if t < 0 then 0 else t

let scale t n = t * n

(* Predicates *)

let is_zero t = t = 0
let is_positive t = t > 0
let is_negative t = t < 0
let is_non_negative t = t >= 0

(* Conversion *)

let to_float t = Float.of_int t /. Float.of_int scale_factor

let to_string t = Format.asprintf "%a" pp t

let to_int_truncated t = t / scale_factor

(* Internal *)

let to_raw t = t
let of_raw t = t
