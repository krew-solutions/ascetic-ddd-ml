type t = string

let max_length = 64

let is_part text =
  (not (String.equal text ""))
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
         | _ -> false)
       text

let make text =
  if String.length text > max_length || not (is_part text) then
    Error
      (Kms_error.Malformed
         (Printf.sprintf
            "a key label is one to sixty-four of letters, digits, `_` and `-`, not `%s`"
            text))
  else Ok text

let to_string t = t
let equal = String.equal
let pp = Format.pp_print_string
