type t = string

let max_length = 40

let of_string name =
  let refuse reason =
    Error (Printf.sprintf "`%s` is not an identifier: %s" name reason)
  in
  let lower c = c >= 'a' && c <= 'z' in
  let digit c = c >= '0' && c <= '9' in
  if name = "" then refuse "it is empty"
  else if not (lower name.[0] || name.[0] = '_') then
    refuse "it must start with a lower-case letter or an underscore"
  else if not (String.for_all (fun c -> lower c || digit c || c = '_') name) then
    refuse
      "only lower-case letters, digits and underscores are allowed; a schema \
       qualification is not, the search_path decides"
  else if String.length name > max_length then refuse "it is longer than forty characters"
  else Ok name

let of_string_exn name =
  match of_string name with Ok name -> name | Error reason -> invalid_arg reason

let to_string name = name
let equal = String.equal
let pp = Format.pp_print_string
