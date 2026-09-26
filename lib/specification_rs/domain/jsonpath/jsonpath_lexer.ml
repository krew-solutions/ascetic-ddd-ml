(* From the text of a template to its tokens.

   The sources try a list of regular expressions at each position. The token set is small
   and fixed, so here each token is recognised by its first character, which also lets a
   string have escapes and a number an exponent, as RFC 9535 has them. Positions count
   characters, not bytes, so that the caret of an error stands under the right one. *)

open Jsonpath_slot

type kind =
  | Dollar
  | At
  | Dot
  | Left_bracket
  | Right_bracket
  | Left_paren
  | Right_paren
  | Question
  | Star
  | And
  | Or
  | Not
  | Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le
  | Int of int64
  | Float of float
  | Text of string
  | Name of string
  | Placeholder of Param.t
[@@deriving show { with_path = false }, eq]

type token = { kind : kind; position : int }

let expected = "valid token"
let ( let* ) = Result.bind

(* The character at [at], if there is one. *)
let get chars at = if at >= 0 && at < Array.length chars then Some chars.(at) else None

(* How many characters from [from] on satisfy [test], none if [from] is past the end. *)
let run chars from test =
  let rec count at =
    match get chars at with Some c when test c -> count (at + 1) | _ -> at - from
  in
  if from > Array.length chars then 0 else count from

(* Unicode's White_Space, which the reference's lexer skips. *)
let is_whitespace c =
  (c >= 0x09 && c <= 0x0D)
  || c = 0x20 || c = 0x85 || c = 0xA0 || c = 0x1680
  || (c >= 0x2000 && c <= 0x200A)
  || c = 0x2028 || c = 0x2029 || c = 0x202F || c = 0x205F || c = 0x3000

let is_digit c = c >= Char.code '0' && c <= Char.code '9'

let is_alpha c =
  (c >= Char.code 'a' && c <= Char.code 'z') || (c >= Char.code 'A' && c <= Char.code 'Z')

let is_name_start c = is_alpha c || c = Char.code '_'
let is_name_part c = is_name_start c || is_digit c

let text_of chars from length =
  let buffer = Buffer.create length in
  for at = from to from + length - 1 do
    Buffer.add_utf_8_uchar buffer (Uchar.of_int chars.(at))
  done;
  Buffer.contents buffer

let is_positional = function
  | Placeholder { Param.key = Position _; _ } -> true
  | _ -> false

(* [%s], [%d], [%f], or the same with a name: [%(age)d]. *)
let placeholder chars at positional =
  let malformed () =
    Jsonpath_error.make "Malformed placeholder" at "%s, %d, %f or %(name)s"
  in
  let* key, letter_at =
    if get chars (at + 1) = Some (Char.code '(') then
      let length = run chars (at + 2) is_name_part in
      let close = at + 2 + length in
      if length = 0 || get chars close <> Some (Char.code ')') then Error (malformed ())
      else Ok (Param_key.Name (text_of chars (at + 2) length), close + 1)
    else Ok (Param_key.Position positional, at + 1)
  in
  let* kind =
    match Option.map Char.chr (get chars letter_at) with
    | Some 's' -> Ok Param_kind.Any
    | Some 'd' -> Ok Param_kind.Integer
    | Some 'f' -> Ok Param_kind.Number
    | _ -> Error (malformed ())
  in
  Ok (Placeholder { Param.key; kind }, letter_at + 1)

(* An integer, or - with a fraction or an exponent - a float. *)
let number chars at =
  let digits from = run chars from is_digit in
  let is c at = get chars at = Some (Char.code c) in
  let sign = if is '-' at then 1 else 0 in
  let whole_end = at + sign + digits (at + sign) in
  let fraction_end =
    match (is '.' whole_end, digits (whole_end + 1)) with
    | true, count when count > 0 -> whole_end + 1 + count
    | _ -> whole_end
  in
  let exponent_end =
    if is 'e' fraction_end || is 'E' fraction_end then
      let sign =
        if is '+' (fraction_end + 1) || is '-' (fraction_end + 1) then 1 else 0
      in
      match digits (fraction_end + 1 + sign) with
      | 0 -> fraction_end
      | count -> fraction_end + 1 + sign + count
    else fraction_end
  in
  let literal = text_of chars at (exponent_end - at) in
  let out_of_range () =
    Jsonpath_error.make "Number out of range" at "a number that fits"
  in
  let* kind =
    if exponent_end = whole_end then
      match Int64.of_string_opt literal with
      | Some value -> Ok (Int value)
      | None -> Error (out_of_range ())
    else
      (* [1e999] parses, to infinity. *)
      match float_of_string_opt literal with
      | Some value when Float.is_finite value -> Ok (Float value)
      | _ -> Error (out_of_range ())
  in
  Ok (kind, exponent_end)

let hex4 chars at =
  if at + 4 > Array.length chars then None
  else
    let digit c =
      if is_digit c then Some (c - Char.code '0')
      else if c >= Char.code 'a' && c <= Char.code 'f' then Some (c - Char.code 'a' + 10)
      else if c >= Char.code 'A' && c <= Char.code 'F' then Some (c - Char.code 'A' + 10)
      else None
    in
    let rec fold code i =
      if i = 4 then Some code
      else
        match digit chars.(at + i) with
        | Some d -> fold ((code * 16) + d) (i + 1)
        | None -> None
    in
    fold 0 0

(* The character the escape at [at] stands for, and where the text goes on. *)
let escape chars at =
  let invalid () =
    Jsonpath_error.make "Invalid escape" at
      "\\\\, \\', \\\", \\/, \\b, \\f, \\n, \\r, \\t or \\uXXXX"
  in
  let simple c = Ok (Char.code c, at + 2) in
  match Option.map Char.chr (get chars (at + 1)) with
  | Some '\\' -> simple '\\'
  | Some '\'' -> simple '\''
  | Some '"' -> simple '"'
  | Some '/' -> simple '/'
  | Some 'b' -> Ok (0x08, at + 2)
  | Some 'f' -> Ok (0x0C, at + 2)
  | Some 'n' -> simple '\n'
  | Some 'r' -> simple '\r'
  | Some 't' -> simple '\t'
  | Some 'u' ->
      let* high = Option.to_result ~none:(invalid ()) (hex4 chars (at + 2)) in
      (* A code point beyond the basic plane is a pair of escapes. *)
      if high >= 0xD800 && high < 0xDC00 then
        let low =
          match (get chars (at + 6), get chars (at + 7)) with
          | Some 0x5C, Some 0x75 -> hex4 chars (at + 8)
          | _ -> None
        in
        match low with
        | Some low when low >= 0xDC00 && low < 0xE000 ->
            Ok (0x10000 + ((high - 0xD800) lsl 10) + (low - 0xDC00), at + 12)
        | _ -> Error (invalid ())
      else if Uchar.is_valid high then Ok (high, at + 6)
      else Error (invalid ())
  | _ -> Error (invalid ())

(* A string in either quote, with the escapes of RFC 9535. Unescaped, a character of a
   string is [%x20] and up (RFC 9535, 2.3.5.1): a control character is written as its
   escape, and a NUL that arrives raw does not get as far as a query. *)
let text chars at quote =
  let unterminated () = Jsonpath_error.make "Unterminated string" at "closing quote" in
  let rec read position points =
    match get chars position with
    | None -> Error (unterminated ())
    | Some c when c = quote ->
        let buffer = Buffer.create 16 in
        List.iter
          (fun c -> Buffer.add_utf_8_uchar buffer (Uchar.of_int c))
          (List.rev points);
        Ok (Text (Buffer.contents buffer), position + 1)
    | Some 0x5C ->
        let* c, next = escape chars position in
        read next (c :: points)
    | Some c when c < 0x20 ->
        Error
          (Jsonpath_error.make "Control character in a string" position
             "its escape, \\n or \\uXXXX")
    | Some c -> read (position + 1) (c :: points)
  in
  read (at + 1) []

(* The token that starts with [first] at [at], and where the next one starts. *)
let token first chars at positional =
  let one kind = Ok (kind, at + 1) and two kind = Ok (kind, at + 2) in
  let next = Option.map Char.chr (get chars (at + 1)) in
  let c = if first < 128 then Some (Char.chr first) else None in
  match (c, next) with
  | Some '$', _ -> one Dollar
  | Some '@', _ -> one At
  | Some '.', _ -> one Dot
  | Some '[', _ -> one Left_bracket
  | Some ']', _ -> one Right_bracket
  | Some '(', _ -> one Left_paren
  | Some ')', _ -> one Right_paren
  | Some '?', _ -> one Question
  | Some '*', _ -> one Star
  | Some '&', Some '&' -> two And
  | Some '|', Some '|' -> two Or
  | Some '=', Some '=' -> two Eq
  | Some '!', Some '=' -> two Ne
  | Some '>', Some '=' -> two Ge
  | Some '<', Some '=' -> two Le
  | Some '!', _ -> one Not
  | Some '>', _ -> one Gt
  | Some '<', _ -> one Lt
  | Some '%', _ -> placeholder chars at positional
  | Some ('\'' | '"'), _ -> text chars at first
  | Some c, _ when is_digit (Char.code c) -> number chars at
  | Some '-', Some c when is_digit (Char.code c) -> number chars at
  | _ when is_name_start first ->
      let length = run chars at is_name_part in
      Ok (Name (text_of chars at length), at + length)
  | _ ->
      Error
        (Jsonpath_error.make
           (Printf.sprintf "Unexpected character '%s'" (Jsonpath_error.shown first))
           at expected)

(* The tokens of [chars], and how many positional placeholders are among them: how many
   parameters the template takes. The place of the next positional placeholder is counted
   along, not over again for each token, which made a text of a hundred kilobytes a second
   of work to refuse, and the time grew as the square of the text. *)
let tokenize chars =
  let rec loop position placeholders tokens =
    let position = position + run chars position is_whitespace in
    match get chars position with
    | None -> Ok (Array.of_list (List.rev tokens), placeholders)
    | Some first ->
        let* kind, next = token first chars position placeholders in
        let placeholders =
          if is_positional kind then placeholders + 1 else placeholders
        in
        loop next placeholders ({ kind; position } :: tokens)
  in
  loop 0 0 []

(* A token as a template spells it, for an error to show. *)
let spelling = function
  | Dollar -> "$"
  | At -> "@"
  | Dot -> "."
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Question -> "?"
  | Star -> "*"
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eq -> "=="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="
  | Int value -> Int64.to_string value
  | Float value -> Printf.sprintf "%g" value
  | Text value -> Printf.sprintf "\"%s\"" value
  | Name name -> name
  | Placeholder { key = Position _; _ } -> "%"
  | Placeholder { key = Name name; _ } -> Printf.sprintf "%%(%s)" name
