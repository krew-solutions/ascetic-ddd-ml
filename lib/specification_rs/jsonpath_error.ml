(* A template is not in the grammar.

   Prints as the sources' [JSONPathSyntaxError] does - the message, where, what was
   expected, and the template with a caret under the place:

   {v
   Unexpected character '#' at position 7 (expected valid token)
     $[?@.a # 1]
            ^
   v} *)

type t = {
  message : string;  (** What was found. *)
  position : int;  (** Where: the index of the character, from zero. *)
  expected : string;  (** What would have been accepted there. *)
  expression : string;  (** The template. *)
}
[@@deriving show { with_path = false }, eq]

let make message position expected = { message; position; expected; expression = "" }
let within error expression = { error with expression }

(* A character as an error shows it: a control character by its escape - [\n], [\t],
   [\r], or [\x00] and the like - not as it is. *)
let shown code =
  match code with
  | 0x0A -> "\\n"
  | 0x09 -> "\\t"
  | 0x0D -> "\\r"
  | code when code < 0x20 || (code >= 0x7F && code <= 0x9F) ->
      Printf.sprintf "\\x%02x" code
  | code ->
      let buffer = Buffer.create 4 in
      Buffer.add_utf_8_uchar buffer (Uchar.of_int code);
      Buffer.contents buffer

(* The code points of a text, in order. A byte sequence that is not UTF-8 is read as the
   replacement character, as a decoder does. *)
let code_points text =
  let rec decode at points =
    if at >= String.length text then List.rev points
    else
      let decoded = String.get_utf_8_uchar text at in
      decode
        (at + Uchar.utf_decode_length decoded)
        (Uchar.to_int (Uchar.utf_decode_uchar decoded) :: points)
  in
  Array.of_list (decode 0 [])

let to_string error =
  (* A template too long to read is not echoed. *)
  if error.expression = "" then
    Printf.sprintf "%s at position %d (expected %s)" error.message error.position
      error.expected
  else
    (* The template is echoed with a control character shown by its escape, so that the
       message has none in it; the caret moves by what the escapes add before the
       position. *)
    let points = code_points error.expression in
    let echoed = String.concat "" (Array.to_list (Array.map shown points)) in
    let before = Array.sub points 0 (min error.position (Array.length points)) in
    let pointer =
      Array.fold_left
        (fun width code -> width + Array.length (code_points (shown code)))
        0 before
    in
    Printf.sprintf "%s at position %d (expected %s)\n  %s\n  %s^" error.message
      error.position error.expected echoed (String.make pointer ' ')
