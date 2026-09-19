type json =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `String of string
  | `List of json list
  | `Assoc of (string * json) list ]

let write_string out text =
  Buffer.add_char out '"';
  String.iter
    (function
      | '"' -> Buffer.add_string out "\\\""
      | '\\' -> Buffer.add_string out "\\\\"
      | '\b' -> Buffer.add_string out "\\b"
      | '\012' -> Buffer.add_string out "\\f"
      | '\n' -> Buffer.add_string out "\\n"
      | '\r' -> Buffer.add_string out "\\r"
      | '\t' -> Buffer.add_string out "\\t"
      | '\000' .. '\031' as c ->
          Buffer.add_string out (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char out c)
    text;
  Buffer.add_char out '"'

(* The fields by name, each name once: the last one given, as a map keeps it. *)
let fields_in_order fields =
  let last_wins =
    List.fold_left
      (fun kept (name, value) -> (name, value) :: List.remove_assoc name kept)
      [] fields
  in
  List.sort (fun (a, _) (b, _) -> String.compare a b) last_wins

let rec write out : json -> unit = function
  | `Null -> Buffer.add_string out "null"
  | `Bool b -> Buffer.add_string out (if b then "true" else "false")
  | `Int n -> Buffer.add_string out (string_of_int n)
  | `String text -> write_string out text
  | `List items ->
      Buffer.add_char out '[';
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_char out ',';
          write out item)
        items;
      Buffer.add_char out ']'
  | `Assoc fields ->
      Buffer.add_char out '{';
      List.iteri
        (fun i (name, value) ->
          if i > 0 then Buffer.add_char out ',';
          write_string out name;
          Buffer.add_char out ':';
          write out value)
        (fields_in_order fields);
      Buffer.add_char out '}'

let to_string json =
  let out = Buffer.create 64 in
  write out json;
  Buffer.contents out
