(** Public API for parsing specification expressions.

    Wraps the generated lexer/parser with error handling. *)

type parse_error = {
  message : string;
  position : int option;
}
[@@deriving show, eq]

let parse (input : string) : (Spec_ast.spec, parse_error) result =
  try
    let lexbuf = Lexing.from_string input in
    let spec = Spec_parser.spec Spec_lexer.token lexbuf in
    Ok spec
  with
  | Spec_lexer.Lexer_error msg ->
      Error { message = msg; position = None }
  | Spec_parser.Error ->
      let pos = (Lexing.from_string input).lex_curr_pos in
      Error
        {
          message = Printf.sprintf "parse error near position %d" pos;
          position = Some pos;
        }

let parse_expr (input : string) : (Spec_ast.expr, parse_error) result =
  try
    let lexbuf = Lexing.from_string input in
    let expr = Spec_parser.expr_only Spec_lexer.token lexbuf in
    Ok expr
  with
  | Spec_lexer.Lexer_error msg ->
      Error { message = msg; position = None }
  | Spec_parser.Error ->
      let pos = (Lexing.from_string input).lex_curr_pos in
      Error
        {
          message = Printf.sprintf "parse error near position %d" pos;
          position = Some pos;
        }
