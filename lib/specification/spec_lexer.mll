{
  open Spec_parser

  exception Lexer_error of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let whitespace = [' ' '\t' '\n' '\r']

rule token = parse
  | whitespace+      { token lexbuf }
  | "true"           { TRUE }
  | "false"          { FALSE }
  | "null"           { NULL }
  | "not"            { NOT }
  | "in"             { IN }
  | "&&"             { AND }
  | "||"             { OR }
  | "=="             { EQ }
  | "!="             { NEQ }
  | "<="             { LTE }
  | ">="             { GTE }
  | '<'              { LT }
  | '>'              { GT }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { STAR }
  | '/'              { SLASH }
  | '!'              { BANG }
  | '.'              { DOT }
  | ','              { COMMA }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  (* Reserved for future: [, ], ?, @ *)
  | '$' (alpha alnum* as name)  { PLACEHOLDER name }
  | '"' ([^ '"' '\\']* as s) '"' { STRING s }
  | '\'' ([^ '\'' '\\']* as s) '\'' { STRING s }
  | digit+ '.' digit+ as f     { FLOAT (float_of_string f) }
  | digit+ as i                { INT (int_of_string i) }
  | alpha alnum* as id          { IDENT id }
  | eof              { EOF }
  | _ as c           { raise (Lexer_error (Printf.sprintf "unexpected char: %c" c)) }
