{
  open Gherkin_parser

  let text_queue : token Queue.t = Queue.create ()

  let enqueue_text_and_newline text =
    let trimmed = String.trim text in
    if String.length trimmed > 0 then
      Queue.push (TEXT trimmed) text_queue;
    Queue.push NEWLINE text_queue
}

let whitespace = [' ' '\t']
let newline = '\n' | '\r' '\n'

rule token = parse
  | whitespace+             { token lexbuf }
  | newline                 { Lexing.new_line lexbuf; NEWLINE }
  | '#' [^ '\n']*          { token lexbuf }
  | "Feature:"             { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; FEATURE }
  | "Scenario:"            { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; SCENARIO }
  | "Given "               { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; GIVEN }
  | "When "                { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; WHEN }
  | "Then "                { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; THEN }
  | "And "                 { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; AND }
  | "But "                 { let s = rest_of_line (Buffer.create 64) lexbuf in
                              enqueue_text_and_newline s; BUT }
  | eof                    { EOF }
  | _ as c                 { failwith (Printf.sprintf
                               "Gherkin lexer: unexpected char %C at line %d"
                               c lexbuf.Lexing.lex_curr_p.pos_lnum) }

and rest_of_line buf = parse
  | newline                { Lexing.new_line lexbuf; Buffer.contents buf }
  | eof                    { Buffer.contents buf }
  | _ as c                 { Buffer.add_char buf c; rest_of_line buf lexbuf }

{
  let next_token lexbuf =
    if Queue.is_empty text_queue then
      token lexbuf
    else
      Queue.pop text_queue
}
