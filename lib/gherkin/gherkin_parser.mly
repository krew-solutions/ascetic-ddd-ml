%{
  open Gherkin_ast
%}

%token FEATURE SCENARIO
%token GIVEN WHEN THEN AND BUT
%token <string> TEXT
%token NEWLINE EOF

%start <Gherkin_ast.feature> feature

%%

feature:
  | newlines; FEATURE; name = TEXT; newlines;
    desc = option(description);
    scenarios = list(scenario);
    EOF
    { { name = String.trim name; description = desc; scenarios } }
  ;

description:
  | t = TEXT; newlines { String.trim t }
  ;

scenario:
  | SCENARIO; name = TEXT; newlines;
    steps = nonempty_list(step)
    { { name = String.trim name; steps } }
  ;

step:
  | kw = step_keyword; text = TEXT; newlines
    { { keyword = kw; text = String.trim text } }
  ;

step_keyword:
  | GIVEN { Given }
  | WHEN  { When }
  | THEN  { Then }
  | AND   { And }
  | BUT   { But }
  ;

newlines:
  | list(NEWLINE) { () }
  ;
