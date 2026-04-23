%{
  open Spec_ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token <string> PLACEHOLDER
%token TRUE FALSE NULL
%token NOT IN
%token AND OR
%token EQ NEQ LT GT LTE GTE
%token PLUS MINUS STAR SLASH
%token BANG
%token DOT COMMA
%token LPAREN RPAREN
%token EOF

%start <Spec_ast.spec> spec
%start <Spec_ast.expr> expr_only

%%

spec:
  | e = or_expr; EOF { { name = None; expr = e } }
  ;

expr_only:
  | e = or_expr; EOF { e }
  ;

or_expr:
  | l = or_expr; OR; r = and_expr   { BinOp (Or, l, r) }
  | e = and_expr                    { e }
  ;

and_expr:
  | l = and_expr; AND; r = cmp_expr { BinOp (And, l, r) }
  | e = cmp_expr                    { e }
  ;

cmp_expr:
  | l = add_expr; EQ; r = add_expr  { BinOp (Eq, l, r) }
  | l = add_expr; NEQ; r = add_expr { BinOp (Neq, l, r) }
  | l = add_expr; LT; r = add_expr  { BinOp (Lt, l, r) }
  | l = add_expr; GT; r = add_expr  { BinOp (Gt, l, r) }
  | l = add_expr; LTE; r = add_expr { BinOp (Lte, l, r) }
  | l = add_expr; GTE; r = add_expr { BinOp (Gte, l, r) }
  | l = add_expr; IN; LPAREN; vs = separated_list(COMMA, add_expr); RPAREN
    { BinOp (In, l, Lit (List (List.map (fun e ->
        match e with Lit v -> v | _ -> failwith "IN requires literals") vs))) }
  | e = add_expr                    { e }
  ;

add_expr:
  | l = add_expr; PLUS; r = mul_expr  { BinOp (Add, l, r) }
  | l = add_expr; MINUS; r = mul_expr { BinOp (Sub, l, r) }
  | e = mul_expr                      { e }
  ;

mul_expr:
  | l = mul_expr; STAR; r = unary_expr  { BinOp (Mul, l, r) }
  | l = mul_expr; SLASH; r = unary_expr { BinOp (Div, l, r) }
  | e = unary_expr                      { e }
  ;

unary_expr:
  | BANG; e = unary_expr              { UnaryOp (Not, e) }
  | NOT; e = unary_expr               { UnaryOp (Not, e) }
  | MINUS; e = unary_expr             { UnaryOp (Neg, e) }
  | e = atom                          { e }
  ;

atom:
  | i = INT                           { Lit (Int i) }
  | f = FLOAT                         { Lit (Float f) }
  | s = STRING                        { Lit (Str s) }
  | TRUE                              { Lit (Bool true) }
  | FALSE                             { Lit (Bool false) }
  | NULL                              { Lit Null }
  | p = PLACEHOLDER                   { Placeholder p }
  /* Quantifier: path.exists(alias, pred) or path.forall(alias, pred)
     Since exists/forall are now lexed as IDENT, the whole thing is
     a dot-separated IDENT list followed by (alias, pred). We check
     the last segment to distinguish quantifiers from other patterns. */
  | ids = dotted; LPAREN; alias = IDENT; COMMA; pred = or_expr; RPAREN
    { let rev = List.rev ids in
      match rev with
      | last :: path_rev ->
          let p = List.rev_map (fun s -> Field s) path_rev in
          if last = "exists" then Exists (p, alias, pred)
          else if last = "forall" then ForAll (p, alias, pred)
          else failwith ("expected exists or forall, got: " ^ last)
      | [] -> failwith "empty dotted path"
    }
  /* Function call: ident(args) */
  | id = IDENT; LPAREN; args = separated_list(COMMA, or_expr); RPAREN
    { Call (id, args) }
  /* Plain dotted path (2+ segments) */
  | ids = dotted
    { Path (List.map (fun s -> Field s) ids) }
  /* Single identifier as path */
  | id = IDENT
    { Path [Field id] }
  | LPAREN; e = or_expr; RPAREN      { e }
  ;

/* Dot-separated identifier list — at least 2 segments (single IDENT is
   handled by the function call and plain path alternatives above). */
dotted:
  | id = IDENT; DOT; rest = dotted_tail { id :: rest }
  ;

dotted_tail:
  | id = IDENT; DOT; rest = dotted_tail { id :: rest }
  | id = IDENT                          { [id] }
  ;
