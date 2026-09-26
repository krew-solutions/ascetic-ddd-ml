(* Templates: what the Python [test_jsonpath_parser] and the Go [parser_test] check, and
   what this parser refuses that theirs let through. *)

open Ascetic_specification
open Ast
open Jsonpath

let template source = Template.parse_exn source

let error source =
  match Template.parse source with
  | Ok _ -> Alcotest.failf "parsed: %s" source
  | Error e -> e

let literal value = Ast.value (Slot.Literal value)
let int i = literal (Value.of_int i)

let positional position kind =
  Ast.value (Slot.Param { key = Param_key.Position position; kind })

let vint i = Value.of_int i
let vtext s = Value.Text s
let slot_tree = Alcotest.testable (Ast.pp Slot.pp) (Ast.equal Slot.equal)
let value_tree = Alcotest.testable (Ast.pp Value.pp) (Ast.equal Value.equal)
let match_error = Alcotest.testable Match_error.pp Match_error.equal
let bind_error = Alcotest.testable Bind_error.pp Bind_error.equal
let matched = Alcotest.(result bool match_error)
let bound = Alcotest.(result value_tree bind_error)

let user age name active =
  Record.(
    to_context
      (object_
         [
           ("age", value (vint age));
           ("name", value (vtext name));
           ("active", value (Value.Bool active));
         ]))

let store () =
  let item name price stock =
    Record.(
      object_
        [
          ("name", value (vtext name));
          ("price", value (Value.Float price));
          ("stock", value (vint stock));
        ])
  in
  Record.(
    to_context
      (object_
         [
           ("limit", value (Value.Float 100.0));
           ("items", collection [ item "Laptop" 999.0 5; item "Mouse" 29.0 100 ]);
           ( "categories",
             collection
               [
                 object_
                   [
                     ("name", value (vtext "Electronics"));
                     ("items", collection [ item "Laptop" 999.0 5 ]);
                   ];
                 object_
                   [
                     ("name", value (vtext "Stationery"));
                     ("items", collection [ item "Pen" 2.0 500 ]);
                   ];
               ] );
           ("warehouse", object_ [ ("items", collection [ item "Widget" 10.0 5 ]) ]);
         ]))

let comparisons_with_positional_placeholders () =
  let alice = user 30 "Alice" true in
  List.iter
    (fun (source, param, expected) ->
      Alcotest.check matched
        (Printf.sprintf "%s with %d" source param)
        (Ok expected)
        (Template.matches (template source) alice (Params.positional [ vint param ])))
    [
      ("$[?@.age > %d]", 25, true);
      ("$[?@.age > %d]", 30, false);
      ("$[?@.age < %d]", 35, true);
      ("$[?@.age == %d]", 30, true);
      ("$[?@.age != %d]", 30, false);
      ("$[?@.age >= %d]", 30, true);
      ("$[?@.age <= %d]", 29, false);
    ]

let placeholders_by_name_and_of_every_kind () =
  let alice = user 30 "Alice" true in
  let by_name = template "$[?@.name == %(name)s && @.age > %(age)d]" in
  Alcotest.check matched "alice" (Ok true)
    (Template.matches by_name alice
       (Params.named [ ("name", vtext "Alice"); ("age", vint 25) ]));
  Alcotest.check matched "bob" (Ok false)
    (Template.matches by_name alice
       (Params.named [ ("name", vtext "Bob"); ("age", vint 25) ]));
  (* [%s] takes a value of any kind, as Python's does. *)
  Alcotest.check matched "%s" (Ok true)
    (Template.matches
       (template "$[?@.active == %s]")
       alice
       (Params.positional [ Value.Bool true ]));
  (* [%f] takes any number. *)
  let cheap = template "$.items[*][?@.price < %f]" in
  Alcotest.check matched "%f float" (Ok true)
    (Template.matches cheap (store ()) (Params.positional [ Value.Float 30.5 ]));
  Alcotest.check matched "%f int" (Ok true)
    (Template.matches cheap (store ()) (Params.positional [ vint 30 ]));
  (* A name may be used twice, and a name not used is ignored. *)
  let twice = template "$[?@.age >= %(n)d && @.age <= %(n)d]" in
  Alcotest.check matched "twice" (Ok true)
    (Template.matches twice alice (Params.named [ ("n", vint 30); ("unused", vint 1) ]))

let a_template_is_parsed_once_and_bound_many_times () =
  let older_than = template "$[?@.age > %d]" in
  let tree = Template.expr older_than in
  let alice = user 30 "Alice" true in
  Alcotest.check matched "25" (Ok true)
    (Template.matches older_than alice (Params.positional [ vint 25 ]));
  Alcotest.check matched "35" (Ok false)
    (Template.matches older_than alice (Params.positional [ vint 35 ]));
  Alcotest.check slot_tree "unchanged" tree (Template.expr older_than);
  Alcotest.(check string) "source" "$[?@.age > %d]" (Template.source older_than);
  Alcotest.check bound "bound"
    (Ok (gt (field "age") (Ast.value (vint 25))))
    (Template.bind older_than (Params.positional [ vint 25 ]))

let parameters_that_do_not_fit_are_refused () =
  let two = template "$[?@.age > %d && @.name == %s]" in
  Alcotest.check bound "missing" (Error (Bind_error.Missing (Param_key.Position 1)))
    (Template.bind two (Params.positional [ vint 25 ]));
  Alcotest.check bound "unused"
    (Error (Bind_error.Unused { placeholders = 2; parameters = 3 }))
    (Template.bind two (Params.positional [ vint 25; vtext "a"; vint 1 ]));
  Alcotest.check bound "wrong style" (Error Bind_error.Wrong_style)
    (Template.bind two (Params.named [ ("age", vint 25) ]));
  Alcotest.check bound "mismatch"
    (Error
       (Bind_error.Mismatch
          { key = Param_key.Position 0; expected = Param_kind.Integer; found = "text" }))
    (Template.bind two (Params.positional [ vtext "25"; vtext "a" ]));
  let named = template "$[?@.age > %(age)d]" in
  Alcotest.check bound "missing name" (Error (Bind_error.Missing (Param_key.Name "age")))
    (Template.bind named (Params.named [ ("years", vint 25) ]));
  Alcotest.check bound "positional for named" (Error Bind_error.Wrong_style)
    (Template.bind named (Params.positional [ vint 25 ]));
  Alcotest.(check bool)
    "%f of a text" true
    (Result.is_error
       (Template.bind (template "$[?@.price < %f]") (Params.positional [ vtext "1" ])));
  (* A null fits a placeholder of any kind. *)
  Alcotest.(check bool)
    "null fits" true
    (Result.is_ok (Template.bind named (Params.named [ ("age", Value.Null) ])));
  Alcotest.check matched "none for named"
    (Error (Match_error.Bind Bind_error.Wrong_style))
    (Template.matches named (user 30 "Alice" true) Params.none)

let literals () =
  let alice = user 30 "Alice" true in
  List.iter
    (fun (source, expected) ->
      Alcotest.check matched source (Ok expected)
        (Template.matches (template source) alice Params.none))
    [
      ("$[?@.active == true]", true);
      ("$[?@.active == false]", false);
      ("$[?@.active == TRUE]", true);
      ("$[?@.name == 'Alice']", true);
      ("$[?@.name == \"Alice\"]", true);
      ("$[?@.age == 30]", true);
      ("$[?@.age > -1]", true);
      ("$[?@.age < 30.5]", true);
      ("$[?@.age == 3e1]", true);
      ("$[?@.age == 300E-1]", true);
      ("$[?@.age == null]", false);
      ("$[?@.active]", true);
      ("$[?!@.active]", false);
    ];
  Alcotest.check slot_tree "escapes"
    (and_
       (and_
          (eq (field "a") (literal (vtext "it's")))
          (eq (field "b") (literal (vtext "A\n\\"))))
       (eq (field "c") (literal (vtext "\xF0\x9F\x98\x80"))))
    (Template.expr
       (template {|$[?@.a == 'it\'s' && @.b == "\u0041\n\\" && @.c == '\uD83D\uDE00']|}));
  Alcotest.check slot_tree "float"
    (eq (field "a") (literal (Value.Float 1.5)))
    (Template.expr (template "$[?@.a == 1.5]"));
  Alcotest.check slot_tree "int"
    (eq (field "a") (int 15))
    (Template.expr (template "$[?@.a == 15]"))

let a_null_is_tested_not_compared () =
  (* In JSONPath null is a value, and [@.a == null] is how a null is found; in the tree
     [a = NULL] is null, as in SQL, and true of nothing. What the template means is
     IS NULL, spelled out or bound to a placeholder. *)
  let record deleted_at =
    Record.(
      to_context
        (object_ [ ("deleted_at", value deleted_at); ("name", value (vtext "x")) ]))
  in
  let deleted = record Value.Null and alive = record (vint 5) in
  List.iter
    (fun (source, params, of_deleted, of_alive) ->
      let template = template source in
      Alcotest.check matched (source ^ " of a null") (Ok of_deleted)
        (Template.matches template deleted params);
      Alcotest.check matched (source ^ " of a value") (Ok of_alive)
        (Template.matches template alive params))
    [
      ("$[?@.deleted_at == null]", Params.none, true, false);
      ("$[?null == @.deleted_at]", Params.none, true, false);
      ("$[?@.deleted_at != null]", Params.none, false, true);
      ("$[?!(@.deleted_at == null)]", Params.none, false, true);
      ("$[?@.deleted_at == %s]", Params.positional [ Value.Null ], true, false);
      ("$[?@.deleted_at != %(at)d]", Params.named [ ("at", Value.Null) ], false, true);
      ("$[?@.deleted_at == %d]", Params.positional [ vint 5 ], false, true);
      (* An order with null is null, and so is its negation. *)
      ("$[?@.deleted_at > 1]", Params.none, false, true);
      ("$[?!(@.deleted_at > 1)]", Params.none, false, false);
      ("$[?@.deleted_at != 5]", Params.none, false, false);
      ("$[?@.deleted_at < null]", Params.none, false, false);
    ];
  Alcotest.check bound "is null"
    (Ok (is_null (field "deleted_at")))
    (Template.bind (template "$[?@.deleted_at == null]") Params.none);
  Alcotest.check bound "is not null"
    (Ok (any "items" (is_not_null (item "price"))))
    (Template.bind
       (template "$.items[*][?@.price != %s]")
       (Params.positional [ Value.Null ]));
  (* The template itself is what was written: the rule is of the values, which a template
     has when it is bound. *)
  Alcotest.check slot_tree "as written"
    (eq (field "deleted_at") (literal Value.Null))
    (Template.expr (template "$[?@.deleted_at == null]"))

let logical_operators () =
  let alice = user 30 "Alice" true in
  List.iter
    (fun (source, params, expected) ->
      Alcotest.check matched source (Ok expected)
        (Template.matches (template source) alice params))
    [
      ( "$[?@.age > %d && @.active == %s]",
        Params.positional [ vint 25; Value.Bool true ],
        true );
      ( "$[?@.age > %d && @.active == %s]",
        Params.positional [ vint 35; Value.Bool true ],
        false );
      ("$[?@.age < %d || @.age > %d]", Params.positional [ vint 18; vint 25 ], true);
      ("$[?@.age < %d || @.age > %d]", Params.positional [ vint 18; vint 65 ], false);
      ("$[?!(@.active == %s)]", Params.positional [ Value.Bool false ], true);
      ("$[?!(@.active == %s)]", Params.positional [ Value.Bool true ], false);
      ("$[?(@.age >= 18 && @.age <= 65) && @.active == true]", Params.none, true);
      ("$[?!!@.active]", Params.none, true);
    ]

let and_binds_tighter_than_or_and_both_nest_to_the_left () =
  let a () = gt (field "a") (int 1)
  and b () = gt (field "b") (int 2)
  and c () = gt (field "c") (int 3) in
  List.iter
    (fun (source, expected) ->
      Alcotest.check slot_tree source expected (Template.expr (template source)))
    [
      ("$[?@.a > 1 && @.b > 2 && @.c > 3]", and_ (and_ (a ()) (b ())) (c ()));
      ("$[?@.a > 1 || @.b > 2 || @.c > 3]", or_ (or_ (a ()) (b ())) (c ()));
      ("$[?@.a > 1 || @.b > 2 && @.c > 3]", or_ (a ()) (and_ (b ()) (c ())));
      ("$[?@.a > 1 && @.b > 2 || @.c > 3]", or_ (and_ (a ()) (b ())) (c ()));
      ("$[?(@.a > 1 || @.b > 2) && @.c > 3]", and_ (or_ (a ()) (b ())) (c ()));
      ("$[?@.a > 1 && (@.b > 2 || @.c > 3)]", and_ (a ()) (or_ (b ()) (c ())));
      (* [!] takes the comparison after it, as the sources read it. *)
      ("$[?!@.a > 1 && @.b > 2]", and_ (not_ (a ())) (b ()));
      ("$[?((@.a > 1))]", a ());
    ]

let paths_and_what_at_means () =
  (* In a filter on the candidate, [@] is the candidate. *)
  Alcotest.check slot_tree "candidate"
    (gt (field "user.profile.age") (positional 0 Param_kind.Integer))
    (Template.expr (template "$[?@.user.profile.age > %d]"));
  (* In a filter on a collection, [@] is the item; [$] is still the candidate. *)
  Alcotest.check slot_tree "item"
    (any "store.items" (gt (item "price") (field "limit")))
    (Template.expr (template "$.store.items[*][?@.price > $.limit]"));
  (* Either side of a comparison is any operand. *)
  Alcotest.check slot_tree "either side"
    (lt (positional 0 Param_kind.Integer) (field "age"))
    (Template.expr (template "$[?%d < @.age]"));
  let store = store () in
  Alcotest.check matched "over the limit" (Ok true)
    (Template.matches (template "$.items[*][?@.price > $.limit]") store Params.none);
  Alcotest.check matched "warehouse 10" (Ok true)
    (Template.matches
       (template "$.warehouse.items[*][?@.stock < %d]")
       store
       (Params.positional [ vint 10 ]));
  Alcotest.check matched "warehouse 3" (Ok false)
    (Template.matches
       (template "$.warehouse.items[*][?@.stock < %d]")
       store
       (Params.positional [ vint 3 ]))

let collections_nest () =
  let dear = template "$.categories[*][?@.items[*][?@.price > %f]]" in
  Alcotest.check slot_tree "nested"
    (any "categories"
       (any_at (Path.item "items") (gt (item "price") (positional 0 Param_kind.Number))))
    (Template.expr dear);
  let store = store () in
  Alcotest.check matched "500" (Ok true)
    (Template.matches dear store (Params.positional [ Value.Float 500.0 ]));
  Alcotest.check matched "1000" (Ok false)
    (Template.matches dear store (Params.positional [ Value.Float 1000.0 ]));
  let both =
    template
      "$.categories[*][?@.name == %(category)s && @.items[*][?@.price > %(price)f && \
       @.stock > 0]]"
  in
  let of_ category price =
    Params.named [ ("category", vtext category); ("price", Value.Float price) ]
  in
  Alcotest.check matched "electronics" (Ok true)
    (Template.matches both store (of_ "Electronics" 500.0));
  Alcotest.check matched "stationery" (Ok false)
    (Template.matches both store (of_ "Stationery" 500.0))

let a_member_that_is_not_there_is_an_error () =
  Alcotest.check matched "missing"
    (Error (Match_error.Eval (Evaluate.Context (Context.Missing "nonexistent"))))
    (Template.matches
       (template "$[?@.nonexistent > %d]")
       (user 30 "Alice" true)
       (Params.positional [ vint 1 ]))

let an_error_says_what_where_and_shows_it () =
  let unexpected = error "$[?@.a # 1]" in
  Alcotest.(check string) "message" "Unexpected character '#'" unexpected.message;
  Alcotest.(check int) "position" 7 unexpected.position;
  Alcotest.(check string) "expected" "valid token" unexpected.expected;
  Alcotest.(check string) "expression" "$[?@.a # 1]" unexpected.expression;
  Alcotest.(check string)
    "shown"
    "Unexpected character '#' at position 7 (expected valid token)\n\
    \  $[?@.a # 1]\n\
    \         ^"
    (Syntax_error.to_string unexpected);
  let ended = error "$[?@.age >" in
  Alcotest.(check (pair string int))
    "ended"
    ("Unexpected end of expression", 10)
    (ended.message, ended.position);
  (* A position counts characters, so the caret stands under the right one. *)
  Alcotest.(check int) "characters" 14 (error "$[?@.a == '\xC3\xA9' # 1]").position

(* An error shows a control character by its escape, not as it is: in the message, and in
   the line that echoes the template, whose caret moves by what the escape adds. *)
let a_control_character_is_shown_by_its_escape () =
  Alcotest.(check string)
    "in a name"
    "Unexpected character '\\x00' at position 7 (expected valid token)\n\
    \  $[?@.na\\x00me == 1]\n\
    \         ^"
    (Syntax_error.to_string (error "$[?@.na\x00me == 1]"));
  Alcotest.(check string)
    "in a string"
    "Control character in a string at position 15 (expected its escape, \\n or \\uXXXX)\n\
    \  $[?@.name == 'a\\x00b' # 1]\n\
    \                 ^"
    (Syntax_error.to_string (error "$[?@.name == 'a\x00b' # 1]"))

let what_the_grammar_does_not_have_is_refused () =
  List.iter
    (fun (source, message, position) ->
      let error = error source in
      Alcotest.(check (pair string int))
        source (message, position)
        (error.message, error.position))
    [
      ("", "Expected '$'", 0);
      ("@.age > 1", "Expected '$'", 0);
      ("$", "Expected filter expression '[?...]'", 1);
      ("$.items", "Expected wildcard '[*]'", 7);
      ("$[?@. > 1]", "Expected field name", 6);
      ("$[?@ > 1]", "Expected field name", 5);
      ("$[?@.age > ]", "Unexpected token ']'", 11);
      ("$[?@.age > foo]", "Unexpected token 'foo'", 11);
      ("$[?@.age > %x]", "Malformed placeholder", 11);
      ("$[?@.age > %(age]", "Malformed placeholder", 11);
      ("$[?@.name == 'open]", "Unterminated string", 13);
      ("$[?@.name == 'a\\qb']", "Invalid escape", 15);
      (* RFC 9535, 2.3.5.1: unescaped, a character of a string is %x20 and up. A raw one
         was taken into the string - a NUL among them, which went as far as the server
         and failed there. *)
      ("$[?@.name == 'a\x00b']", "Control character in a string", 15);
      ("$[?@.name == 'a\tb']", "Control character in a string", 15);
      ("$[?@.name == 'a\nb']", "Control character in a string", 15);
      ("$[?@.age > 99999999999999999999]", "Number out of range", 11);
      ({|$[?@.name == '\u00zz']|}, "Invalid escape", 14);
      ({|$[?@.name == '\uD83Dx']|}, "Invalid escape", 14);
      ({|$[?@.name == '\uDE00']|}, "Invalid escape", 14);
      ("$[?@.age > 1e999]", "Number out of range", 11);
      (* What the sources read as something other than what it says. *)
      ("$[?@.age > 1", "Expected ']'", 12);
      ("$[?(@.age > 1]", "Expected ')'", 13);
      ("$[?@.age > 1)]", "Expected ']'", 12);
      ("$[?@.age > 1]]", "Unexpected token ']'", 13);
      ("$[?@.age > 1] extra", "Unexpected token 'extra'", 14);
      ("$[@.age > 1]", "Expected filter expression '[?...]'", 2);
      ("$[?age > 1]", "Unexpected token 'age'", 3);
      ("$.items[?@.price > 1]", "Expected wildcard '[*]'", 8);
      ("$[?@.items[?@.price > 1]]", "Expected wildcard '[*]'", 11);
      ("$.items[*]", "Expected filter expression '[?...]'", 10);
      ("$[?@.a == 1 == 2]", "Expected ']'", 12);
      ( "$[?@.a > %d && @.b > %(b)d]",
        "Positional and named placeholders in one template",
        21 );
    ]

(* The levels of the longest way down a tree: what a reader of it recurses through. *)
let rec height = function
  | Value _ | Field _ -> 1
  | Prefix (_, operand) | Postfix (operand, _) | Any (_, operand) -> 1 + height operand
  | Infix (left, _, right) -> 1 + max (height left) (height right)

let repeat text times = String.concat "" (List.init times (fun _ -> text))

(* Groups nested on the left, [levels] of them, each the first operand of a chain of [&&]
   that is the first operand of a chain of [||], [links] long each. How deep the parser is
   grows by one with a group; how tall the tree is, by two chains. *)
let groups_on_the_left levels links =
  let inner =
    List.fold_left
      (fun inner level ->
        let n = links level in
        Printf.sprintf "(%s%s%s)" inner (repeat " && @.a" n) (repeat " || @.a" n))
      "@.a"
      (List.rev (List.init levels (fun i -> i + 1)))
  in
  Printf.sprintf "$[?%s]" inner

let too_deep source =
  Alcotest.(check string) source "Expression is nested too deep" (error source).message

let a_group_on_the_left_counts_towards_the_depth () =
  (* As long a chain after each group as a count of how deep the parser is lets through:
     the count was passed down to the right operands alone, so this was a tree of some
     sixteen thousand levels, and binding it overflowed the stack. *)
  too_deep (groups_on_the_left 127 (fun level -> 128 - level))

(* The bounds, to the level. A tree of 128 levels is a template and one of 129 is not,
   whether it grows by a chain or by a comparison above a chain. The parser goes 32 deep
   and no deeper, by groups - which add no level to the tree - by [!], or by the filters
   of collections. *)
let the_bounds_are_held_to_the_level () =
  let links operands = "@.a" ^ repeat " && @.a" (operands - 1) in
  let tall source = height (Template.expr (template source)) in
  Alcotest.(check int) "128 links" 128 (tall (Printf.sprintf "$[?%s]" (links 128)));
  too_deep (Printf.sprintf "$[?%s]" (links 129));
  (* The chain is the LEFT operand of the comparison: it was read before anything knew of
     an operator over it. *)
  Alcotest.(check int)
    "left chain" 128
    (tall (Printf.sprintf "$[?(%s) == true]" (links 127)));
  too_deep (Printf.sprintf "$[?(%s) == true]" (links 128));
  let grouped groups =
    Printf.sprintf "$[?%s@.a%s]" (repeat "(" groups) (repeat ")" groups)
  in
  Alcotest.(check int) "32 groups" 1 (tall (grouped 32));
  too_deep (grouped 33);
  let negated nots = Printf.sprintf "$[?%s@.a]" (repeat "!" nots) in
  Alcotest.(check int) "32 nots" 33 (tall (negated 32));
  too_deep (negated 33);
  let filtered filters =
    Printf.sprintf "$[?%s@.a%s]" (repeat "@.items[*][?" filters) (repeat "]" filters)
  in
  Alcotest.(check int) "32 filters" 33 (tall (filtered 32));
  too_deep (filtered 33)

(* Whatever the shape, a template is refused or its tree is within the bound: groups at
   the left of chains and at the right, under [!], as the predicates of collections, and
   chains of every length about the bound. *)
let no_tree_of_a_template_is_taller_than_the_bound () =
  let shapes =
    [
      (fun inner links -> Printf.sprintf "(%s%s)" inner links);
      (fun inner links -> Printf.sprintf "(@.a%s && %s)" links inner);
      (fun inner links -> Printf.sprintf "!(%s%s)" inner links);
      (fun inner links -> Printf.sprintf "@.items[*][?%s%s]" inner links);
      (fun inner links -> Printf.sprintf "(%s%s) == (%s)" inner links inner);
    ]
  in
  let accepted = ref 0 in
  List.iter
    (fun wrap ->
      List.iter
        (fun levels ->
          List.iter
            (fun length ->
              let links = repeat " && @.a" length ^ repeat " || @.a" length in
              (* The last shape doubles the text with each level. *)
              let levels =
                if String.length (wrap "" "") > 0 && String.contains (wrap "" "") '=' then
                  min levels 7
                else levels
              in
              let inner =
                List.fold_left
                  (fun inner _ -> wrap inner links)
                  "@.a" (List.init levels Fun.id)
              in
              match Template.parse (Printf.sprintf "$[?%s]" inner) with
              | Ok template ->
                  incr accepted;
                  Alcotest.(check bool)
                    (Printf.sprintf "%d levels of %d" levels length)
                    true
                    (height (Template.expr template) <= 128)
              | Error error ->
                  Alcotest.(check string)
                    "refused" "Expression is nested too deep" error.message)
            [ 0; 1; 5; 40; 63; 64; 126; 127; 128 ])
        [ 1; 2; 3; 7; 20; 60; 127 ])
    shapes;
  (* Not all refused: the property is of trees that were made. *)
  Alcotest.(check bool) "accepted" true (!accepted > 50)

(* What the bounds are for: the deepest templates there can be are parsed, and their
   trees go through everything that reads a tree - bound, evaluated, compiled,
   transformed, compared, shown. *)
let the_deepest_template_is_read_by_everything () =
  let same : (Value.t, Value.t, string) Mapping.t =
    {
      field = (fun path -> Ok (Scalar (Field path)));
      value = (fun value -> Ok (Scalar (Value value)));
    }
  in
  let deep open_ close times =
    Printf.sprintf "$[?%s@.a == %%d%s]" (repeat open_ times) (repeat close times)
  in
  (* As deep as the parser goes and as tall as a tree gets, at once: 32 filters, the inner
     one the first operand of a chain of three. *)
  let both =
    List.fold_left
      (fun inner level ->
        (* A comparison is two levels; a filter over a chain of three, four. *)
        let links = if level >= 30 then 2 else 3 in
        Printf.sprintf "@.items[*][?%s%s]" inner (repeat " && @.a == %d" links))
      "@.a == %d" (List.init 32 Fun.id)
  in
  let sources =
    [
      (Printf.sprintf "$[?@.a == %%d%s]" (repeat " && @.a == %d" 126), 128);
      (deep "(" ")" 32, 2);
      (* A [!] and a group are a level of the parser each. *)
      (deep "!(" ")" 16, 18);
      (deep "@.items[*][?" "]" 32, 34);
      (Printf.sprintf "$[?%s]" both, 128);
    ]
  in
  List.iter
    (fun (source, tall) ->
      let template = template source in
      Alcotest.(check int) source tall (height (Template.expr template));
      let count = List.length (String.split_on_char '%' source) - 1 in
      let params = Params.positional (List.init count (fun _ -> vint 1)) in
      let item = Record.(object_ [ ("a", value (vint 1)) ]) in
      let record =
        List.fold_left
          (fun item _ ->
            Record.(object_ [ ("a", value (vint 1)); ("items", collection [ item ]) ]))
          item (List.init 33 Fun.id)
      in
      (match Template.matches template (Record.to_context record) params with
      | Ok _ -> ()
      | Error error -> Alcotest.fail (Match_error.to_string error));
      let bound =
        match Template.bind template params with
        | Ok bound -> bound
        | Error e -> Alcotest.fail (Bind_error.to_string e)
      in
      (match Pg.compile bound with
      | Ok query -> Alcotest.(check int) "params" count (List.length query.params)
      | Error error -> Alcotest.fail (Pg.error_to_string error));
      Alcotest.(check (result value_tree string))
        "transformed" (Ok bound)
        (Result.map_error (Mapping.error_to_string Fun.id) (Mapping.transform same bound));
      Alcotest.check value_tree "equal" bound bound;
      Alcotest.(check bool) "shown" true (String.length (Ast.show Value.pp bound) > 0))
    sources

(* A text is read in a time that grows as its length: a lexer that counted its
   placeholders over again for each token took the square of it. *)
let a_long_text_is_refused_in_the_time_it_takes_to_read_it () =
  let long = Printf.sprintf "$[?@.a == %%d%s]" (repeat " @.a %d" 30_000) in
  Alcotest.(check string) "long" "Expected ']'" (error long).message

(* The bounds on height and nesting bound the shape of a tree and not the size of a
   text: the length is the first thing looked at, in bytes of UTF-8, so a template is one
   in every port or in none. *)
let a_template_longer_than_the_bound_is_refused_before_it_is_read () =
  let room = "$[?@.a == 1]" in
  let at_the_bound =
    Printf.sprintf "$[?@.a == 1%s]" (String.make (max_length - String.length room) ' ')
  in
  Alcotest.(check int) "at the bound" max_length (String.length at_the_bound);
  Alcotest.(check bool) "parses" true (Result.is_ok (Template.parse at_the_bound));
  Alcotest.(check string)
    "over" "Template too long at position 262144 (expected at most 262144 bytes of UTF-8)"
    (Syntax_error.to_string (error (at_the_bound ^ " ")));
  Alcotest.(check bool)
    "bytes not characters" true
    (Result.is_error
       (Template.parse (Printf.sprintf "$[?@.a == '%s']" (repeat "\xC3\xA9" 131_072))));
  let started = Unix.gettimeofday () in
  let chain =
    Printf.sprintf "$[?%s]"
      (String.concat " && " (List.init 400_000 (fun _ -> "@.a == 1")))
  in
  Alcotest.(check string) "chain" "Template too long" (error chain).message;
  Alcotest.(check bool) "fast" true (Unix.gettimeofday () -. started < 1.0)

let a_tree_has_a_bound_on_its_depth () =
  too_deep (Printf.sprintf "$[?%s@.a%s]" (repeat "(" 200) (repeat ")" 200));
  too_deep (Printf.sprintf "$[?%s@.a]" (repeat "!" 200));
  too_deep (Printf.sprintf "$[?@.a%s]" (repeat " && @.a" 200));
  Alcotest.(check bool)
    "100 links" true
    (Result.is_ok (Template.parse (Printf.sprintf "$[?@.a%s]" (repeat " && @.a" 100))))

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "jsonpath"
    [
      ( "templates",
        [
          case "comparisons with positional placeholders"
            comparisons_with_positional_placeholders;
          case "placeholders by name and of every kind"
            placeholders_by_name_and_of_every_kind;
          case "a template is parsed once and bound many times"
            a_template_is_parsed_once_and_bound_many_times;
          case "parameters that do not fit are refused"
            parameters_that_do_not_fit_are_refused;
          case "literals" literals;
          case "a null is tested, not compared" a_null_is_tested_not_compared;
          case "logical operators" logical_operators;
          case "and binds tighter than or and both nest to the left"
            and_binds_tighter_than_or_and_both_nest_to_the_left;
          case "paths and what @ means" paths_and_what_at_means;
          case "collections nest" collections_nest;
          case "a member that is not there is an error"
            a_member_that_is_not_there_is_an_error;
        ] );
      ( "errors",
        [
          case "an error says what, where, and shows it"
            an_error_says_what_where_and_shows_it;
          case "a control character is shown by its escape"
            a_control_character_is_shown_by_its_escape;
          case "what the grammar does not have is refused"
            what_the_grammar_does_not_have_is_refused;
        ] );
      ( "bounds",
        [
          case "a group on the left counts towards the depth"
            a_group_on_the_left_counts_towards_the_depth;
          case "the bounds are held to the level" the_bounds_are_held_to_the_level;
          case "no tree of a template is taller than the bound"
            no_tree_of_a_template_is_taller_than_the_bound;
          case "the deepest template is read by everything"
            the_deepest_template_is_read_by_everything;
          case "a long text is refused in the time it takes to read it"
            a_long_text_is_refused_in_the_time_it_takes_to_read_it;
          case "a template longer than the bound is refused before it is read"
            a_template_longer_than_the_bound_is_refused_before_it_is_read;
          case "a tree has a bound on its depth" a_tree_has_a_bound_on_its_depth;
        ] );
    ]
