(* The tree and its evaluation: what the Python [test_specification] and the Go
   [specification_test] and [operators_test] check, and the three-valued logic the Go
   registry has. *)

open Ascetic_specification
open Ast
module E = Evaluate.Make (Value)
module Null_test = Null_test.Make (Value)

let int i = value (Value.of_int i)
let int64 i = value (Value.Int i)
let float f = value (Value.Float f)
let text s = value (Value.Text s)
let bool b = value (Value.Bool b)
let null = value Value.Null
let nothing = Record.to_context (Record.object_ [])
let constant expr = E.evaluate expr nothing
let value_t = Alcotest.testable Value.pp Value.equal
let eval_error = Alcotest.testable Evaluate.pp_error Evaluate.equal_error
let evaluated = Alcotest.(result value_t eval_error)
let satisfied = Alcotest.(result bool eval_error)
let spec = Alcotest.testable (Ast.pp Value.pp) (Ast.equal Value.equal)
let path = Alcotest.testable Path.pp Path.equal
let root = Alcotest.testable Path.pp_root Path.equal_root

let check_each testable cases =
  List.iter
    (fun (name, actual, expected) -> Alcotest.check testable name expected actual)
    cases

let store () =
  Record.(
    to_context
      (object_
         [
           ("name", value (Value.Text "MyStore"));
           ( "items",
             collection
               [
                 object_
                   [
                     ("name", value (Value.Text "Laptop"));
                     ("price", value (Value.of_int 999));
                   ];
                 object_
                   [
                     ("name", value (Value.Text "Mouse"));
                     ("price", value (Value.of_int 29));
                   ];
               ] );
           ("empty", collection []);
           ("owner", object_ [ ("profile", object_ [ ("age", value (Value.of_int 30)) ]) ]);
         ]))

let a_path_is_never_empty_and_keeps_its_names_in_order () =
  let p = Path.(child (child (global "user") "profile") "age") in
  Alcotest.check root "root" Path.Global (Path.root p);
  Alcotest.(check (list string)) "objects" [ "user"; "profile" ] (Path.objects p);
  Alcotest.(check string) "name" "age" (Path.name p);
  Alcotest.(check (list string)) "names" [ "user"; "profile"; "age" ] (Path.names p);
  Alcotest.check path "dotted" p (Path.of_string "user.profile.age");
  Alcotest.check path "dotted item" (Path.item "price")
    (Path.dotted (Path.Item 0) "price");
  Alcotest.check path "outer 0" (Path.item "price") (Path.outer 0 "price");
  Alcotest.check root "outer 1" (Path.Item 1) (Path.root (Path.outer 1 "price"))

let several_operands_nest_to_the_left () =
  let a, b, c = (field "a", field "b", field "c") in
  Alcotest.check spec "and_all" (and_ (and_ a b) c) (and_all a [ b; c ]);
  Alcotest.check spec "or_all" (or_ (or_ a b) c) (or_all a [ b; c ]);
  Alcotest.check spec "alone" a (and_all a [])

let an_operator_is_matched_by_its_constant () =
  match and_ (field "a") (eq (field "b") (int 1)) with
  | Infix (_, Logical And, Infix (_, Comparison Eq, _)) -> ()
  | _ -> Alcotest.fail "an AND over an EQ"

let comparisons () =
  check_each evaluated
    (List.map
       (fun (name, expr, expected) -> (name, constant expr, Ok (Value.Bool expected)))
       [
         ("5 = 5", eq (int 5) (int 5), true);
         ("5 = 10", eq (int 5) (int 10), false);
         ("5 != 10", ne (int 5) (int 10), true);
         ("5 != 5", ne (int 5) (int 5), false);
         ("10 > 5", gt (int 10) (int 5), true);
         ("5 > 10", gt (int 5) (int 10), false);
         ("5 < 10", lt (int 5) (int 10), true);
         ("10 < 5", lt (int 10) (int 5), false);
         ("5 >= 5", ge (int 5) (int 5), true);
         ("4 >= 5", ge (int 4) (int 5), false);
         ("5 <= 5", le (int 5) (int 5), true);
         ("6 <= 5", le (int 6) (int 5), false);
         ("a = a", eq (text "a") (text "a"), true);
         ("a < b", lt (text "a") (text "b"), true);
         ("true = true", eq (bool true) (bool true), true);
         ("true != false", ne (bool true) (bool false), true);
         (* False comes before true, as in PostgreSQL. *)
         ("true > false", gt (bool true) (bool false), true);
         ("true <= false", le (bool true) (bool false), false);
         (* An integer meeting a float is promoted, as in PostgreSQL. *)
         ("1 = 1.0", eq (int 1) (float 1.0), true);
         ("1 < 1.5", lt (int 1) (float 1.5), true);
         (* PostgreSQL's NaN equals itself and is the greatest number. *)
         ("nan = nan", eq (float Float.nan) (float Float.nan), true);
         ("nan > max", gt (float Float.nan) (float Float.max_float), true);
       ])

let arithmetic_is_postgresqls () =
  let interval micros = Value.Interval (Value.Interval.of_micros micros) in
  ignore interval;
  check_each evaluated
    (List.map
       (fun (name, expr, expected) -> (name, constant expr, Ok expected))
       [
         ("5 + 3", add (int 5) (int 3), Value.of_int 8);
         ("5 - 3", sub (int 5) (int 3), Value.of_int 2);
         ("5 * 3", mul (int 5) (int 3), Value.of_int 15);
         (* Integer division truncates, towards zero. *)
         ("7 / 2", div (int 7) (int 2), Value.of_int 3);
         ("-7 / 2", div (int (-7)) (int 2), Value.of_int (-3));
         ("-7 % 2", modulo (int (-7)) (int 2), Value.of_int (-1));
         ("min % -1", modulo (int64 Int64.min_int) (int (-1)), Value.of_int 0);
         ("7.0 / 2", div (float 7.0) (int 2), Value.Float 3.5);
         ("1 + 0.5", add (int 1) (float 0.5), Value.Float 1.5);
         ("-5", neg (int 5), Value.of_int (-5));
         ("-2.5", neg (float 2.5), Value.Float (-2.5));
         ("1 << 3", left_shift (int 1) (int 3), Value.of_int 8);
         ("8 >> 2", right_shift (int 8) (int 2), Value.of_int 2);
         (* The count of a shift is taken modulo 64. *)
         ("1 << 64", left_shift (int 1) (int 64), Value.of_int 1);
         ("1 << -1", left_shift (int 1) (int (-1)), Value.Int Int64.min_int);
         ("8 >> 65", right_shift (int 8) (int 65), Value.of_int 4);
       ])

let arithmetic_fails_where_postgresql_fails () =
  check_each evaluated
    (List.map
       (fun (name, expr, expected) ->
         (name, constant expr, Error (Evaluate.Operand expected)))
       [
         ("1 / 0", div (int 1) (int 0), Operand.Division_by_zero);
         ("1 % 0", modulo (int 1) (int 0), Operand.Division_by_zero);
         ("1.0 / 0.0", div (float 1.0) (float 0.0), Operand.Division_by_zero);
         ("max + 1", add (int64 Int64.max_int) (int 1), Operand.Out_of_range);
         ("max * 2", mul (int64 Int64.max_int) (int 2), Operand.Out_of_range);
         ("min / -1", div (int64 Int64.min_int) (int (-1)), Operand.Out_of_range);
         ("-min", neg (int64 Int64.min_int), Operand.Out_of_range);
         ("max * 2.0", mul (float Float.max_float) (float 2.0), Operand.Out_of_range);
       ])

let an_operator_names_itself_and_its_operands_when_it_does_not_apply () =
  let unsupported operator left right =
    Error (Evaluate.Operand (Operand.Unsupported { operator; left; right }))
  in
  check_each evaluated
    [
      (">=", constant (ge (int 5) (text "5")), unsupported ">=" "integer" (Some "text"));
      ( "!=",
        constant (ne (bool true) (int 1)),
        unsupported "!=" "boolean" (Some "integer") );
      (">", constant (gt (text "a") (float 1.5)), unsupported ">" "text" (Some "float"));
      ( "%",
        constant (modulo (float 5.5) (int 2)),
        unsupported "%" "float" (Some "integer") );
      ("+", constant (add (text "a") (text "b")), unsupported "+" "text" (Some "text"));
      ("-", constant (neg (text "a")), unsupported "-" "text" None);
    ]

let time () =
  let noon = Value.Timestamp.of_micros 43_200_000_000L in
  let hour = Value.Interval.of_micros 3_600_000_000L in
  let later = Value.Timestamp.of_micros 46_800_000_000L in
  let point t = value (Value.Timestamp t) and span i = value (Value.Interval i) in
  check_each evaluated
    (List.map
       (fun (name, expr, expected) -> (name, constant expr, Ok expected))
       [
         ("later - noon", sub (point later) (point noon), Value.Interval hour);
         ("noon + hour", add (point noon) (span hour), Value.Timestamp later);
         ("hour + noon", add (span hour) (point noon), Value.Timestamp later);
         ("later - hour", sub (point later) (span hour), Value.Timestamp noon);
         ( "hour + hour",
           add (span hour) (span hour),
           Value.Interval (Value.Interval.of_micros 7_200_000_000L) );
         ( "hour - hour",
           sub (span hour) (span hour),
           Value.Interval (Value.Interval.of_micros 0L) );
         ( "-hour",
           neg (span hour),
           Value.Interval (Value.Interval.of_micros (-3_600_000_000L)) );
         ("noon < later", lt (point noon) (point later), Value.Bool true);
         ("hour = hour", eq (span hour) (span hour), Value.Bool true);
       ]);
  Alcotest.(check bool)
    "noon + noon" true
    (Result.is_error (constant (add (point noon) (point noon))));
  Alcotest.(check bool)
    "hour * 2" true
    (Result.is_error (constant (mul (span hour) (int 2))))

let a_null_operand_makes_a_null_result () =
  List.iter
    (fun expr ->
      Alcotest.check evaluated (Ast.show Value.pp expr) (Ok Value.Null) (constant expr))
    [
      eq null (int 1);
      eq null null;
      ne (int 1) null;
      lt null (int 1);
      add (int 1) null;
      div null (int 0);
      neg null;
      not_ null;
    ]

let the_connectives_are_three_valued () =
  let t = bool true and f = bool false in
  check_each evaluated
    (List.map
       (fun (name, expr, expected) -> (name, constant expr, Ok expected))
       [
         ("t and t", and_ t t, Value.Bool true);
         ("t and f", and_ t f, Value.Bool false);
         ("null and f", and_ null f, Value.Bool false);
         ("f and null", and_ f null, Value.Bool false);
         ("null and t", and_ null t, Value.Null);
         ("t and null", and_ t null, Value.Null);
         ("null and null", and_ null null, Value.Null);
         ("f or f", or_ f f, Value.Bool false);
         ("f or t", or_ f t, Value.Bool true);
         ("null or t", or_ null t, Value.Bool true);
         ("t or null", or_ t null, Value.Bool true);
         ("null or f", or_ null f, Value.Null);
         ("f or null", or_ f null, Value.Null);
         ("not t", not_ t, Value.Bool false);
         ("not f", not_ f, Value.Bool true);
       ]);
  check_each evaluated
    [
      ("1 and t", constant (and_ (int 1) t), Error (Evaluate.Not_boolean "integer"));
      ("f or x", constant (or_ f (text "x")), Error (Evaluate.Not_boolean "text"));
      ("not 1", constant (not_ (int 1)), Error (Evaluate.Not_boolean "integer"));
    ]

let a_decided_connective_does_not_evaluate_its_right_side () =
  let fails () = eq (div (int 1) (int 0)) (int 1) in
  Alcotest.check evaluated "false and fails" (Ok (Value.Bool false))
    (constant (and_ (bool false) (fails ())));
  Alcotest.check evaluated "true or fails" (Ok (Value.Bool true))
    (constant (or_ (bool true) (fails ())));
  Alcotest.(check bool)
    "true and fails" true
    (Result.is_error (constant (and_ (bool true) (fails ()))));
  Alcotest.(check bool)
    "fails and false" true
    (Result.is_error (constant (and_ (fails ()) (bool false))))

let is_and_is_null_are_never_null () =
  check_each evaluated
    (List.map
       (fun (name, expr, expected) -> (name, constant expr, Ok (Value.Bool expected)))
       [
         ("t is t", is (bool true) (bool true), true);
         ("t is f", is (bool true) (bool false), false);
         ("null is null", is null null, true);
         ("null is t", is null (bool true), false);
         ("1 is null", is (int 1) null, false);
         ("1 is 1", is (int 1) (int 1), true);
         ("null is null (postfix)", is_null null, true);
         ("42 is null", is_null (int 42), false);
         ("42 is not null", is_not_null (int 42), true);
         ("null is not null", is_not_null null, false);
       ])

let members_are_reached_through_objects () =
  let store = store () in
  Alcotest.check evaluated "name" (Ok (Value.Text "MyStore"))
    (E.evaluate (field "name") store);
  Alcotest.check evaluated "owner.profile.age"
    (Ok (Value.of_int 30))
    (E.evaluate (field "owner.profile.age") store);
  Alcotest.check satisfied "age > 25" (Ok true)
    (E.is_satisfied_by (gt (field "owner.profile.age") (int 25)) store)

let a_member_that_is_not_there_is_an_error () =
  let store = store () in
  let missing name = Error (Evaluate.Context (Context.Missing name)) in
  Alcotest.check evaluated "nonexistent" (missing "nonexistent")
    (E.evaluate (field "nonexistent") store);
  Alcotest.check evaluated "absent.age" (missing "absent")
    (E.evaluate (field "absent.age") store);
  Alcotest.check evaluated "owner"
    (Error (Evaluate.Context (Context.Not_a_value "owner")))
    (E.evaluate (field "owner") store);
  Alcotest.check evaluated "name.first"
    (Error (Evaluate.Context (Context.Not_an_object "name")))
    (E.evaluate (field "name.first") store);
  Alcotest.check evaluated "any name"
    (Error (Evaluate.Context (Context.Not_a_collection "name")))
    (E.evaluate (any "name" (bool true)) store)

(* The guard a host-language frontend writes for "is some and" over a Value Object,
   [discount IS NOT NULL AND discount.percent > 10], asks whether an object is null. An
   object that is there is no value, and was [Not_a_value] to the test; it is not null. An
   object that is not there is a null value, [Record.value Value.Null]: null to the test,
   and no object to go into, as the domain's unwrapping of a [None] has none. A member
   left out is another thing, missing. *)
let a_null_test_of_an_object_asks_whether_it_is_there () =
  let present =
    Record.(
      to_context
        (object_ [ ("discount", object_ [ ("percent", value (Value.of_int 15)) ]) ]))
  in
  let absent = Record.(to_context (object_ [ ("discount", value Value.Null) ])) in
  let guarded () =
    and_ (is_not_null (field "discount")) (gt (field "discount.percent") (int 10))
  in
  Alcotest.check satisfied "present is not null" (Ok true)
    (E.is_satisfied_by (is_not_null (field "discount")) present);
  Alcotest.check satisfied "present is null" (Ok false)
    (E.is_satisfied_by (is_null (field "discount")) present);
  Alcotest.check satisfied "guarded present" (Ok true)
    (E.is_satisfied_by (guarded ()) present);
  Alcotest.check satisfied "absent is null" (Ok true)
    (E.is_satisfied_by (is_null (field "discount")) absent);
  Alcotest.check satisfied "guarded absent" (Ok false)
    (E.is_satisfied_by (guarded ()) absent);
  Alcotest.check evaluated "absent.percent"
    (Error (Evaluate.Context (Context.Not_an_object "discount")))
    (E.evaluate (field "discount.percent") absent);
  (* Under any other operator an object is no value, as it was; and a member left out is
     missing, under a null test as anywhere. *)
  Alcotest.check evaluated "present = 1"
    (Error (Evaluate.Context (Context.Not_a_value "discount")))
    (E.evaluate (eq (field "discount") (int 1)) present);
  Alcotest.check evaluated "nothing is null"
    (Error (Evaluate.Context (Context.Missing "discount")))
    (E.evaluate (is_null (field "discount")) nothing)

let some_item_satisfies_the_predicate () =
  let store = store () in
  let dearer_than price = any "items" (gt (item "price") (int price)) in
  Alcotest.check satisfied "dearer than 500" (Ok true)
    (E.is_satisfied_by (dearer_than 500) store);
  Alcotest.check satisfied "dearer than 1000" (Ok false)
    (E.is_satisfied_by (dearer_than 1000) store);
  Alcotest.check satisfied "empty" (Ok false)
    (E.is_satisfied_by (any "empty" (gt (item "price") (int 0))) store);
  (* The candidate is in reach of the predicate too. *)
  Alcotest.check satisfied "named as the store" (Ok false)
    (E.is_satisfied_by (any "items" (eq (item "name") (field "name"))) store)

let any_is_never_null_and_a_null_predicate_is_no_witness () =
  let store = store () in
  let unknown = any "items" (gt (item "price") null) in
  Alcotest.check evaluated "unknown" (Ok (Value.Bool false)) (E.evaluate unknown store);
  Alcotest.check evaluated "not unknown" (Ok (Value.Bool true))
    (E.evaluate (not_ unknown) store)

let any_stops_at_its_first_witness () =
  (* The second item would fail: a text compared with a number. *)
  let first_is_dear =
    any "items" (or_ (gt (item "price") (int 500)) (gt (item "name") (int 1)))
  in
  Alcotest.check satisfied "first is dear" (Ok true)
    (E.is_satisfied_by first_is_dear (store ()))

let all_is_no_item_failing () =
  let store = store () in
  let dearer_than price = all "items" (gt (item "price") (int price)) in
  Alcotest.check satisfied "all dearer than 10" (Ok true)
    (E.is_satisfied_by (dearer_than 10) store);
  Alcotest.check satisfied "all dearer than 500" (Ok false)
    (E.is_satisfied_by (dearer_than 500) store);
  (* Nothing fails in an empty collection. *)
  Alcotest.check satisfied "empty" (Ok true)
    (E.is_satisfied_by (all "empty" (eq (item "price") (int 0))) store);
  Alcotest.check spec "all is not any not"
    (not_ (any "items" (not_ (item "active"))))
    (all "items" (item "active"))

let the_item_is_only_inside_a_collection () =
  Alcotest.check evaluated "item outside" (Error Evaluate.No_current_item)
    (E.evaluate (item "price") (store ()));
  (* As is the item a collection out: one collection deep, there is none. *)
  Alcotest.check evaluated "outer outside" (Error Evaluate.No_current_item)
    (E.evaluate (any "items" (gt (outer 1 "limit") (int 1))) (store ()))

(* A shop with categories, each with a limit and products of its own. *)
let shop () =
  let category limit prices =
    Record.(
      object_
        [
          ("limit", value (Value.of_int limit));
          ( "products",
            collection
              (List.map
                 (fun price -> object_ [ ("price", value (Value.of_int price)) ])
                 prices) );
        ])
  in
  Record.(
    to_context
      (object_
         [
           ("limit", value (Value.of_int 50));
           ("categories", collection [ category 10 [ 5; 20 ]; category 100 [ 30 ] ]);
         ]))

(* The item of an enclosing collection is named from an inner predicate: [Item 1] is the
   item one collection out, as [Item 0] - the item - is the nearest. *)
let the_item_of_an_enclosing_collection_is_named_by_how_far_out_it_is () =
  let price () = item "price" and category_limit () = outer 1 "limit" in
  let over_its_category predicate =
    any "categories" (any_at (Path.item "products") predicate)
  in
  check_each evaluated
    (List.map
       (fun (name, specification, expected) ->
         (name, E.evaluate specification (shop ()), Ok (Value.Bool expected)))
       [
         (* 20 > 10 in the first category; 30 > 100 is not. *)
         ("over its category", over_its_category (gt (price ()) (category_limit ())), true);
         (* A product is priced over the shop's limit in neither. *)
         ("over the shop", over_its_category (gt (price ()) (field "limit")), false);
         (* The limit of the category, from the inner predicate, beside the shop's. *)
         ( "both limits",
           over_its_category
             (and_
                (gt (price ()) (category_limit ()))
                (lt (category_limit ()) (field "limit"))),
           true );
         (* The nearest item is still the product. *)
         ("nearest", over_its_category (gt (price ()) (int 25)), true);
         (* From the outer predicate the category is the item, at depth 0. *)
         ("category at depth 0", any "categories" (gt (item "limit") (int 50)), true);
       ])

let the_predicate_of_any_is_a_boolean () =
  Alcotest.check evaluated "integer predicate" (Error (Evaluate.Not_boolean "integer"))
    (E.evaluate (any "items" (item "price")) (store ()))

let a_candidate_satisfies_what_is_true_of_it () =
  let store = store () in
  Alcotest.check satisfied "true" (Ok true) (E.is_satisfied_by (bool true) store);
  Alcotest.check satisfied "false" (Ok false) (E.is_satisfied_by (bool false) store);
  (* As a row with a null condition is not selected. *)
  Alcotest.check satisfied "null" (Ok false) (E.is_satisfied_by null store);
  Alcotest.check satisfied "1" (Error (Evaluate.Not_boolean "integer"))
    (E.is_satisfied_by (int 1) store)

let equality_with_the_null_constant_is_the_null_test () =
  let a () = field "a" in
  check_each spec
    [
      ("a = null", Null_test.equal (a ()) null, is_null (a ()));
      ("null = a", Null_test.equal null (a ()), is_null (a ()));
      ("a != null", Null_test.not_equal (a ()) null, is_not_null (a ()));
      ("null != a", Null_test.not_equal null (a ()), is_not_null (a ()));
      (* Of two nulls one is tested: true, as in the notations this is for. *)
      ("null = null", Null_test.equal null null, is_null null);
      (* Anything else is the comparison it says. *)
      ("a = 1", Null_test.equal (a ()) (int 1), eq (a ()) (int 1));
      ("a != b", Null_test.not_equal (a ()) (field "b"), ne (a ()) (field "b"));
    ]

let the_null_test_is_found_throughout_a_tree () =
  let a () = field "a" and it () = item "price" in
  let tree =
    and_ (not_ (eq (a ()) null)) (any "items" (or_ (ne null (it ())) (lt (it ()) null)))
  in
  Alcotest.check spec "throughout"
    (and_
       (not_ (is_null (a ())))
       (* An order with null is left what it is: null, true of nothing. *)
       (any "items" (or_ (is_not_null (it ())) (lt (it ()) null))))
    (Null_test.throughout tree);
  (* The tree by itself keeps SQL's meaning: [a = NULL] is null. *)
  let store = store () in
  Alcotest.check evaluated "name = null" (Ok Value.Null)
    (E.evaluate (eq (field "name") null) store);
  Alcotest.check evaluated "name is null" (Ok (Value.Bool false))
    (E.evaluate (Null_test.equal (field "name") null) store)

let the_values_of_a_tree_can_be_mapped () =
  let int_tree =
    Alcotest.testable
      (Ast.pp (fun fmt v -> Format.fprintf fmt "%Ld" v))
      (Ast.equal Int64.equal)
  in
  let specification =
    and_ (eq (field "a") (int 1)) (any "items" (eq (item "b") (int 2)))
  in
  let doubled =
    Ast.try_map_values
      (function Value.Int n -> Ok (Int64.mul n 2L) | other -> Error (Value.show other))
      specification
  in
  Alcotest.(check (result int_tree string))
    "doubled"
    (Ok (and_ (eq (field "a") (Value 2L)) (any "items" (eq (item "b") (Value 4L)))))
    doubled;
  let refused =
    Ast.try_map_values
      (function Value.Int n -> Ok n | other -> Error (Value.show other))
      (eq (text "x") (int 1))
  in
  Alcotest.(check (result int_tree string)) "refused" (Error "(Text \"x\")") refused

(* A template has the literals of RFC 9535 and no others, so a point in time or a UUID in
   it is a string. The server reads an untyped parameter by the column; the evaluator
   compared a string with a point in time and refused, and the two readers parted on
   [@.created_at > '2026-09-01']. A string constant beside a value of a kind that has no
   literal of its own is read as that kind, within a subset of what the server reads
   (ADR-0015 of the reference). The rows are in [test_pg]. *)
let a_string_constant_is_read_as_the_kind_of_the_member_beside_it () =
  let today = Option.get (Value.Date.of_civil 2026 9 1) in
  let at hour =
    Value.Timestamp
      (Value.Timestamp.of_micros
         (Int64.add
            (Int64.mul (Int64.of_int (Value.Date.to_days today)) 86_400_000_000L)
            (Int64.mul (Int64.of_int hour) 3_600_000_000L)))
  in
  let uid = Option.get (Uuidm.of_string "3f2a0c1e-5b7d-4e8a-9f01-23456789abcd") in
  let row =
    Record.(
      to_context
        (object_
           [
             ("at", value (at 12));
             ("day", value (Value.Date today));
             ("uid", value (Value.Uuid uid));
             ("price", value (Value.of_int 100));
             ("name", value (Value.Text "2026-09-01"));
           ]))
  in
  let holds specification = E.is_satisfied_by specification row in
  let at = field "at" and day = field "day" and uid = field "uid" in
  check_each satisfied
    (List.map
       (fun (name, specification, expected) -> (name, holds specification, Ok expected))
       [
         (* Midnight, UTC without an offset; the forms of the subset. *)
         ("a date alone", gt at (text "2026-09-01"), true);
         ("Z", gt at (text "2026-09-01T12:00:00Z"), false);
         ("a space for the T", gt at (text "2026-09-01 12:00:00"), false);
         ("an offset", gt at (text "2026-09-01T15:00:00+03:00"), false);
         ("no seconds", gt at (text "2026-09-01T12:00"), false);
         ("a fraction", gt at (text "2026-09-01T11:59:59.999999Z"), true);
         ("on either side", eq (text "2026-09-01T15:00:00+03:00") at, true);
         ("under IS", is at (text "2026-09-01T12:00:00Z"), true);
         (* A date takes the date of a full timestamp, as the server does: the time and
            the offset are not looked at. *)
         ("a date", eq day (text "2026-09-01"), true);
         ("a date, ordered", lt day (text "2026-09-02"), true);
         ("the date of a timestamp", lt day (text "2026-09-01T12:00:00Z"), false);
         ("the date before the offset", eq day (text "2026-09-01T23:59:59+03:00"), true);
         ( "a UUID in upper case",
           eq uid (text "3F2A0C1E-5B7D-4E8A-9F01-23456789ABCD"),
           true );
         ("another UUID", ne uid (text "00000000-0000-0000-0000-000000000000"), true);
         (* Two strings are two strings. *)
         ("a string beside a string", eq (field "name") (text "2026-09-01"), true);
       ]);
  (* What the server reads beyond the subset, and what nothing reads: loud here, and never
     the other way round. *)
  let unreadable name specification =
    match holds specification with
    | Error (Evaluate.Operand (Operand.Unreadable _)) -> ()
    | other -> Alcotest.failf "%s: %a" name (Alcotest.pp satisfied) other
  in
  List.iter
    (fun text_ ->
      List.iter (fun member -> unreadable text_ (gt member (text text_))) [ at; day ])
    [
      "yesterday";
      "20260901";
      "Sep 1 2026";
      "2026-13-01";
      "2026-09-01T25:00:00Z";
      "2026-09-01T23:59:60Z";
      "";
    ];
  List.iter
    (fun text_ -> unreadable text_ (eq uid (text text_)))
    [
      "{3f2a0c1e-5b7d-4e8a-9f01-23456789abcd}";
      "3f2a0c1e5b7d4e8a9f0123456789abcd";
      "not-a-uuid";
    ];
  (* A number has a literal: a string beside it is meant, and does not compare. A member
     holding a string is the candidate's data, not a constant. And [at + '1 day'] is an
     interval to the server, another reading: here it is the error it was. *)
  List.iter
    (fun (name, specification) ->
      match E.evaluate specification row with
      | Error (Evaluate.Operand (Operand.Unsupported _)) -> ()
      | other -> Alcotest.failf "%s: %a" name (Alcotest.pp evaluated) other)
    [
      ("a string beside a number", gt (field "price") (text "100"));
      ("a member holding a string", gt (field "name") at);
      ("added, not compared", add at (text "1 day"));
    ]

(* A date is days since the Unix epoch, as a point in time is microseconds: a civil date
   converts to it, and the count is the one PostgreSQL's is taken from. *)
let a_date_is_days_since_the_unix_epoch () =
  let date = Alcotest.testable Value.Date.pp Value.Date.equal in
  let days = Value.Date.of_days in
  check_each
    Alcotest.(option date)
    [
      ("1970-01-01", Value.Date.of_civil 1970 1 1, Some (days 0));
      ("2000-01-01", Value.Date.of_civil 2000 1 1, Some (days 10_957));
      ("1969-12-31", Value.Date.of_civil 1969 12 31, Some (days (-1)));
      ("2024-02-29", Value.Date.of_civil 2024 2 29, Some (days 19_782));
      ("2026-02-29", Value.Date.of_civil 2026 2 29, None);
    ];
  check_each
    Alcotest.(triple int int int)
    [
      ("day 0", Value.Date.to_civil (days 0), (1970, 1, 1));
      ("day 10957", Value.Date.to_civil (days 10_957), (2000, 1, 1));
      ("day -1", Value.Date.to_civil (days (-1)), (1969, 12, 31));
    ];
  Alcotest.(check bool) "ordered" true (Value.Date.compare (days 1) (days 2) < 0)

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "specification"
    [
      ( "tree",
        [
          case "a path is never empty and keeps its names in order"
            a_path_is_never_empty_and_keeps_its_names_in_order;
          case "several operands nest to the left" several_operands_nest_to_the_left;
          case "an operator is matched by its constant"
            an_operator_is_matched_by_its_constant;
          case "the values of a tree can be mapped" the_values_of_a_tree_can_be_mapped;
        ] );
      ( "values",
        [
          case "comparisons" comparisons;
          case "arithmetic is PostgreSQL's" arithmetic_is_postgresqls;
          case "arithmetic fails where PostgreSQL fails"
            arithmetic_fails_where_postgresql_fails;
          case "an operator names itself and its operands when it does not apply"
            an_operator_names_itself_and_its_operands_when_it_does_not_apply;
          case "time" time;
          case "a string constant is read as the kind of the member beside it"
            a_string_constant_is_read_as_the_kind_of_the_member_beside_it;
          case "a date is days since the Unix epoch" a_date_is_days_since_the_unix_epoch;
        ] );
      ( "nulls",
        [
          case "a null operand makes a null result" a_null_operand_makes_a_null_result;
          case "the connectives are three-valued" the_connectives_are_three_valued;
          case "a decided connective does not evaluate its right side"
            a_decided_connective_does_not_evaluate_its_right_side;
          case "IS and IS NULL are never null" is_and_is_null_are_never_null;
          case "equality with the null constant is the null test"
            equality_with_the_null_constant_is_the_null_test;
          case "the null test is found throughout a tree"
            the_null_test_is_found_throughout_a_tree;
        ] );
      ( "candidates",
        [
          case "members are reached through objects" members_are_reached_through_objects;
          case "a member that is not there is an error"
            a_member_that_is_not_there_is_an_error;
          case "a null test of an object asks whether it is there"
            a_null_test_of_an_object_asks_whether_it_is_there;
          case "a candidate satisfies what is true of it"
            a_candidate_satisfies_what_is_true_of_it;
        ] );
      ( "collections",
        [
          case "some item satisfies the predicate" some_item_satisfies_the_predicate;
          case "any is never null and a null predicate is no witness"
            any_is_never_null_and_a_null_predicate_is_no_witness;
          case "any stops at its first witness" any_stops_at_its_first_witness;
          case "all is no item failing" all_is_no_item_failing;
          case "the item is only inside a collection" the_item_is_only_inside_a_collection;
          case "the item of an enclosing collection is named by how far out it is"
            the_item_of_an_enclosing_collection_is_named_by_how_far_out_it_is;
          case "the predicate of any is a boolean" the_predicate_of_any_is_a_boolean;
        ] );
    ]
