(* Typed terms: what the Python [test_public] and the Go [public_test] check. That a term
   of one sort does not take the operators of another is checked by the compiler, where
   the sources check nothing: [add (Number.field "age") (Text.field "name")],
   [and_ (Number.field "age") (Number.field "rank")], [eq (Number.field "age") (Text.value "18")]
   and [is_null (Number.field "age")] do not type-check. *)

open Ascetic_specification
open Dsl
module E = Evaluate.Make (Value)

let spec = Alcotest.testable (Ast.pp Value.pp) (Ast.equal Value.equal)
let int i = Ast.value (Value.of_int i)
let check name expected actual = Alcotest.check spec name expected (expr actual)

let fields_and_values () =
  check "field" (Ast.field "age") (Number.field "age");
  check "dotted field" (Ast.field "user.profile.age") (Number.field "user.profile.age");
  check "item field" (Ast.item "price") (Number.field_at (Path.item "price"));
  check "int" (int 18) (Number.of_int 18);
  check "float" (Ast.value (Value.Float 1.5)) (Number.of_float 1.5);
  check "text" (Ast.value (Value.Text "Alice")) (Text.value "Alice");
  check "bool" (Ast.value (Value.Bool true)) (Boolean.value true);
  check "some int" (int 18) (Null_number.of_int (Some 18));
  check "none text" (Ast.value Value.Null) (Null_text.value None)

let booleans_combine () =
  let active () = Boolean.field "active" and deleted () = Null_boolean.field "deleted" in
  let a () = Ast.field "active" and d () = Ast.field "deleted" in
  check "and" (Ast.and_ (a ()) (d ())) (and_ (active ()) (deleted ()));
  check "or" (Ast.or_ (a ()) (d ())) (or_ (active ()) (deleted ()));
  check "not" (Ast.not_ (a ())) (not_ (active ()));
  check "is"
    (Ast.is (a ()) (Ast.value (Value.Bool true)))
    (is (active ()) (Boolean.value true));
  check "is null" (Ast.is_null (d ())) (is_null (deleted ()));
  check "is not null" (Ast.is_not_null (d ())) (is_not_null (deleted ()));
  (* [&&] binds tighter than [||], in OCaml as in SQL. *)
  check "precedence"
    (Ast.or_ (a ()) (Ast.and_ (d ()) (a ())))
    Infix.(active () || (deleted () && active ()))

let comparables_compare () =
  let age () = Number.field "age" and n () = Ast.field "age" in
  let eighteen () = Number.of_int 18 in
  check "eq" (Ast.eq (n ()) (int 18)) (eq (age ()) (eighteen ()));
  check "ne" (Ast.ne (n ()) (int 18)) (ne (age ()) (eighteen ()));
  check "gt" (Ast.gt (n ()) (int 18)) (gt (age ()) (eighteen ()));
  check "lt" (Ast.lt (n ()) (int 18)) (lt (age ()) (eighteen ()));
  check "ge" (Ast.ge (n ()) (int 18)) (ge (age ()) (eighteen ()));
  check "le" (Ast.le (n ()) (int 18)) (le (age ()) (eighteen ()));
  check "text with nullable text"
    (Ast.eq (Ast.field "name") (Ast.field "alias"))
    (eq (Text.field "name") (Null_text.field "alias"))

let a_null_is_tested_not_compared () =
  (* [email == maybe], of an option known when the term is built. *)
  let email () = Null_text.field "email" in
  check "eq none" (Ast.is_null (Ast.field "email")) (eq (email ()) (Null_text.value None));
  check "ne none"
    (Ast.is_not_null (Ast.field "email"))
    (ne (email ()) (Null_text.value None));
  check "eq some"
    (Ast.eq (Ast.field "email") (Ast.value (Value.Text "a@b")))
    (eq (email ()) (Null_text.value (Some "a@b")))

let numbers_compute () =
  let a () = Number.field "a" and b () = Null_number.field "b" in
  let x () = Ast.field "a" and y () = Ast.field "b" in
  check "+" (Ast.add (x ()) (y ())) Infix.(a () + b ());
  check "-" (Ast.sub (x ()) (y ())) Infix.(a () - b ());
  check "*" (Ast.mul (x ()) (y ())) Infix.(a () * b ());
  check "/" (Ast.div (x ()) (y ())) Infix.(a () / b ());
  check "%" (Ast.modulo (x ()) (y ())) Infix.(a () mod b ());
  check "<<" (Ast.left_shift (x ()) (y ())) (shl (a ()) (b ()));
  check ">>" (Ast.right_shift (x ()) (y ())) (shr (a ()) (b ()));
  check "neg" (Ast.neg (x ())) Infix.(-a ());
  (* The operators bind as SQL's do: [*] over [-] over [>]. *)
  check "composed"
    (Ast.gt (Ast.mul (Ast.sub (x ()) (y ())) (int 2)) (int 100))
    Infix.((a () - b ()) * Number.of_int 2 > Number.of_int 100);
  check "named"
    (Ast.gt (Ast.mul (Ast.sub (x ()) (y ())) (int 2)) (int 100))
    (gt (mul (sub (a ()) (b ())) (Number.of_int 2)) (Number.of_int 100))

let time_computes_to_the_sort_it_makes () =
  let created () = Datetime.field "created_at"
  and updated () = Null_datetime.field "updated_at" in
  let day () = Timespan.value (Value.Interval.of_micros 86_400_000_000L) in
  let age : Timespan.t = Datetime.diff (updated ()) (created ()) in
  let tomorrow : Datetime.t = Datetime.add (created ()) (day ()) in
  let yesterday : Datetime.t = Datetime.sub (created ()) (day ()) in
  let two_days : Timespan.t = Timespan.add (day ()) (day ()) in
  let back : Timespan.t = neg (day ()) in
  check "age > day"
    (Ast.gt (Ast.sub (Ast.field "updated_at") (Ast.field "created_at")) (expr (day ())))
    (gt age (day ()));
  check "tomorrow" (Ast.add (Ast.field "created_at") (expr (day ()))) tomorrow;
  check "yesterday" (Ast.sub (Ast.field "created_at") (expr (day ()))) yesterday;
  check "two days" (Ast.add (expr (day ())) (expr (day ()))) two_days;
  check "back" (Ast.neg (expr (day ()))) back;
  check "created < epoch"
    (Ast.lt (Ast.field "created_at")
       (Ast.value (Value.Timestamp (Value.Timestamp.of_micros 0L))))
    (lt (created ()) (Datetime.value (Value.Timestamp.of_micros 0L)))

let a_tree_built_otherwise_comes_in_as_the_sort_it_is_declared () =
  let dear : Boolean.t =
    of_expr
      (Ast.any "items"
         (expr (gt (Number.field_at (Path.item "price")) (Number.of_int 500))))
  in
  check "declared boolean"
    (Ast.and_ (Ast.field "active")
       (Ast.any "items" (Ast.gt (Ast.item "price") (int 500))))
    (and_ (Boolean.field "active") dear)

let what_is_built_is_a_specification () =
  let specification =
    expr
      Infix.(
        Number.field "price" - Number.field "discount" < Number.of_int 100
        && is_null (Null_datetime.field "deleted_at"))
  in
  let product =
    Record.(
      to_context
        (object_
           [
             ("price", value (Value.of_int 120));
             ("discount", value (Value.of_int 30));
             ("deleted_at", value Value.Null);
           ]))
  in
  Alcotest.(check (result bool (testable Evaluate.pp_error Evaluate.equal_error)))
    "satisfied" (Ok true)
    (E.is_satisfied_by specification product);
  match Pg.compile specification with
  | Ok query ->
      Alcotest.(check string)
        "compiled" {|"price" - "discount" < $1 AND "deleted_at" IS NULL|} query.sql
  | Error error -> Alcotest.fail (Pg.error_to_string error)

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "dsl"
    [
      ( "terms",
        [
          case "fields and values" fields_and_values;
          case "booleans combine" booleans_combine;
          case "comparables compare" comparables_compare;
          case "a null is tested, not compared" a_null_is_tested_not_compared;
          case "numbers compute" numbers_compute;
          case "time computes to the sort it makes" time_computes_to_the_sort_it_makes;
          case "a tree built otherwise comes in as the sort it is declared"
            a_tree_built_otherwise_comes_in_as_the_sort_it_is_declared;
          case "what is built is a specification" what_is_built_is_a_specification;
        ] );
    ]
