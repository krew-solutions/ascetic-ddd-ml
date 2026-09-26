(* [let%specification]: what the Python [test_lambda_parser] and the Go specgen
   [main_test] check - the tree a predicate function is read as - and that the function
   and its tree agree on the same candidates. *)

open Ascetic_specification
module E = Evaluate.Make (Value)

type profile = { age : int64 }
type item = { name : string; price : int64; active : bool; discount : int64 option }
type category = { items : item list }

type store = {
  name : string;
  rating : float;
  active : bool;
  closed_at : int64 option;
  alias : string option;
  owner : profile;
  items : item list;
  categories : category list;
}

let%specification adult_owner (store : store) = store.owner.age >= 18L

let%specification premium (s : store) =
  (s.owner.age >= 18L && s.active && s.name <> "") || s.rating > 4.5

let%specification arithmetic (s : store) =
  Int64.sub (Int64.mul (Int64.add s.owner.age 1L) 2L) 3L > Int64.rem (Int64.div 10L 2L) 3L
  && Int64.shift_right (Int64.shift_left s.owner.age 1) 1 = s.owner.age
  && Int64.neg s.owner.age < -5L

let%specification open_ (s : store) =
  Option.is_none s.closed_at && not (Option.is_some s.closed_at)

let%specification methods (s : store) =
  s.owner.age >= 18L
  && s.owner.age <> 0L = true
  && s.rating < 5.0 && String.equal s.name "MyStore"

let%specification has_dear_items (s : store) =
  List.exists (fun (item : item) -> item.price > 500L && item.active) s.items

let%specification all_items_active (s : store) =
  List.for_all (fun (item : item) -> item.active) s.items

let%specification has_dear_item_in_a_category (s : store) =
  List.exists
    (fun (category : category) ->
      List.exists (fun (item : item) -> item.price > 500L) category.items)
    s.categories

let%specification has_an_item_named_as_the_store (s : store) =
  List.exists (fun (item : item) -> item.name = s.name) s.items

let%specification older_than (s : store) (age : int64) (name : string) =
  s.owner.age > age && s.owner.age < Int64.add age 100L && s.name = name

(* [<> None] is what [Option.is_some] is. *)
let%specification closed (s : store) = s.closed_at <> None
let%specification closed_on (s : store) = s.closed_at = Some 1_700_000_000L

(* A parameter that may be none: known only when the tree is asked for. *)
let%specification closed_at (s : store) (at : int64 option) = s.closed_at = at
let%specification known_as (s : store) (alias : string option) = s.alias = alias

(* What an option holds is asked under a name. The null test beside the predicate makes
   the whole of two values, so it holds under a [not] too. *)
let%specification closed_before (s : store) (at : int64) =
  Option.fold ~none:false ~some:(fun closed -> closed < at) s.closed_at

let%specification not_closed_before (s : store) (at : int64) =
  not (Option.fold ~none:false ~some:(fun closed -> closed < at) s.closed_at)

let%specification open_or_closed_after (s : store) (at : int64) =
  Option.fold ~none:true ~some:(fun closed -> closed > at) s.closed_at

let%specification known_as_pens (s : store) =
  match s.alias with None -> false | Some alias -> alias = "Pens"

(* A parameter that is an option is asked the same way, and what it holds is compared
   with what the member holds: an option itself has no order. *)
let%specification closed_after (s : store) (at : int64 option) =
  Option.fold ~none:false
    ~some:(fun limit ->
      Option.fold ~none:false ~some:(fun closed -> closed > limit) s.closed_at)
    at

let%specification not_closed_after (s : store) (at : int64 option) =
  not
    (Option.fold ~none:false
       ~some:(fun limit ->
         Option.fold ~none:false ~some:(fun closed -> closed > limit) s.closed_at)
       at)

let%specification closed_before_if_asked (s : store) (at : int64 option) =
  match at with
  | None -> true
  | Some at -> Option.fold ~none:false ~some:(fun closed -> closed < at) s.closed_at

(* What the item holds, beside the item it is a member of. *)
let%specification has_a_well_discounted_item (s : store) =
  List.exists
    (fun (item : item) ->
      match item.discount with
      | None -> false
      | Some discount -> Int64.mul discount 10L > Int64.sub item.price 10L)
    s.items

(* The constants of a specification are its parameters: what the reference keeps as the
   fields of a specification type. *)
let%specification dear_and_open (s : store) (min : int64) (closed_before : int64 option) =
  List.exists (fun (item : item) -> item.price > min) s.items
  &&
  match closed_before with
  | None -> true
  | Some at -> Option.fold ~none:false ~some:(fun closed -> closed < at) s.closed_at

let spec = Alcotest.testable (Ast.pp Value.pp) (Ast.equal Value.equal)
let int i = Ast.value (Value.of_int i)
let int64 i = Ast.value (Value.Int i)
let text s = Ast.value (Value.Text s)
let null = Ast.value Value.Null

let the_tree_of_a_predicate () =
  let open Ast in
  let age () = field "owner.age" in
  List.iter
    (fun (name, tree, expected) -> Alcotest.check spec name expected tree)
    [
      ("adult_owner", adult_owner_ast, ge (age ()) (int64 18L));
      (* OCaml's [&&] and [||] group to the right, where the reference's group to the
         left; the connectives regroup freely, so the query is the same. *)
      ( "premium",
        premium_ast,
        or_
          (and_
             (ge (age ()) (int64 18L))
             (and_ (field "active") (ne (field "name") (text ""))))
          (gt (field "rating") (value (Value.Float 4.5))) );
      ( "arithmetic",
        arithmetic_ast,
        and_
          (gt
             (sub (mul (add (age ()) (int64 1L)) (int64 2L)) (int64 3L))
             (modulo (div (int64 10L) (int64 2L)) (int64 3L)))
          (and_
             (eq (right_shift (left_shift (age ()) (int 1)) (int 1)) (age ()))
             (lt (neg (age ())) (int64 (-5L)))) );
      ( "open",
        open__ast,
        and_ (is_null (field "closed_at")) (not_ (is_not_null (field "closed_at"))) );
      ( "methods",
        methods_ast,
        and_
          (ge (age ()) (int64 18L))
          (and_
             (eq (ne (age ()) (int64 0L)) (value (Value.Bool true)))
             (and_
                (lt (field "rating") (value (Value.Float 5.0)))
                (eq (field "name") (text "MyStore")))) );
      ( "has_dear_items",
        has_dear_items_ast,
        any "items" (and_ (gt (item "price") (int64 500L)) (item "active")) );
      ("all_items_active", all_items_active_ast, all "items" (item "active"));
      ( "has_dear_item_in_a_category",
        has_dear_item_in_a_category_ast,
        any "categories" (any_at (Path.item "items") (gt (item "price") (int64 500L))) );
      ( "has_an_item_named_as_the_store",
        has_an_item_named_as_the_store_ast,
        any "items" (eq (item "name") (field "name")) );
      ( "older_than",
        older_than_ast 25L "MyStore",
        and_
          (gt (age ()) (int64 25L))
          (and_
             (lt (age ()) (add (int64 25L) (int64 100L)))
             (eq (field "name") (text "MyStore"))) );
      (* A comparison with none is the null test, in the tree as in OCaml. *)
      ("closed", closed_ast, is_not_null (field "closed_at"));
      ("closed_on", closed_on_ast, eq (field "closed_at") (int64 1_700_000_000L));
      ("closed_at None", closed_at_ast None, is_null (field "closed_at"));
      ("closed_at Some", closed_at_ast (Some 7L), eq (field "closed_at") (int64 7L));
      ("known_as None", known_as_ast None, is_null (field "alias"));
      ("known_as Some", known_as_ast (Some "Pens"), eq (field "alias") (text "Pens"));
      (* What an option holds: the null test, and the predicate of the same member. *)
      ( "closed_before",
        closed_before_ast 5L,
        and_ (is_not_null (field "closed_at")) (lt (field "closed_at") (int64 5L)) );
      ( "not_closed_before",
        not_closed_before_ast 5L,
        not_ (and_ (is_not_null (field "closed_at")) (lt (field "closed_at") (int64 5L)))
      );
      ( "open_or_closed_after",
        open_or_closed_after_ast 5L,
        or_ (is_null (field "closed_at")) (gt (field "closed_at") (int64 5L)) );
      ( "known_as_pens",
        known_as_pens_ast,
        and_ (is_not_null (field "alias")) (eq (field "alias") (text "Pens")) );
      ( "closed_after",
        closed_after_ast (Some 5L),
        and_
          (is_not_null (int64 5L))
          (and_ (is_not_null (field "closed_at")) (gt (field "closed_at") (int64 5L))) );
      ( "closed_before_if_asked",
        closed_before_if_asked_ast None,
        or_ (is_null null)
          (and_ (is_not_null (field "closed_at")) (lt (field "closed_at") null)) );
      ( "has_a_well_discounted_item",
        has_a_well_discounted_item_ast,
        any "items"
          (and_
             (is_not_null (item "discount"))
             (gt (mul (item "discount") (int64 10L)) (sub (item "price") (int64 10L)))) );
      ( "dear_and_open",
        dear_and_open_ast 500L (Some 5L),
        and_
          (any "items" (gt (item "price") (int64 500L)))
          (or_
             (is_null (int64 5L))
             (and_ (is_not_null (field "closed_at")) (lt (field "closed_at") (int64 5L))))
      );
    ]

(* What a domain object does to be a candidate: say which of its members are values,
   which objects, which collections. *)
let missing name = Error (Context.Missing name)
let opt_int = Value.of_option (fun v -> Value.Int v)
let opt_text = Value.of_option Value.of_string

let profile_context (p : profile) : Value.t Context.t =
  {
    field = (function "age" -> Ok (Value.Int p.age) | name -> missing name);
    object_ = missing;
    collection = missing;
  }

let item_context (i : item) : Value.t Context.t =
  {
    field =
      (function
      | "name" -> Ok (Value.Text i.name)
      | "price" -> Ok (Value.Int i.price)
      | "active" -> Ok (Value.Bool i.active)
      | "discount" -> Ok (opt_int i.discount)
      | name -> missing name);
    object_ = missing;
    collection = missing;
  }

let category_context (c : category) : Value.t Context.t =
  {
    field = missing;
    object_ = missing;
    collection =
      (function "items" -> Ok (List.map item_context c.items) | name -> missing name);
  }

let store_context (s : store) : Value.t Context.t =
  {
    field =
      (function
      | "name" -> Ok (Value.Text s.name)
      | "rating" -> Ok (Value.Float s.rating)
      | "active" -> Ok (Value.Bool s.active)
      | "closed_at" -> Ok (opt_int s.closed_at)
      | "alias" -> Ok (opt_text s.alias)
      | name -> missing name);
    object_ = (function "owner" -> Ok (profile_context s.owner) | name -> missing name);
    collection =
      (function
      | "items" -> Ok (List.map item_context s.items)
      | "categories" -> Ok (List.map category_context s.categories)
      | name -> missing name);
  }

let stores =
  (* A dear item has a discount, of a tenth of its price. *)
  let item name price active =
    {
      name;
      price;
      active;
      discount =
        (if Int64.compare price 100L > 0 then Some (Int64.div price 10L) else None);
    }
  in
  [
    {
      name = "MyStore";
      rating = 4.0;
      active = true;
      closed_at = None;
      alias = None;
      owner = { age = 30L };
      items = [ item "Laptop" 999L true; item "Mouse" 29L true ];
      categories = [ { items = [ item "Laptop" 999L true ] } ];
    };
    {
      name = "Pen";
      rating = 4.9;
      active = false;
      closed_at = Some 1_700_000_000L;
      alias = Some "Pens";
      owner = { age = 17L };
      items = [ item "Pen" 2L false; item "Ink" 900L false ];
      categories = [ { items = [] }; { items = [ item "Pen" 2L true ] } ];
    };
    {
      name = "";
      rating = 0.0;
      active = true;
      closed_at = None;
      alias = Some "Inks";
      owner = { age = 125L };
      items = [];
      categories = [];
    };
  ]

let satisfied = Alcotest.(result bool (testable Evaluate.pp_error Evaluate.equal_error))

let agree name (predicate : store -> bool) tree =
  List.iter
    (fun store ->
      Alcotest.check satisfied
        (Printf.sprintf "%s of %S" name store.name)
        (Ok (predicate store))
        (E.is_satisfied_by tree (store_context store)))
    stores

let the_function_and_its_tree_agree () =
  List.iter
    (fun (name, predicate, tree) -> agree name predicate tree)
    [
      ("closed", closed, closed_ast);
      ("closed_on", closed_on, closed_on_ast);
      ("adult_owner", adult_owner, adult_owner_ast);
      ("premium", premium, premium_ast);
      ("arithmetic", arithmetic, arithmetic_ast);
      ("open", open_, open__ast);
      ("methods", methods, methods_ast);
      ("has_dear_items", has_dear_items, has_dear_items_ast);
      ("all_items_active", all_items_active, all_items_active_ast);
      ( "has_dear_item_in_a_category",
        has_dear_item_in_a_category,
        has_dear_item_in_a_category_ast );
      ( "has_an_item_named_as_the_store",
        has_an_item_named_as_the_store,
        has_an_item_named_as_the_store_ast );
      ("known_as_pens", known_as_pens, known_as_pens_ast);
      ( "has_a_well_discounted_item",
        has_a_well_discounted_item,
        has_a_well_discounted_item_ast );
    ];
  List.iter
    (fun age ->
      agree
        (Printf.sprintf "older_than %Ld" age)
        (fun s -> older_than s age "MyStore")
        (older_than_ast age "MyStore"))
    [ 10L; 25L; 30L; 200L ];
  List.iter
    (fun at -> agree "closed_at" (fun s -> closed_at s at) (closed_at_ast at))
    [ None; Some 1_700_000_000L; Some 5L ];
  List.iter
    (fun at ->
      agree
        (Printf.sprintf "closed_before %Ld" at)
        (fun s -> closed_before s at)
        (closed_before_ast at);
      agree
        (Printf.sprintf "not_closed_before %Ld" at)
        (fun s -> not_closed_before s at)
        (not_closed_before_ast at);
      agree
        (Printf.sprintf "open_or_closed_after %Ld" at)
        (fun s -> open_or_closed_after s at)
        (open_or_closed_after_ast at))
    [ 0L; 1_700_000_000L; 1_700_000_001L ];
  List.iter
    (fun at ->
      agree "closed_after" (fun s -> closed_after s at) (closed_after_ast at);
      agree "not_closed_after" (fun s -> not_closed_after s at) (not_closed_after_ast at);
      agree "closed_before_if_asked"
        (fun s -> closed_before_if_asked s at)
        (closed_before_if_asked_ast at))
    [ None; Some 0L; Some 1_700_000_000L; Some 1_700_000_001L ];
  List.iter
    (fun min ->
      List.iter
        (fun closed_before ->
          agree
            (Printf.sprintf "dear_and_open %Ld" min)
            (fun s -> dear_and_open s min closed_before)
            (dear_and_open_ast min closed_before))
        [ None; Some 5L; Some 1_700_000_001L ])
    [ 2L; 500L ];
  List.iter
    (fun alias -> agree "known_as" (fun s -> known_as s alias) (known_as_ast alias))
    [ None; Some "Pens"; Some "Inks" ]

let compiled specification =
  match Pg.compile specification with
  | Ok query -> query
  | Error e -> Alcotest.fail (Pg.error_to_string e)

let value_t = Alcotest.testable Value.pp Value.equal

let the_tree_compiles_to_a_query () =
  let query = compiled has_dear_items_ast in
  Alcotest.(check string)
    "dear items"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."price" > $1 AND "item_1"."active")|}
    query.sql;
  Alcotest.(check (list value_t)) "params" [ Value.Int 500L ] query.params;
  Alcotest.(check string)
    "all active"
    {|NOT EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE NOT "item_1"."active")|}
    (compiled all_items_active_ast).sql;
  (* What an option holds, of a parameter and of a member: a none is a null constant,
     which has no neighbour to take its type from. *)
  let query = compiled (not_closed_after_ast None) in
  Alcotest.(check string)
    "not closed after"
    {|NOT ($1::text IS NOT NULL AND "closed_at" IS NOT NULL AND "closed_at" > $2)|}
    query.sql;
  Alcotest.(check (list value_t)) "nulls" [ Value.Null; Value.Null ] query.params;
  Alcotest.(check string)
    "closed before if asked"
    {|$1::bigint IS NULL OR "closed_at" IS NOT NULL AND "closed_at" < $2|}
    (compiled (closed_before_if_asked_ast (Some 5L))).sql;
  let query = compiled (dear_and_open_ast 500L None) in
  Alcotest.(check string)
    "dear and open"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."price" > $1) AND ($2::text IS NULL OR "closed_at" IS NOT NULL AND "closed_at" < $3)|}
    query.sql;
  Alcotest.(check (list value_t))
    "params"
    [ Value.Int 500L; Value.Null; Value.Null ]
    query.params

(* What cannot be a tree is reported where it stands, and the function stays: the second
   item of the expansion is the error. *)
let error_of source =
  let open Ppxlib in
  match Parse.implementation (Lexing.from_string source) with
  | [ { pstr_desc = Pstr_value (_, [ binding ]); pstr_loc = loc } ] -> (
      match
        Ascetic_specification_ppx.Translate.structure_items ~loc binding.pvb_pat
          binding.pvb_expr
      with
      | [
       _;
       ({ pstr_desc = Pstr_extension (({ txt = "ocaml.error"; _ }, _), _); _ } as error);
      ] ->
          Pprintast.string_of_structure [ error ]
      | [ _; _ ] -> Alcotest.failf "a tree was made of: %s" source
      | _ -> Alcotest.fail "two items")
  | _ -> Alcotest.fail "one binding"

let contains text part =
  let n = String.length part in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = part || at (i + 1))
  in
  at 0

let what_cannot_be_a_tree_is_reported_where_it_stands () =
  List.iter
    (fun (source, message) ->
      let error = error_of source in
      Alcotest.(check bool) (source ^ ": " ^ error) true (contains error message))
    [
      ("let f (u : user) = u.a < Some 5L", "an option has no order");
      ("let f (u : user) = None > u.a", "an option has no order");
      ("let f (u : user) (at : int64 option) = u.a <= at", "an option has no order");
      ("let f (u : user) = u", "the candidate itself is not a value");
      ( "let f (u : user) = u.a > limit",
        "not a member of the candidate, an item, or a parameter" );
      ("let f (u : user) = u.a > Limits.dear", "not a member of the candidate");
      ("let f (u : user) age = u.a > age", "annotate the parameter");
      ( "let f (u : user) (m : money) = u.a > m",
        "a constant of a specification is a parameter of type" );
      ( "let f (u : user) (limit : int64 option) = Option.fold ~none:false ~some:(fun \
         limit -> u.a > limit.amount) limit",
        "what a parameter holds" );
      ("let f (u : user) = Option.fold ~none:false ~some:check u.a", "takes what it holds");
      ( "let f (u : user) = Option.fold ~none:false ~some:(fun (x, y) -> x > y) u.a",
        "a plain one" );
      ("let f (u : user) = List.exists check u.items", "takes the item");
      ( "let f (u : user) = List.exists (fun i -> i.a > 1L) [ 1L ]",
        "a collection of a specification is a member" );
      ("let f (u : user) = if u.a then true else false", "not expressible");
      ("let f (u : user) = u.a lsr 1", "a logical shift");
      ("let f (u : user) = u.a == u.b", "physical equality");
      ("let f (u : user) = String.length u.name > 3", "has no meaning in a specification");
      ( "let f (u : user) = match u.a with None -> 1L | Some a -> a",
        "a match in a specification is on an option" );
      ("let f = true", "takes its candidate as the first parameter");
      ("let f (u : user) ~(age : int64) = u.a > age", "unlabelled");
    ]

(* The name of what is held is the nearest of that name, and what the candidate holds is
   in reach of the predicate of a collection. *)
let%specification shadowed (s : store) =
  Option.fold ~none:false ~some:(fun s -> s > 1L) s.closed_at
  && List.exists (fun (s : item) -> s.active) s.items

let%specification limited (s : store) =
  Option.fold ~none:false
    ~some:(fun limit -> List.exists (fun (i : item) -> i.price > limit) s.items)
    s.closed_at

let names_are_the_nearest () =
  let open Ast in
  Alcotest.check spec "shadowed"
    (and_
       (and_ (is_not_null (field "closed_at")) (gt (field "closed_at") (int64 1L)))
       (any "items" (item "active")))
    shadowed_ast;
  Alcotest.check spec "limited"
    (and_
       (is_not_null (field "closed_at"))
       (any "items" (gt (item "price") (field "closed_at"))))
    limited_ast;
  agree "shadowed" shadowed shadowed_ast;
  agree "limited" limited limited_ast

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "ppx"
    [
      ( "let%specification",
        [
          case "the tree of a predicate" the_tree_of_a_predicate;
          case "the function and its tree agree" the_function_and_its_tree_agree;
          case "the tree compiles to a query" the_tree_compiles_to_a_query;
          case "names are the nearest" names_are_the_nearest;
          case "what cannot be a tree is reported where it stands"
            what_cannot_be_a_tree_is_reported_where_it_stands;
        ] );
    ]
