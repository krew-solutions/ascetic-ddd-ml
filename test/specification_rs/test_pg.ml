(* The two readers against each other: whatever the evaluator says of a specification,
   PostgreSQL must say of its compiled text - the same value of a constant expression,
   the same error, the same rows selected. This is what the claims of [Value],
   [Evaluate] and [Pg] rest on: null logic, integer division, shifts, [IS], and
   parentheses that keep the shape of the tree. It needs a live database, named by
   [TEST_DATABASE_URL], and is skipped without one. *)

open Ascetic_specification
open Ast
module Sql_params = Ascetic_specification_caqti.Params
module E = Evaluate.Make (Value)

let int i = value (Value.of_int i)
let int64 i = value (Value.Int i)
let float f = value (Value.Float f)
let text s = value (Value.Text s)
let bool b = value (Value.Bool b)
let null = value Value.Null
let fail_caqti what error = Alcotest.failf "%s: %s" what (Caqti_error.show error)

let exec (module C : Caqti_eio.CONNECTION) sql =
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error error -> fail_caqti sql error

let exec_all conn statements = List.iter (exec conn) statements

(* The rows a query selects with its parameters, or the driver's error. *)
let collect (module C : Caqti_eio.CONNECTION) row sql params =
  match Sql_params.request row Caqti_mult.zero_or_more sql params with
  | Error Sql_params.Nul_in_text -> Alcotest.fail "a NUL in a parameter"
  | Ok (Sql_params.R (request, args)) -> C.collect_list request args

let find (module C : Caqti_eio.CONNECTION) row sql params =
  match Sql_params.request row Caqti_mult.one sql params with
  | Error Sql_params.Nul_in_text -> Alcotest.fail "a NUL in a parameter"
  | Ok (Sql_params.R (request, args)) -> C.find request args

let ids conn sql params =
  match collect conn Caqti_type.int64 sql params with
  | Ok ids -> ids
  | Error error -> fail_caqti sql error

let sqlstate_of : Caqti_error.t -> string option = function
  | `Request_failed { msg = Caqti_driver_postgresql.Result_error_msg { sqlstate; _ }; _ }
  | `Response_failed { msg = Caqti_driver_postgresql.Result_error_msg { sqlstate; _ }; _ }
    ->
      Some sqlstate
  | _ -> None

let with_connection env uri f () =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  match Caqti_eio_unix.connect ~sw ~stdenv uri with
  | Ok conn -> f conn
  | Error error -> Alcotest.failf "connect: %s" (Caqti_error.show error)

let compiled ?schema specification =
  match Pg.compile ?schema specification with
  | Ok query -> query
  | Error e -> Alcotest.fail (Pg.error_to_string e)

let transformed mapping specification =
  match Mapping.transform mapping specification with
  | Ok tree -> tree
  | Error e -> Alcotest.fail (Mapping.error_to_string Fun.id e)

let satisfied evaluate specification rows =
  List.filter_map
    (fun (id, row) ->
      match evaluate specification row with
      | Ok true -> Some id
      | Ok false -> None
      | Error error -> Alcotest.failf "evaluated: %s" (Evaluate.error_to_string error))
    rows

let int64s = Alcotest.(list int64)

(* A mapping that renames every name of a path by one rule, and leaves the values: the
   storage's name of a member, whatever leads to it. *)
let renamed rename : (Value.t, Value.t, string) Mapping.t =
  {
    field =
      (fun path ->
        match List.map rename (Path.names path) with
        | [] -> Error "an empty path"
        | first :: rest ->
            Ok
              (Scalar
                 (Field
                    (List.fold_left Path.child (Path.make (Path.root path) first) rest))));
    value = (fun value -> Ok (Scalar (Value value)));
  }

(* ------------------------------------------------------------------------ *)
(* Constants                                                                 *)

let constants () =
  let t = bool true and f = bool false in
  let noon = Value.Timestamp.of_micros 1_700_000_000_000_000L in
  let hour = Value.Interval.of_micros 3_600_000_000L in
  let point t = value (Value.Timestamp t) and span i = value (Value.Interval i) in
  let written =
    [
      (* Arithmetic, and the parentheses that keep its shape. *)
      sub (int 10) (sub (int 4) (int 3));
      sub (sub (int 10) (int 4)) (int 3);
      sub (int 10) (add (int 4) (int 3));
      div (int 100) (div (int 10) (int 5));
      div (mul (int 7) (int 3)) (int 2);
      mul (add (int 1) (int 2)) (int 3);
      add (int 1) (mul (int 2) (int 3));
      div (int 7) (int 2);
      div (int (-7)) (int 2);
      modulo (int (-7)) (int 2);
      modulo (int 7) (int (-2));
      modulo (int64 Int64.min_int) (int (-1));
      neg (neg (int 5));
      sub (int 5) (neg (int 3));
      neg (add (int 1) (int 2));
      left_shift (int 1) (int 3);
      left_shift (int 1) (int 64);
      left_shift (int 1) (int (-1));
      right_shift (int 8) (int 65);
      right_shift (int (-8)) (int 1);
      left_shift (add (int 1) (int 2)) (int 3);
      add (int 1) (left_shift (int 2) (int 3));
      add (int 1) (float 0.5);
      div (float 7.0) (int 2);
      mul (float 2.5) (int 4);
      (* Where it fails. *)
      div (int 1) (int 0);
      modulo (int 1) (int 0);
      div (float 1.0) (float 0.0);
      add (int64 Int64.max_int) (int 1);
      mul (int64 Int64.max_int) (int 2);
      div (int64 Int64.min_int) (int (-1));
      neg (int64 Int64.min_int);
      mul (float Float.max_float) (float 2.0);
      (* What is not defined here is not defined there. *)
      add (text "a") (text "b");
      modulo (float 5.5) (int 2);
      neg (text "a");
      lt (int 1) (text "b");
      (* Comparisons. *)
      gt t f;
      le t t;
      eq (int 1) (float 1.0);
      lt (int 1) (float 1.5);
      ge (int 2) (int 2);
      le (int 3) (int 2);
      ne (text "a") (text "b");
      lt (text "a") (text "b");
      eq (float Float.nan) (float Float.nan);
      gt (float Float.nan) (float Float.max_float);
      eq (float (-0.0)) (float 0.0);
      eq (eq (int 1) (int 1)) t;
      eq t (eq (int 1) (int 2));
      eq (is_null null) t;
      (* Nulls. *)
      eq null (int 1);
      eq null null;
      ne (int 1) null;
      add (int 1) null;
      neg null;
      div null (int 0);
      not_ null;
      and_ null f;
      and_ f null;
      and_ null t;
      and_ null null;
      or_ null t;
      or_ t null;
      or_ null f;
      and_ (or_ t f) f;
      or_ t (and_ f f);
      and_ t (and_ t f);
      not_ (and_ t f);
      not_ (not_ t);
      is_null (or_ null f);
      is_null (is_null null);
      is_not_null (eq (int 1) null);
      is_null (eq (int 1) (int 1));
      not_ (is_null null);
      (* IS. *)
      is t t;
      is t f;
      is null null;
      is null t;
      is (int 1) null;
      is (int 1) (int 1);
      eq (is t null) f;
      is (eq (int 1) (int 1)) t;
      (* Time. *)
      sub (point (Value.Timestamp.of_micros 1_700_000_000_000_005L)) (point noon);
      add (point noon) (span hour);
      add (span hour) (point noon);
      sub (point noon) (span hour);
      add (span hour) (span hour);
      neg (span hour);
      lt (point noon) (add (point noon) (span hour));
      gt (span hour) (sub (span hour) (span hour));
    ]
  in
  (* Floats at their edges, every pair under every operator. What the server makes of
     each - a value, "out of range" for a result too large or too small to be one,
     "division by zero" - is the server's to say, and the evaluator's to repeat. *)
  let edges =
    [
      0.0;
      1.0;
      -1.0;
      1e300;
      1e-300;
      Float.max_float;
      Float.min_float;
      Float.infinity;
      Float.neg_infinity;
      Float.nan;
    ]
  in
  let at_the_edges =
    List.concat_map
      (fun left ->
        List.concat_map
          (fun right ->
            List.map (fun op -> op (float left) (float right)) [ add; sub; mul; div ])
          edges)
      edges
  in
  written @ at_the_edges

let a_constant_expression_has_one_value_for_both_readers conn =
  let nothing = Record.to_context (Record.object_ []) in
  List.iter
    (fun expr ->
      let query = compiled expr in
      let evaluated = E.evaluate expr nothing in
      (* The value the evaluator found goes in as one more parameter, of a type inferred
         from what it is compared with. *)
      let text =
        Printf.sprintf "SELECT (%s) IS NOT DISTINCT FROM $%d" query.sql
          (List.length query.params + 1)
      in
      let expected = match evaluated with Ok value -> value | Error _ -> Value.Null in
      let answered = find conn Caqti_type.bool text (query.params @ [ expected ]) in
      match (evaluated, answered) with
      | Ok value, Ok agreed ->
          Alcotest.(check bool)
            (Printf.sprintf "%s: evaluated to %s" query.sql (Value.show value))
            true agreed
      (* The same failure, not just a failure: by its SQLSTATE. *)
      | Error (Evaluate.Operand failure), Error error ->
          let expected =
            match failure with
            | Operand.Division_by_zero -> "22012"
            | Operand.Out_of_range -> "22003"
            | Operand.Unsupported _ -> "42883"
          in
          Alcotest.(check (option string))
            (Printf.sprintf "%s: %s" query.sql (Caqti_error.show error))
            (Some expected) (sqlstate_of error)
      | Ok value, Error error ->
          Alcotest.failf "%s: evaluated to %s, PostgreSQL: %s" query.sql
            (Value.show value) (Caqti_error.show error)
      | Error error, Ok agreed ->
          Alcotest.failf "%s: evaluated: %s, PostgreSQL: %b" query.sql
            (Evaluate.error_to_string error)
            agreed
      | Error error, Error _ ->
          Alcotest.failf "%s: evaluated: %s" query.sql (Evaluate.error_to_string error))
    (constants ())

(* ------------------------------------------------------------------------ *)
(* Rows                                                                      *)

type item = { price : int64 option; active : bool option }

type store = {
  id : int64;
  a : int64 option;
  b : int64 option;
  flag : bool option;
  name : string option;
  items : item list;
}

(* The name of the item's maker: a Value Object inside the item, which the storage keeps
   as a composite inside the item's row. *)
let maker_name item =
  Option.map
    (fun price -> if Int64.compare price 500L > 0 then "dear" else "cheap")
    item.price

(* An owner: an object of its own, which the storage keeps in a table of its own and the
   row refers to by a key. The third owner has no name. *)
let owner_of = function
  | Some true -> (1L, Some "ann")
  | Some false -> (2L, Some "bob")
  | None -> (3L, None)

let stores =
  let item price active = { price; active } in
  let store id a b flag name items = { id; a; b; flag; name; items } in
  [
    store 1L (Some 1L) (Some 1L) (Some true) (Some "one")
      [ item (Some 900L) (Some true); item (Some 10L) (Some false) ];
    store 2L (Some 1L) (Some 2L) (Some false) (Some "two") [ item (Some 10L) (Some true) ];
    store 3L None (Some 2L) None None [ item None (Some true); item (Some 10L) None ];
    store 4L None None (Some true) (Some "four") [];
    store 5L (Some 7L) None (Some false) (Some "five") [ item None None ];
    store 6L (Some (-3L)) (Some 0L) None (Some "")
      [ item (Some 900L) None; item (Some 901L) (Some true) ];
  ]

let opt_int = Value.of_option (fun v -> Value.Int v)
let opt_bool = Value.of_option Value.of_bool
let opt_text = Value.of_option Value.of_string

let record store =
  let items =
    List.map
      (fun item ->
        Record.(
          object_
            [
              ("price", value (opt_int item.price));
              ("active", value (opt_bool item.active));
              ("maker", object_ [ ("name", value (opt_text (maker_name item))) ]);
              ( "owner",
                object_ [ ("name", value (opt_text (snd (owner_of item.active)))) ] );
            ]))
      store.items
  in
  Record.(
    to_context
      (object_
         [
           ("id", value (Value.Int store.id));
           ("a", value (opt_int store.a));
           ("b", value (opt_int store.b));
           ("flag", value (opt_bool store.flag));
           ("name", value (opt_text store.name));
           ("items", collection items);
           ("owner", object_ [ ("name", value (opt_text (snd (owner_of store.flag)))) ]);
           (* Members named as PostgreSQL names other things, under columns of those very
              names: [user] is the session's user if it is not quoted, [order] does not
              parse, [createdAt] is folded to [createdat]. *)
           ("user", value (opt_text store.name));
           ("order", value (opt_int store.a));
           ("createdAt", value (opt_int store.b));
         ]))

let lit_int = function None -> "NULL::int8" | Some i -> Printf.sprintf "%Ld::int8" i
let lit_bool = function None -> "NULL::bool" | Some b -> Printf.sprintf "%b::bool" b
let lit_text = function None -> "NULL::text" | Some s -> Printf.sprintf "'%s'::text" s

let tables conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_maker AS (name text)";
      "CREATE TYPE pg_temp.spec_item AS (price int8, active bool, maker \
       pg_temp.spec_maker, owner_id int8)";
      "CREATE TEMP TABLE spec_owners (id int8 PRIMARY KEY, name text)";
      "INSERT INTO spec_owners VALUES (1, 'ann'), (2, 'bob'), (3, NULL)";
      "CREATE TEMP TABLE spec_stores (id int8 PRIMARY KEY, a int8, b int8, flag bool, \
       name text, items pg_temp.spec_item[] NOT NULL, \"user\" text, \"order\" int8, \
       \"createdAt\" int8, owner_id int8)";
      "CREATE TEMP TABLE spec_items (store_id int8 NOT NULL, price int8, active bool, \
       maker pg_temp.spec_maker, owner_id int8 REFERENCES spec_owners)";
    ];
  List.iter
    (fun store ->
      exec conn
        (Printf.sprintf
           "INSERT INTO spec_stores VALUES (%Ld, %s, %s, %s, %s, '{}', %s, %s, %s, %Ld)"
           store.id (lit_int store.a) (lit_int store.b) (lit_bool store.flag)
           (lit_text store.name) (lit_text store.name) (lit_int store.a) (lit_int store.b)
           (fst (owner_of store.flag)));
      List.iter
        (fun item ->
          let row =
            Printf.sprintf "%s, %s, ROW(%s)::pg_temp.spec_maker, %Ld::int8"
              (lit_int item.price) (lit_bool item.active)
              (lit_text (maker_name item))
              (fst (owner_of item.active))
          in
          exec conn
            (Printf.sprintf
               "UPDATE spec_stores SET items = items || ROW(%s)::pg_temp.spec_item WHERE \
                id = %Ld"
               row store.id);
          exec conn
            (Printf.sprintf "INSERT INTO spec_items VALUES (%Ld, %s)" store.id row))
        store.items)
    stores

let bound source params =
  match Jsonpath.Template.bind (Jsonpath.Template.parse_exn source) params with
  | Ok tree -> tree
  | Error error -> Alcotest.failf "%s: %s" source (Jsonpath.Bind_error.to_string error)

let specifications () =
  let dear () = gt (item "price") (int 500) in
  let maker_name () = field_at (Path.child (Path.item "maker") "name") in
  let owner_name () = field_at (Path.child (Path.item "owner") "name") in
  let positional values = Jsonpath.Params.positional values in
  [
    eq (field "a") (field "b");
    not_ (eq (field "a") (field "b"));
    ne (field "a") (field "b");
    is (field "a") (field "b");
    not_ (is (field "a") (field "b"));
    is_null (field "a");
    and_ (is_not_null (field "a")) (is_null (field "b"));
    or_ (eq (field "a") (field "b")) (field "flag");
    and_ (not_ (field "flag")) (gt (field "b") (int 1));
    not_ (or_ (field "flag") (is_null (field "name")));
    gt (sub (field "a") (sub (field "b") (int 1))) (int 0);
    lt (mul (add (field "a") (int 1)) (int 2)) (int 5);
    eq (is_null (field "a")) (field "flag");
    eq (field "name") (text "");
    lt (field "name") (text "one");
    is (field "flag") null;
    any "items" (dear ());
    not_ (any "items" (dear ()));
    any "items" (or_ (dear ()) (item "active"));
    any "items" (and_ (dear ()) (item "active"));
    any "items" (not_ (item "active"));
    any "items" (is_null (item "price"));
    any "items" (gt (item "price") (field "a"));
    all "items" (item "active");
    all "items" (gt (item "price") (int 5));
    not_ (all "items" (is_not_null (item "price")));
    and_ (field "flag") (any "items" (dear ()));
    (* A member of a Value Object inside the item: a composite inside the item's row, in
       the array and in the table alike. *)
    any "items" (eq (maker_name ()) (text "dear"));
    any "items" (and_ (is_null (maker_name ())) (item "active"));
    all "items" (ne (maker_name ()) (text "cheap"));
    bound "$.items[*][?@.maker.name == %s && @.price > 5]"
      (positional [ Value.Text "cheap" ]);
    (* A member of an object the item refers to by a key: the schema says [items.owner]
       is kept in a table of its own. *)
    any "items" (eq (owner_name ()) (text "ann"));
    any "items" (and_ (is_null (owner_name ())) (dear ()));
    all "items" (ne (owner_name ()) (text "bob"));
    any "items" (eq (owner_name ()) (maker_name ()));
    (* The same of the candidate itself, and both in one predicate. *)
    eq (field "owner.name") (text "bob");
    and_ (is_null (field "owner.name")) (is_not_null (field "a"));
    any "items" (eq (owner_name ()) (field "owner.name"));
    bound "$.items[*][?@.owner.name == %s && @.price > 5]"
      (positional [ Value.Text "bob" ]);
    (* Constants with nothing but constants beside them: their types are said in the
       text, for the server has nothing to find them by. *)
    gt (field "a") (sub (int 4) (int 3));
    any "items" (gt (item "price") (mul (int 100) (int 5)));
    lt (field "a") (neg (int (-2)));
    or_ (is_null null) (field "flag");
    (* A name is the column's, whatever else PostgreSQL knows by it. *)
    eq (field "user") (text "one");
    gt (field "order") (int 0);
    eq (field "createdAt") (int 2);
    bound "$[?@.user == %s]" (positional [ Value.Text "two" ]);
    (* A null found the way a template finds it, spelled out and bound. *)
    bound "$[?@.a == null]" Jsonpath.Params.none;
    bound "$[?@.b != null && @.a == %s]" (positional [ Value.Null ]);
    bound "$[?@.a == %d || @.name == %s]" (positional [ Value.Int 1L; Value.Null ]);
    bound "$.items[*][?@.price == %s]" (positional [ Value.Null ]);
    bound "$.items[*][?@.active != null && @.price > 500]" Jsonpath.Params.none;
  ]

let a_specification_selects_the_rows_it_is_satisfied_by conn =
  tables conn;
  (* The schema is the storage's keys; the tree reaches the compiler in the storage's
     names, which a mapping gives it: the items are a table of their own in one storage
     and an array in the other, and the owner is named by the key's column in both. *)
  let relational =
    Pg.Schema.(
      make "spec_stores"
      |> foreign_key "spec_items" "store_id" "spec_stores" "id"
      |> foreign_key "spec_items" "owner_id" "spec_owners" "id"
      |> foreign_key "spec_stores" "owner_id" "spec_owners" "id")
  in
  let embedded =
    Pg.Schema.(
      make "spec_stores"
      |> foreign_key "spec_stores.items" "owner_id" "spec_owners" "id"
      |> foreign_key "spec_stores" "owner_id" "spec_owners" "id")
  in
  let in_a_table =
    renamed (function "items" -> "spec_items" | "owner" -> "owner_id" | name -> name)
  in
  let in_the_row = renamed (function "owner" -> "owner_id" | name -> name) in
  let rows = List.map (fun store -> (store.id, record store)) stores in
  List.iter
    (fun specification ->
      let satisfied = satisfied E.is_satisfied_by specification rows in
      List.iter
        (fun (storage, schema, mapping) ->
          let query = compiled ~schema (transformed mapping specification) in
          let text =
            Printf.sprintf "SELECT id FROM spec_stores WHERE %s ORDER BY id" query.sql
          in
          Alcotest.check int64s
            (Printf.sprintf "%s: %s" storage text)
            satisfied (ids conn text query.params))
        [ ("embedded", embedded, in_the_row); ("relational", relational, in_a_table) ])
    (specifications ())

(* ------------------------------------------------------------------------ *)
(* Value Objects                                                             *)

(* The values of a domain whose items may have a discount. A discount is a Value Object,
   and a specification compares it as one: [@.discount > Discount 10], not a number found
   inside it. An item that has no discount has the special case of one - not a null in a
   discount's place, which a specification would have to step around. *)
module Priced = struct
  type t = Scalar of Value.t | Discount of int64 | No_discount [@@deriving show, eq]

  let null = Scalar Value.Null

  (* The special case is what is not known, to a comparison: as the column it is kept in
     is null. *)
  let is_null = function
    | Scalar value -> Value.is_null value
    | Discount _ -> false
    | No_discount -> true

  let of_bool value = Scalar (Value.of_bool value)
  let to_bool = function Scalar value -> Value.to_bool value | _ -> None

  let kind = function
    | Scalar value -> Value.kind value
    | Discount _ | No_discount -> "discount"

  let compare left right =
    match (left, right) with
    | Scalar left, Scalar right -> Value.compare left right
    | Discount left, Discount right -> Ok (Int64.compare left right)
    | _ -> Error (Operand.unsupported "<" (kind left) (kind right))

  let equals left right = Result.map (fun order -> order = 0) (compare left right)

  let negate = function
    | Scalar value -> Result.map (fun value -> Scalar value) (Value.negate value)
    | value -> Error (Operand.unsupported_unary "-" (kind value))

  let compute op left right =
    match (left, right) with
    | Scalar left, Scalar right ->
        Result.map (fun value -> Scalar value) (Value.compute op left right)
    | _ ->
        Error
          (Operand.unsupported
             (Operator.arithmetic_to_string op)
             (kind left) (kind right))
end

module Priced_evaluate = Evaluate.Make (Priced)

(* What the storage has for them: a discount is its percent in a column, and the special
   case is that column's null. *)
let prices : (Priced.t, Value.t, string) Mapping.t =
  {
    field =
      (fun path ->
        (* By the whole path from the candidate: the collection, and a member of its item
           under it. Where the item is, is the tree's. *)
        match Path.names path with
        | [ "items" ] -> Ok (Scalar (Field path))
        | [ "items"; "discount" ] ->
            Ok (Scalar (Field (Path.sibling path "discount_percent")))
        | names -> Error ("no such member: " ^ String.concat "." names));
    value =
      (fun value ->
        Ok
          (Scalar
             (Value
                (match value with
                | Priced.Scalar value -> value
                | Discount percent -> Value.Int percent
                | No_discount -> Value.Null))));
  }

(* A Value Object is compared as a whole by the evaluator, and what is not there is a
   special case of it: no path into it, so no member of a null to ask for, and the answer
   does not hang on the order of the items. The mapping says what it is in the storage,
   and the two readers agree. *)
let a_value_object_is_compared_as_a_whole_and_its_absence_is_a_special_case conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_priced AS (price int8, discount_percent int8)";
      "CREATE TEMP TABLE spec_shops (id int8, items pg_temp.spec_priced[])";
      "INSERT INTO spec_shops VALUES (1, ARRAY[ROW(900, 15), ROW(100, \
       NULL)]::pg_temp.spec_priced[]), (2, ARRAY[ROW(100, NULL), ROW(900, \
       15)]::pg_temp.spec_priced[]), (3, ARRAY[ROW(100, NULL)]::pg_temp.spec_priced[])";
    ];
  let priced price discount =
    Record.(
      object_
        [
          ("price", value (Priced.Scalar (Value.Int price))); ("discount", value discount);
        ])
  in
  let discounted () = priced 900L (Priced.Discount 15L)
  and plain () = priced 100L Priced.No_discount in
  let shops =
    List.map
      (fun (id, items) ->
        (id, Record.(to_context (object_ [ ("items", collection items) ]))))
      [
        (1L, [ discounted (); plain () ]);
        (2L, [ plain (); discounted () ]);
        (3L, [ plain () ]);
      ]
  in
  let discount () = item "discount" in
  let over percent = gt (discount ()) (value (Priced.Discount percent)) in
  List.iter
    (fun (specification, expected) ->
      let satisfied = satisfied Priced_evaluate.is_satisfied_by specification shops in
      Alcotest.check int64s (Ast.show Priced.pp specification) expected satisfied;
      let query = compiled (transformed prices specification) in
      let text =
        Printf.sprintf "SELECT id FROM spec_shops WHERE %s ORDER BY id" query.sql
      in
      Alcotest.check int64s text satisfied (ids conn text query.params))
    [
      (any "items" (over 10L), [ 1L; 2L ]);
      (any "items" (eq (discount ()) (value (Priced.Discount 15L))), [ 1L; 2L ]);
      (any "items" (is_null (discount ())), [ 1L; 2L; 3L ]);
      (not_ (any "items" (over 10L)), [ 3L ]);
      (any "items" (over 20L), []);
    ]

(* The same discount with a special case that answers for itself, as Fowler's Special
   Case does: it is equal to itself and to no discount, and less than any. Nothing of it
   is null to the evaluator. *)
module Answering = struct
  type t = Priced.t [@@deriving show, eq]

  let null = Priced.null
  let is_null = function Priced.Scalar value -> Value.is_null value | _ -> false
  let of_bool = Priced.of_bool
  let to_bool = Priced.to_bool
  let kind = Priced.kind

  let compare left right =
    match (left, right) with
    | Priced.No_discount, Priced.No_discount -> Ok 0
    | No_discount, Discount _ -> Ok (-1)
    | Discount _, No_discount -> Ok 1
    | left, right -> Priced.compare left right

  let equals left right = Result.map (fun order -> order = 0) (compare left right)
  let negate = Priced.negate
  let compute = Priced.compute
end

module Answering_evaluate = Evaluate.Make (Answering)

(* The same storage: the special case is the column's null. That it is one the mapping
   says, which [transform] reads where the two are compared for equality. *)
let answering_prices : (Answering.t, Value.t, string) Mapping.t =
  {
    field = prices.field;
    value =
      (function Priced.No_discount -> Ok (Null Value.Null) | other -> prices.value other);
  }

(* A special case that answers for itself is equal to itself, and the storage has a null
   for it: [discount = $1] with a null is true of nothing, so the server found no shop
   where the evaluator found all three. Equality with what the mapping says is the
   storage's null is the null test.

   What stays the server's own: a null compared with a value is unknown to it, and so is
   the negation of that, where the special case answers false and true. A special case
   kept as a value, and not as a null, has none of this. *)
let equality_with_a_special_case_kept_as_a_null_is_the_null_test conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_answering AS (price int8, discount_percent int8)";
      "CREATE TEMP TABLE spec_answering_shops (id int8, items pg_temp.spec_answering[])";
      "INSERT INTO spec_answering_shops VALUES (1, ARRAY[ROW(900, 15), ROW(100, \
       NULL)]::pg_temp.spec_answering[]), (2, ARRAY[ROW(100, NULL), ROW(900, \
       15)]::pg_temp.spec_answering[]), (3, ARRAY[ROW(100, \
       NULL)]::pg_temp.spec_answering[])";
    ];
  let of_ discount = Record.(object_ [ ("discount", value discount) ]) in
  let shops =
    List.map
      (fun (id, items) ->
        (id, Record.(to_context (object_ [ ("items", collection items) ]))))
      [
        (1L, [ of_ (Priced.Discount 15L); of_ Priced.No_discount ]);
        (2L, [ of_ Priced.No_discount; of_ (Priced.Discount 15L) ]);
        (3L, [ of_ Priced.No_discount ]);
      ]
  in
  let discount () = item "discount" in
  let none () = value Priced.No_discount in
  let over percent = gt (discount ()) (value (Priced.Discount percent)) in
  (* The specification; the shops it is satisfied by; the shops the server selects. *)
  List.iter
    (fun (specification, in_memory, on_the_server) ->
      let satisfied = satisfied Answering_evaluate.is_satisfied_by specification shops in
      Alcotest.check int64s (Ast.show Answering.pp specification) in_memory satisfied;
      let query = compiled (transformed answering_prices specification) in
      let text =
        Printf.sprintf "SELECT id FROM spec_answering_shops WHERE %s ORDER BY id"
          query.sql
      in
      Alcotest.check int64s text on_the_server (ids conn text query.params))
    [
      (any "items" (eq (discount ()) (none ())), [ 1L; 2L; 3L ], [ 1L; 2L; 3L ]);
      (any "items" (eq (none ()) (discount ())), [ 1L; 2L; 3L ], [ 1L; 2L; 3L ]);
      (any "items" (ne (discount ()) (none ())), [ 1L; 2L ], [ 1L; 2L ]);
      (any "items" (over 10L), [ 1L; 2L ], [ 1L; 2L ]);
      (* The server's own logic of a null, which the null test does not reach. *)
      (any "items" (not_ (over 10L)), [ 1L; 2L; 3L ], []);
      (any "items" (ne (discount ()) (value (Priced.Discount 15L))), [ 1L; 2L; 3L ], []);
    ]

(* ------------------------------------------------------------------------ *)
(* Types                                                                     *)

(* Why a type is said only where nothing stands beside the constant. A point in time is
   written as a timestamp with zone or without, whichever the column is. Said to be
   [timestamptz] beside a column without zone, it would be compared in the session's
   time zone, and the row would not be found. *)
let a_constant_beside_a_column_takes_the_columns_type conn =
  exec_all conn
    [
      "SET TIME ZONE 'Asia/Tokyo'";
      "CREATE TEMP TABLE spec_moments (id int8, at timestamp, zoned timestamptz, small \
       int2)";
      "INSERT INTO spec_moments VALUES (1, '2023-11-14 22:13:20', '2023-11-14 \
       22:13:20+00', 7)";
    ];
  let noon = value (Value.Timestamp (Value.Timestamp.of_micros 1_700_000_000_000_000L)) in
  List.iter
    (fun specification ->
      let query = compiled specification in
      let text = Printf.sprintf "SELECT id FROM spec_moments WHERE %s" query.sql in
      Alcotest.check int64s text [ 1L ] (ids conn text query.params))
    [
      eq (field "at") noon;
      eq (field "zoned") noon;
      eq (field "small") (int 7);
      (* And where nothing stands beside them, the constants say their own. *)
      eq (field "small") (add (int 3) (int 4));
    ]

(* PostgreSQL shifts by an [integer] and by nothing else: a [bigint] column as the count
   was "operator does not exist: bigint << bigint". Both readers take the count modulo
   64, a negative one included. *)
let the_count_of_a_shift_is_an_integer_whatever_its_column_is conn =
  exec_all conn
    [
      "CREATE TEMP TABLE spec_shifts (id int8, n int8, count int8, small int2)";
      "INSERT INTO spec_shifts VALUES (1, 1, 3, 3), (2, 1, 64, 64), (3, 1, -1, -1), (4, \
       1, NULL, NULL), (5, 8, 62, NULL)";
    ];
  let row n count small =
    Record.(
      to_context
        (object_
           [
             ("n", value (Value.Int n));
             ("count", value (opt_int count));
             ("small", value (opt_int small));
           ]))
  in
  let rows =
    [
      (1L, row 1L (Some 3L) (Some 3L));
      (2L, row 1L (Some 64L) (Some 64L));
      (3L, row 1L (Some (-1L)) (Some (-1L)));
      (4L, row 1L None None);
      (5L, row 8L (Some 62L) None);
    ]
  in
  let n () = field "n" and count () = field "count" and small () = field "small" in
  List.iter
    (fun (specification, expected) ->
      let satisfied = satisfied E.is_satisfied_by specification rows in
      Alcotest.check int64s "evaluated" expected satisfied;
      let query = compiled specification in
      let text =
        Printf.sprintf "SELECT id FROM spec_shifts WHERE %s ORDER BY id" query.sql
      in
      Alcotest.check int64s text expected (ids conn text query.params))
    [
      (eq (left_shift (n ()) (count ())) (int 8), [ 1L ]);
      (* 64 is no shift at all, and -1 is one by 63. *)
      (eq (left_shift (n ()) (count ())) (int 1), [ 2L ]);
      (eq (left_shift (n ()) (count ())) (int64 Int64.min_int), [ 3L ]);
      (is_null (left_shift (n ()) (count ())), [ 4L ]);
      (is_null (left_shift (n ()) (small ())), [ 4L; 5L ]);
      (eq (left_shift (n ()) (small ())) (int 8), [ 1L ]);
      (* An expression as the count, and a constant shifted by a column. *)
      (eq (left_shift (n ()) (add (count ()) (int 1))) (int 16), [ 1L ]);
      (eq (right_shift (int 64) (count ())) (int 8), [ 1L ]);
    ]

(* A Value Object kept in the candidate's row as a composite column: ["address"."city"]
   was a table PostgreSQL does not have. Declared in the schema, the column is read as a
   composite, [("t"."address")."city"], whose member of a null is null. *)
let a_composite_column_of_the_candidate_is_a_member_where_the_schema_says conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_address AS (city text, zip int8)";
      "CREATE TEMP TABLE spec_addressed (id int8, address pg_temp.spec_address)";
      "INSERT INTO spec_addressed VALUES (1, ROW('Minsk', 220000)), (2, ROW('Riga', \
       NULL)), (3, ROW(NULL, 1000))";
    ];
  let row city zip =
    Record.(
      to_context
        (object_
           [
             ( "address",
               object_ [ ("city", value (opt_text city)); ("zip", value (opt_int zip)) ]
             );
           ]))
  in
  let rows =
    [
      (1L, row (Some "Minsk") (Some 220_000L));
      (2L, row (Some "Riga") None);
      (3L, row None (Some 1000L));
    ]
  in
  let city () = field "address.city" and zip () = field "address.zip" in
  let schema =
    Pg.Schema.(make "spec_addressed" |> alias "t" |> composite "spec_addressed" "address")
  in
  List.iter
    (fun (specification, expected) ->
      Alcotest.check int64s "evaluated" expected
        (satisfied E.is_satisfied_by specification rows);
      let query = compiled ~schema specification in
      let text =
        Printf.sprintf "SELECT id FROM spec_addressed t WHERE %s ORDER BY id" query.sql
      in
      Alcotest.check int64s text expected (ids conn text query.params))
    [
      (eq (city ()) (text "Minsk"), [ 1L ]);
      (ne (city ()) (text "Minsk"), [ 2L ]);
      (is_null (city ()), [ 3L ]);
      (and_ (is_not_null (zip ())) (gt (zip ()) (int 5000)), [ 1L ]);
    ];
  (* Undeclared, the same path is a table the query does not have. *)
  let query = compiled (eq (city ()) (text "Minsk")) in
  Alcotest.(check string) "undeclared" {|"address"."city" = $1|} query.sql;
  let text = Printf.sprintf "SELECT id FROM spec_addressed t WHERE %s" query.sql in
  match collect conn Caqti_type.int64 text query.params with
  | Ok _ -> Alcotest.fail "a table the query does not have"
  | Error error ->
      Alcotest.(check (option string)) text (Some "42P01") (sqlstate_of error)

(* An option of a Value Object kept as a composite column is [Some] or [None] whatever
   the members hold, and so it is to the evaluator; to [IS NOT NULL] of the column a row
   with a null member was neither null nor not. Declared a composite, the column is
   tested as a whole. A [None] is a null column in the storage and a null value in the
   record. *)
let a_null_test_of_a_declared_composite_is_of_the_value_as_a_whole conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_discount AS (percent int8, code text)";
      "CREATE TEMP TABLE spec_deals (id int8, discount pg_temp.spec_discount)";
      "INSERT INTO spec_deals VALUES (1, ROW(15, 'x')), (2, ROW(NULL, 'x')), (3, \
       ROW(NULL, NULL)), (4, NULL)";
    ];
  let some percent code =
    Record.(
      to_context
        (object_
           [
             ( "discount",
               object_
                 [ ("percent", value (opt_int percent)); ("code", value (opt_text code)) ]
             );
           ]))
  in
  let rows =
    [
      (1L, some (Some 15L) (Some "x"));
      (2L, some None (Some "x"));
      (3L, some None None);
      (4L, Record.(to_context (object_ [ ("discount", value Value.Null) ])));
    ]
  in
  let discount () = field "discount" and percent () = field "discount.percent" in
  let declared =
    Pg.Schema.(make "spec_deals" |> alias "d" |> composite "spec_deals" "discount")
  in
  let undeclared = Pg.Schema.(make "spec_deals" |> alias "d") in
  (* Declared, both readers agree; undeclared, the server tests the members and the
     evaluator is not asked. *)
  List.iter
    (fun (schema, specification, expected) ->
      if schema == declared then
        Alcotest.check int64s "evaluated" expected
          (satisfied E.is_satisfied_by specification rows);
      let query = compiled ~schema specification in
      let text =
        Printf.sprintf "SELECT id FROM spec_deals d WHERE %s ORDER BY id" query.sql
      in
      Alcotest.check int64s text expected (ids conn text query.params))
    [
      (declared, is_not_null (discount ()), [ 1L; 2L; 3L ]);
      (declared, is_null (discount ()), [ 4L ]);
      (declared, not_ (is_null (discount ())), [ 1L; 2L; 3L ]);
      (* The guards a frontend writes: "is some and", "is none or". *)
      (declared, and_ (is_not_null (discount ())) (gt (percent ()) (int 10)), [ 1L ]);
      (declared, or_ (is_null (discount ())) (gt (percent ()) (int 10)), [ 1L; 4L ]);
      (declared, and_ (is_not_null (discount ())) (is_null (percent ())), [ 2L; 3L ]);
      (* Undeclared, the test is of the members: a row with a null inside is neither
         null nor not. *)
      (undeclared, is_not_null (discount ()), [ 1L ]);
      (undeclared, is_null (discount ()), [ 3L; 4L ]);
    ]

(* Why the compiler refuses a text with a NUL in it: the server has no such text, and a
   C string would end at the NUL in silence, so the adapter refuses it too. In memory it
   is a string like any other. *)
let a_text_with_a_nul_is_no_text_of_the_server _conn =
  Alcotest.(check (result bool (testable Sql_params.pp_error Sql_params.equal_error)))
    "adapter" (Error Sql_params.Nul_in_text)
    (Result.map (fun _ -> true) (Sql_params.of_values [ Value.Text "a\000b" ]));
  let bound =
    bound "$[?@.name == %s]" (Jsonpath.Params.positional [ Value.Text "a\000b" ])
  in
  Alcotest.(check bool) "compiler" true (Result.is_error (Pg.compile bound));
  let row = Record.(to_context (object_ [ ("name", value (Value.Text "a\000b")) ])) in
  Alcotest.(check (result bool (testable Evaluate.pp_error Evaluate.equal_error)))
    "evaluator" (Ok true) (E.is_satisfied_by bound row)

(* A value goes to the server as a text of no declared type, and the server reads it as
   the type it inferred, if the text spells one. *)
let a_value_is_read_as_the_type_the_server_asks_for_if_it_fits conn =
  let narrow = "SELECT $1::int4 + $2::int2 + $3::float4" in
  (match
     find conn Caqti_type.float narrow [ Value.Int 40L; Value.Int 2L; Value.Int 1L ]
   with
  | Ok sum -> Alcotest.(check (float 0.0)) "the values fit" 43.0 sum
  | Error error -> fail_caqti narrow error);
  (* What does not fit is refused by the server, not truncated on the way. *)
  Alcotest.(check bool)
    "too wide" true
    (Result.is_error
       (find conn Caqti_type.float narrow
          [ Value.Int Int64.max_int; Value.Int 2L; Value.Int 1L ]));
  (* A text that spells no number is refused; one that does is read as the number, which
     the reference's typed driver refuses. *)
  Alcotest.(check bool)
    "no number" true
    (Result.is_error
       (find conn Caqti_type.float narrow
          [ Value.Text "forty"; Value.Int 2L; Value.Int 1L ]));
  (* The server infers the type of a parameter from the column it meets. *)
  let inferred = "SELECT $1 = 7::int4 AND $2 = 'x'::varchar AND $3 > 0.5::float8" in
  match
    find conn Caqti_type.bool inferred [ Value.Int 7L; Value.Text "x"; Value.Float 0.75 ]
  with
  | Ok agreed -> Alcotest.(check bool) "inferred" true agreed
  | Error error -> fail_caqti inferred error

(* The item of an enclosing collection, named from an inner predicate by how far out it
   is: the category's limit beside the price of its product. In either storage - arrays
   nested in a composite, or tables that point at one another - the enclosing item's row
   is in scope of the inner query. *)
let the_item_of_an_enclosing_collection_is_named_from_an_inner_predicate conn =
  exec_all conn
    [
      "CREATE TYPE pg_temp.spec_product AS (price int8)";
      "CREATE TYPE pg_temp.spec_category AS (\"limit\" int8, products \
       pg_temp.spec_product[])";
      "CREATE TEMP TABLE spec_shops (id int8 PRIMARY KEY, \"limit\" int8, categories \
       pg_temp.spec_category[])";
      "CREATE TEMP TABLE spec_categories (id int8 PRIMARY KEY, shop_id int8, \"limit\" \
       int8)";
      "CREATE TEMP TABLE spec_products (category_id int8, price int8)";
      "INSERT INTO spec_shops VALUES (1, 50, ARRAY[ROW(10, ARRAY[ROW(5), \
       ROW(20)]::pg_temp.spec_product[]), ROW(100, \
       ARRAY[ROW(30)]::pg_temp.spec_product[])]::pg_temp.spec_category[]), (2, 50, \
       ARRAY[ROW(100, ARRAY[ROW(30), \
       ROW(NULL)]::pg_temp.spec_product[])]::pg_temp.spec_category[]), (3, 5, \
       ARRAY[ROW(NULL, \
       ARRAY[ROW(30)]::pg_temp.spec_product[])]::pg_temp.spec_category[]), (4, 50, '{}')";
      "INSERT INTO spec_categories VALUES (11, 1, 10), (12, 1, 100), (21, 2, 100), (31, \
       3, NULL)";
      "INSERT INTO spec_products VALUES (11, 5), (11, 20), (12, 30), (21, 30), (21, \
       NULL), (31, 30)";
    ];
  let shop limit categories =
    Record.(
      to_context
        (object_
           [
             ("limit", value (opt_int limit));
             ( "categories",
               collection
                 (List.map
                    (fun (limit, prices) ->
                      object_
                        [
                          ("limit", value (opt_int limit));
                          ( "products",
                            collection
                              (List.map
                                 (fun price ->
                                   object_ [ ("price", value (opt_int price)) ])
                                 prices) );
                        ])
                    categories) );
           ]))
  in
  let shops =
    [
      ( 1L,
        shop (Some 50L) [ (Some 10L, [ Some 5L; Some 20L ]); (Some 100L, [ Some 30L ]) ]
      );
      (2L, shop (Some 50L) [ (Some 100L, [ Some 30L; None ]) ]);
      (3L, shop (Some 5L) [ (None, [ Some 30L ]) ]);
      (4L, shop (Some 50L) []);
    ]
  in
  let price () = item "price" and category_limit () = outer 1 "limit" in
  let over_its_category predicate =
    any "categories" (any_at (Path.item "products") predicate)
  in
  let specifications =
    [
      over_its_category (gt (price ()) (category_limit ()));
      over_its_category (lt (price ()) (category_limit ()));
      over_its_category (gt (price ()) (field "limit"));
      over_its_category
        (and_
           (gt (price ()) (category_limit ()))
           (lt (category_limit ()) (field "limit")));
      over_its_category (is_null (category_limit ()));
      not_ (over_its_category (not_ (gt (price ()) (category_limit ()))));
    ]
  in
  let relational =
    Pg.Schema.(
      make "spec_shops"
      |> foreign_key "spec_categories" "shop_id" "spec_shops" "id"
      |> foreign_key "spec_products" "category_id" "spec_categories" "id")
  in
  let embedded = Pg.Schema.make "spec_shops" in
  let in_tables =
    renamed (function
      | "categories" -> "spec_categories"
      | "products" -> "spec_products"
      | name -> name)
  in
  let in_the_row = renamed Fun.id in
  List.iter
    (fun specification ->
      let satisfied = satisfied E.is_satisfied_by specification shops in
      List.iter
        (fun (storage, schema, mapping) ->
          let query = compiled ~schema (transformed mapping specification) in
          let text =
            Printf.sprintf "SELECT id FROM spec_shops WHERE %s ORDER BY id" query.sql
          in
          Alcotest.check int64s
            (Printf.sprintf "%s: %s" storage text)
            satisfied (ids conn text query.params))
        [ ("embedded", embedded, in_the_row); ("relational", relational, in_tables) ])
    specifications

let cases env uri =
  let case name f = Alcotest.test_case name `Quick (with_connection env uri f) in
  [
    case "a constant expression has one value for both readers"
      a_constant_expression_has_one_value_for_both_readers;
    case "a specification selects the rows it is satisfied by"
      a_specification_selects_the_rows_it_is_satisfied_by;
    case "a Value Object is compared as a whole and its absence is a special case"
      a_value_object_is_compared_as_a_whole_and_its_absence_is_a_special_case;
    case "equality with a special case kept as a null is the null test"
      equality_with_a_special_case_kept_as_a_null_is_the_null_test;
    case "a constant beside a column takes the column's type"
      a_constant_beside_a_column_takes_the_columns_type;
    case "the count of a shift is an integer whatever its column is"
      the_count_of_a_shift_is_an_integer_whatever_its_column_is;
    case "a composite column of the candidate is a member where the schema says"
      a_composite_column_of_the_candidate_is_a_member_where_the_schema_says;
    case "a null test of a declared composite is of the value as a whole"
      a_null_test_of_a_declared_composite_is_of_the_value_as_a_whole;
    case "a text with a NUL is no text of the server"
      a_text_with_a_nul_is_no_text_of_the_server;
    case "a value is read as the type the server asks for if it fits"
      a_value_is_read_as_the_type_the_server_asks_for_if_it_fits;
    case "the item of an enclosing collection is named from an inner predicate"
      the_item_of_an_enclosing_collection_is_named_from_an_inner_predicate;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline
        "[skip] specification differential tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "specification_pg" [ ("against the server", cases env uri) ]
