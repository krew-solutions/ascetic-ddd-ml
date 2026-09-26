(* The text of compiled queries. The expected strings of the first tests are those of the
   Go [postgresql_visitor_test], [postgresql_wildcard_test], [schema_test] and
   [compile_test], names in Go's case as they are there and, as they are not there,
   between quotes; the later tests are where this compiler differs from the sources. *)

open Ascetic_specification
open Ast
module Schema = Pg.Schema
module Foreign_key = Pg.Foreign_key

let int i = value (Value.of_int i)
let text s = value (Value.Text s)
let bool b = value (Value.Bool b)
let null = value Value.Null
let query = Alcotest.testable (Pg.pp_query Value.pp) (Pg.equal_query Value.equal)
let compile_error = Alcotest.testable Pg.pp_error Pg.equal_error
let compiled = Alcotest.(result query compile_error)

let sql ?schema specification =
  match Pg.compile ?schema specification with
  | Ok query -> query.sql
  | Error e -> Alcotest.fail (Pg.error_to_string e)

let check_sql ?schema cases =
  List.iter
    (fun (specification, expected) ->
      Alcotest.(check string) expected expected (sql ?schema specification))
    cases

let check_sql_with cases =
  List.iter
    (fun (schema, specification, expected) ->
      Alcotest.(check string) expected expected (sql ~schema specification))
    cases

let fields_values_and_operators () =
  Alcotest.check compiled "age >= 18"
    (Ok { Pg.sql = {|"age" >= $1|}; params = [ Value.Int 18L ] })
    (Pg.compile (ge (field "age") (int 18)));
  check_sql
    [
      (field "users.name", {|"users"."name"|});
      (int 1, "$1");
      (ge (field "user.profile.age") (int 18), {|"user"."profile"."age" >= $1|});
      (is_null (field "deleted_at"), {|"deleted_at" IS NULL|});
      (is_not_null (field "created_at"), {|"created_at" IS NOT NULL|});
      (not_ (lt (field "age") (int 18)), {|NOT "age" < $1|});
      ( and_ (eq (field "active") (bool true)) (gt (field "age") (int 18)),
        {|"active" = $1 AND "age" > $2|} );
      (ne (field "a") (int 1), {|"a" != $1|});
      ( gt (sub (field "price") (field "discount")) (int 100),
        {|"price" - "discount" > $1|} );
      ( or_
          (and_ (eq (field "active") (bool true)) (ge (field "age") (int 18)))
          (eq (field "premium") (bool true)),
        {|"active" = $1 AND "age" >= $2 OR "premium" = $3|} );
    ]

let parameters_are_numbered_in_order_and_from_an_offset () =
  let specification = and_ (eq (field "a") (text "x")) (eq (field "b") (int 2)) in
  Alcotest.check compiled "offset"
    (Ok { Pg.sql = {|"a" = $3 AND "b" = $4|}; params = [ Value.Text "x"; Value.Int 2L ] })
    (Pg.compile ~offset:2 specification)

let an_embedded_collection_is_unnested () =
  let dear price = gt (item "Price") (int price) in
  check_sql
    [
      ( any "Items" (dear 500),
        {|EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" > $1)|}
      );
      ( any "Items" (item "Active"),
        {|EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Active")|} );
      ( any "Items" (and_all (dear 500) [ item "Active"; gt (item "Stock") (int 0) ]),
        {|EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" > $1 AND "item_1"."Active" AND "item_1"."Stock" > $2)|}
      );
      ( and_ (field "Active") (any "Items" (dear 500)),
        {|"Active" AND EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" > $1)|}
      );
      ( not_ (any "Items" (dear 500)),
        {|NOT EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" > $1)|}
      );
      ( any "Items" (gt (sub (item "Price") (int 100)) (int 400)),
        {|EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" - $1 > $2)|}
      );
      ( and_all (field "Active")
          [ any "Items" (dear 500); any "Items" (lt (item "Price") (int 100)) ],
        {|"Active" AND EXISTS (SELECT 1 FROM unnest("Items") AS "item_1" WHERE "item_1"."Price" > $1) AND EXISTS (SELECT 1 FROM unnest("Items") AS "item_2" WHERE "item_2"."Price" < $2)|}
      );
      ( any "Categories" (any_at (Path.item "Items") (dear 500)),
        {|EXISTS (SELECT 1 FROM unnest("Categories") AS "category_1" WHERE EXISTS (SELECT 1 FROM unnest("category_1"."Items") AS "item_2" WHERE "item_2"."Price" > $1))|}
      );
      ( any "Categories" (and_ (item "Active") (any_at (Path.item "Items") (dear 500))),
        {|EXISTS (SELECT 1 FROM unnest("Categories") AS "category_1" WHERE "category_1"."Active" AND EXISTS (SELECT 1 FROM unnest("category_1"."Items") AS "item_2" WHERE "item_2"."Price" > $1))|}
      );
      ( any "Regions"
          (any_at (Path.item "Categories") (any_at (Path.item "Items") (dear 500))),
        {|EXISTS (SELECT 1 FROM unnest("Regions") AS "region_1" WHERE EXISTS (SELECT 1 FROM unnest("region_1"."Categories") AS "category_2" WHERE EXISTS (SELECT 1 FROM unnest("category_2"."Items") AS "item_3" WHERE "item_3"."Price" > $1)))|}
      );
      ( any "Store.Items" (dear 500),
        {|EXISTS (SELECT 1 FROM unnest("Store"."Items") AS "item_1" WHERE "item_1"."Price" > $1)|}
      );
    ]

(* A member of a Value Object inside an item is a member of a composite kept in the
   item's row. With dots alone PostgreSQL reads a schema, a table and a column, and says
   there is no such table; the sources, besides, drop the item's alias from such a path
   and write ["maker"."name"], which is the column of another table if the query has one
   of that name. *)
let a_member_of_an_object_inside_an_item_is_a_member_of_a_composite () =
  let maker names =
    eq (field_at (List.fold_left Path.child (Path.item "maker") names)) (text "x")
  in
  let schema =
    Schema.(
      make "stores" |> alias "s" |> foreign_key "store_items" "store_id" "stores" "id")
  in
  check_sql
    [
      ( any "items" (maker [ "name" ]),
        {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE ("item_1"."maker")."name" = $1)|}
      );
      ( any "items" (maker [ "country"; "code" ]),
        {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE (("item_1"."maker")."country")."code" = $1)|}
      );
      (* The item of an inner collection, which is itself a member of the outer item. *)
      ( any "items" (any_at (Path.item "parts") (maker [ "name" ])),
        {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE EXISTS (SELECT 1 FROM unnest("item_1"."parts") AS "part_2" WHERE ("part_2"."maker")."name" = $1))|}
      );
    ];
  (* In a table of its own an item is a row as well, and its column a composite. *)
  Alcotest.(check string)
    "in a table"
    {|EXISTS (SELECT 1 FROM "store_items" AS "store_item_1" WHERE "store_item_1"."store_id" = "s"."id" AND ("store_item_1"."maker")."name" = $1)|}
    (sql ~schema (any "store_items" (maker [ "name" ])));
  (* From the candidate the dots stay: a qualified name, [alias.column]. *)
  Alcotest.(check string)
    "qualified" {|"s"."maker" = $1|}
    (sql (eq (field "s.maker") (text "x")))

(* An object on the way to a member is looked up in the schema, as a collection is. Kept
   in a table of its own it is read through its key, by a subquery in the column's place:
   at most the one row the key names, and null if there is none. *)
let a_member_of_an_object_kept_in_a_table_of_its_own_is_read_through_the_key () =
  let owner_name () = field_at (Path.child (Path.item "owner_id") "name") in
  let named name = eq (owner_name ()) (text name) in
  (* Whether the items are an array or a table, their owner is a table. A row of an array
     has no table: it is named by the array's column. *)
  let embedded =
    Schema.(
      make "stores" |> alias "s" |> foreign_key "stores.items" "owner_id" "owners" "id")
  in
  Alcotest.(check string)
    "embedded"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE (SELECT "owner_2"."name" FROM "owners" AS "owner_2" WHERE "owner_2"."id" = "item_1"."owner_id") = $1)|}
    (sql ~schema:embedded (any "items" (named "ann")));
  let relational =
    Schema.(
      make "stores" |> alias "s"
      |> foreign_key "store_items" "store_id" "stores" "id"
      |> foreign_key "store_items" "owner_id" "owners" "id")
  in
  Alcotest.(check string)
    "relational"
    {|EXISTS (SELECT 1 FROM "store_items" AS "store_item_1" WHERE "store_item_1"."store_id" = "s"."id" AND (SELECT "owner_2"."name" FROM "owners" AS "owner_2" WHERE "owner_2"."id" = "store_item_1"."owner_id") = $1)|}
    (sql ~schema:relational (any "store_items" (named "ann")));
  (* A key of two columns is named by either of them, unless another key has it too; and
     what is inside the owner's row is a composite. *)
  let composite_key =
    Schema.(
      make "stores" |> alias "s"
      |> key
           Foreign_key.(
             make "stores.items" "tenant_id" "public.owners" "tenant_id"
             |> and_ "owner_id" "id"))
  in
  let city = field_at (Path.child (Path.child (Path.item "owner_id") "address") "city") in
  Alcotest.(check string)
    "composite key"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE (SELECT ("owner_2"."address")."city" FROM "public"."owners" AS "owner_2" WHERE "owner_2"."tenant_id" = "item_1"."tenant_id" AND "owner_2"."id" = "item_1"."owner_id") IS NULL)|}
    (sql ~schema:composite_key (any "items" (is_null city)));
  (* Of the candidate itself, the key is the root row's; and each object read so has an
     alias of its own. *)
  let of_both =
    Schema.(
      make "stores" |> alias "s"
      |> foreign_key "stores" "owner_id" "owners" "id"
      |> foreign_key "stores.items" "owner_id" "owners" "id")
  in
  Alcotest.(check string)
    "of both"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE (SELECT "owner_2"."name" FROM "owners" AS "owner_2" WHERE "owner_2"."id" = "item_1"."owner_id") = (SELECT "owner_3"."name" FROM "owners" AS "owner_3" WHERE "owner_3"."id" = "s"."owner_id"))|}
    (sql ~schema:of_both (any "items" (eq (owner_name ()) (field "owner_id.name"))));
  (* The root row by its table, which carries its schema. *)
  let qualified =
    Schema.(make "public.stores" |> foreign_key "public.stores" "owner_id" "owners" "id")
  in
  Alcotest.(check string)
    "qualified"
    {|(SELECT "owner_1"."name" FROM "owners" AS "owner_1" WHERE "owner_1"."id" = "public"."stores"."owner_id") = $1|}
    (sql ~schema:qualified (eq (field "owner_id.name") (text "x")));
  (* What the schema does not mention stays what the dots have meant. *)
  Alcotest.(check string)
    "unmentioned" {|"s"."name" = $1|}
    (sql ~schema:of_both (eq (field "s.name") (text "x")));
  Alcotest.(check string)
    "composite"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE ("item_1"."maker")."name" = $1)|}
    (sql ~schema:of_both
       (any "items" (eq (field_at (Path.child (Path.item "maker") "name")) (text "x"))))

(* From the candidate a path of two names is a qualified name, ["s"."price"]: an object
   under the root is a table's alias. So a Value Object kept in the candidate's row as a
   composite column could not be reached: ["address"."city"] is a table PostgreSQL does
   not have. The schema says which columns are composites, as it says which are keys, and
   a path through one is a member of it; an undeclared name stays a qualifier. *)
let a_composite_column_of_the_candidate_is_declared () =
  let city () = field "address.city" in
  let schema = Schema.(make "stores" |> alias "s" |> composite "stores" "address") in
  Alcotest.(check string)
    "declared" {|("s"."address")."city" = $1|}
    (sql ~schema (eq (city ()) (text "x")));
  Alcotest.(check string)
    "nested" {|(("s"."address")."country")."code" IS NULL|}
    (sql ~schema (is_null (field "address.country.code")));
  (* Inside a collection's predicate the candidate's, beside the item's. *)
  Alcotest.(check string)
    "inside"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."city" = ("s"."address")."city")|}
    (sql ~schema (any "items" (eq (item "city") (city ()))));
  (* An undeclared name stays a qualifier; without an alias, the table's. *)
  Alcotest.(check string)
    "undeclared" {|"owner"."name" = $1|}
    (sql ~schema (eq (field "owner.name") (text "x")));
  Alcotest.(check string)
    "no alias" {|("stores"."address")."city" = $1|}
    (sql
       ~schema:Schema.(make "stores" |> composite "stores" "address")
       (eq (city ()) (text "x")));
  (* Without a schema there is no row to read a composite of; and a composite column of
     another table is not the candidate's. *)
  Alcotest.(check string)
    "no schema" {|"address"."city" = $1|}
    (sql (eq (city ()) (text "x")));
  Alcotest.(check string)
    "another table" {|"address"."city" = $1|}
    (sql
       ~schema:Schema.(make "stores" |> composite "items" "address")
       (eq (city ()) (text "x")))

(* Of a composite [IS NULL] is true when all its members are null and [IS NOT NULL] when
   none is, so a row with a null member is neither: the SQL standard's null predicate over
   a row value, which PostgreSQL follows. An option of a Value Object is [Some] or [None]
   whatever its members hold; [IS NOT NULL] of the column said otherwise of a [Some] with
   a null inside. A null test of a column the schema declares a composite is of the value
   as a whole, [IS DISTINCT FROM NULL], as the manual advises; a [None] is a null column,
   not a row of nulls. *)
let a_null_test_of_a_declared_composite_is_of_the_value_as_a_whole () =
  let schema = Schema.(make "stores" |> alias "s" |> composite "stores" "discount") in
  let discount () = field "discount" and percent () = field "discount.percent" in
  check_sql ~schema
    [
      (is_not_null (discount ()), {|"discount" IS DISTINCT FROM NULL|});
      (is_null (discount ()), {|"discount" IS NOT DISTINCT FROM NULL|});
      (* The guard a frontend writes, and its negation. *)
      ( and_ (is_not_null (discount ())) (gt (percent ()) (int 10)),
        {|"discount" IS DISTINCT FROM NULL AND ("s"."discount")."percent" > $1|} );
      (not_ (is_null (discount ())), {|NOT "discount" IS NOT DISTINCT FROM NULL|});
      (* Inside a collection's predicate, qualified as the candidate's columns are. *)
      ( any "items" (is_null (discount ())),
        {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "s"."discount" IS NOT DISTINCT FROM NULL)|}
      );
      (* What is not declared is tested as it was: another column, a scalar member of the
         composite. *)
      (is_null (field "price"), {|"price" IS NULL|});
      (is_null (percent ()), {|("s"."discount")."percent" IS NULL|});
    ];
  (* A row of the items array is named by the array's column, as it is to a key; a row of
     a table by the table; a composite inside a composite by the column. *)
  Alcotest.(check string)
    "array row"
    {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."maker" IS DISTINCT FROM NULL)|}
    (sql
       ~schema:Schema.(make "stores" |> composite "stores.items" "maker")
       (any "items" (is_not_null (item "maker"))));
  Alcotest.(check string)
    "table row"
    {|EXISTS (SELECT 1 FROM "store_items" AS "store_item_1" WHERE "store_item_1"."store_id" = "s"."id" AND "store_item_1"."maker" IS DISTINCT FROM NULL)|}
    (sql
       ~schema:
         Schema.(
           make "stores" |> alias "s"
           |> foreign_key "store_items" "store_id" "stores" "id"
           |> composite "store_items" "maker")
       (any "store_items" (is_not_null (item "maker"))));
  Alcotest.(check string)
    "nested composite" {|("s"."discount")."country" IS NOT DISTINCT FROM NULL|}
    (sql
       ~schema:
         Schema.(
           make "stores" |> alias "s" |> composite "stores" "discount"
           |> composite "stores.discount" "country")
       (is_null (field "discount.country")));
  (* Without a schema, and with the composite declared on another table. *)
  Alcotest.(check string) "no schema" {|"discount" IS NULL|} (sql (is_null (discount ())));
  Alcotest.(check string)
    "another table" {|"discount" IS NULL|}
    (sql
       ~schema:Schema.(make "stores" |> composite "items" "discount")
       (is_null (discount ())))

let a_relational_collection_is_joined_by_its_keys () =
  let stores () = Schema.(make "stores" |> alias "s") in
  (* The tree names a collection by its table. *)
  let dear () = any "items" (gt (item "Price") (int 500)) in
  check_sql_with
    [
      ( Schema.foreign_key "items" "store_id" "stores" "id" (stores ()),
        dear (),
        {|EXISTS (SELECT 1 FROM "items" AS "item_1" WHERE "item_1"."store_id" = "s"."id" AND "item_1"."Price" > $1)|}
      );
      ( Schema.key
          Foreign_key.(
            make "items" "tenant_id" "stores" "tenant_id" |> and_ "store_id" "id")
          (stores ()),
        dear (),
        {|EXISTS (SELECT 1 FROM "items" AS "item_1" WHERE "item_1"."tenant_id" = "s"."tenant_id" AND "item_1"."store_id" = "s"."id" AND "item_1"."Price" > $1)|}
      );
      (* Without an alias the root row goes by its table; and a table may carry its
         schema, in the tree as in the key. *)
      ( Schema.(make "stores" |> foreign_key "public.items" "store_id" "stores" "id"),
        any "public.items" (gt (item "Price") (int 500)),
        {|EXISTS (SELECT 1 FROM "public"."items" AS "item_1" WHERE "item_1"."store_id" = "stores"."id" AND "item_1"."Price" > $1)|}
      );
      ( Schema.(
          make "public.stores"
          |> foreign_key "public.items" "store_id" "public.stores" "id"),
        any "public.items" (gt (item "Price") (int 500)),
        {|EXISTS (SELECT 1 FROM "public"."items" AS "item_1" WHERE "item_1"."store_id" = "public"."stores"."id" AND "item_1"."Price" > $1)|}
      );
      (* A name that is no key's, and no table's with a key to the row, is an array in
         the row. *)
      ( Schema.foreign_key "orders" "store_id" "stores" "id" (stores ()),
        dear (),
        {|EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."Price" > $1)|}
      );
      (* A collection inside a collection joins to the row it is inside of: the key of
         its table that references that row's table. *)
      ( Schema.(
          stores ()
          |> foreign_key "categories" "store_id" "stores" "id"
          |> foreign_key "items" "category_id" "categories" "id"),
        any "categories" (any_at (Path.item "items") (gt (item "Price") (int 500))),
        {|EXISTS (SELECT 1 FROM "categories" AS "category_1" WHERE "category_1"."store_id" = "s"."id" AND EXISTS (SELECT 1 FROM "items" AS "item_2" WHERE "item_2"."category_id" = "category_1"."id" AND "item_2"."Price" > $1))|}
      );
    ]

let two_collections_of_one_name_are_two_collections () =
  let schema =
    Schema.(
      make "stores" |> alias "s"
      |> foreign_key "store_items" "store_id" "stores" "id"
      |> foreign_key "categories" "store_id" "stores" "id")
  in
  Alcotest.(check string)
    "of store"
    {|EXISTS (SELECT 1 FROM "store_items" AS "store_item_1" WHERE "store_item_1"."store_id" = "s"."id" AND "store_item_1"."Active")|}
    (sql ~schema (any "store_items" (item "Active")));
  (* No key of a table [items] references categories: an array in the row. *)
  Alcotest.(check string)
    "of category"
    {|EXISTS (SELECT 1 FROM "categories" AS "category_1" WHERE "category_1"."store_id" = "s"."id" AND EXISTS (SELECT 1 FROM unnest("category_1"."items") AS "item_2" WHERE "item_2"."Active"))|}
    (sql ~schema (any "categories" (any_at (Path.item "items") (item "Active"))))

let a_collection_of_the_candidate_inside_another_joins_to_the_root () =
  let schema =
    Schema.(
      make "stores" |> alias "s"
      |> foreign_key "items" "store_id" "stores" "id"
      |> foreign_key "tags" "store_id" "stores" "id")
  in
  Alcotest.(check string)
    "root"
    {|EXISTS (SELECT 1 FROM "items" AS "item_1" WHERE "item_1"."store_id" = "s"."id" AND EXISTS (SELECT 1 FROM "tags" AS "tag_2" WHERE "tag_2"."store_id" = "s"."id" AND "tag_2"."Name" = $1))|}
    (sql ~schema (any "items" (any "tags" (eq (item "Name") (text "sale")))))

let the_predicate_of_a_relational_collection_stays_inside_its_keys () =
  let schema =
    Schema.(make "stores" |> alias "s" |> foreign_key "items" "store_id" "stores" "id")
  in
  Alcotest.(check string)
    "parenthesised"
    {|EXISTS (SELECT 1 FROM "items" AS "item_1" WHERE "item_1"."store_id" = "s"."id" AND ("item_1"."Active" OR "item_1"."Price" > $1))|}
    (sql ~schema (any "items" (or_ (item "Active") (gt (item "Price") (int 500)))))

let parentheses_keep_the_shape_of_the_tree () =
  let a () = field "a" and b () = field "b" and c () = field "c" in
  check_sql
    [
      (* Looser inside tighter. *)
      (and_ (or_ (a ()) (b ())) (c ()), {|("a" OR "b") AND "c"|});
      (or_ (and_ (a ()) (b ())) (c ()), {|"a" AND "b" OR "c"|});
      (mul (add (a ()) (b ())) (c ()), {|("a" + "b") * "c"|});
      (add (mul (a ()) (b ())) (c ()), {|"a" * "b" + "c"|});
      (not_ (and_ (a ()) (b ())), {|NOT ("a" AND "b")|});
      (not_ (eq (a ()) (b ())), {|NOT "a" = "b"|});
      (is_null (or_ (a ()) (b ())), {|("a" OR "b") IS NULL|});
      (neg (add (a ()) (b ())), {|-("a" + "b")|});
      (left_shift (add (a ()) (b ())) (c ()), {|"a" + "b" << "c"::integer|});
      (add (a ()) (left_shift (b ()) (c ())), {|"a" + ("b" << "c"::integer)|});
      (* As tight: by the side the operator groups to. *)
      (sub (sub (a ()) (b ())) (c ()), {|"a" - "b" - "c"|});
      (sub (a ()) (sub (b ()) (c ())), {|"a" - ("b" - "c")|});
      (sub (a ()) (add (b ()) (c ())), {|"a" - ("b" + "c")|});
      (div (a ()) (div (b ()) (c ())), {|"a" / ("b" / "c")|});
      (div (mul (a ()) (b ())) (c ()), {|"a" * "b" / "c"|});
      (* A comparison groups to neither side. *)
      (eq (eq (a ()) (b ())) (c ()), {|("a" = "b") = "c"|});
      (eq (a ()) (eq (b ()) (c ())), {|"a" = ("b" = "c")|});
      (eq (is_null (a ())) (c ()), {|("a" IS NULL) = "c"|});
      (is_null (is_null (a ())), {|("a" IS NULL) IS NULL|});
      (is_null (eq (a ()) (b ())), {|"a" = "b" IS NULL|});
      (* The connectives regroup freely. *)
      (and_ (a ()) (and_ (b ()) (c ())), {|"a" AND "b" AND "c"|});
      (or_ (a ()) (or_ (b ()) (c ())), {|"a" OR "b" OR "c"|});
      (* Two minus signs are a comment. *)
      (neg (neg (a ())), {|-(-"a")|});
      (sub (a ()) (neg (b ())), {|"a" - -"b"|});
      (not_ (not_ (a ())), {|NOT NOT "a"|});
      ( all "items" (item "active"),
        {|NOT EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE NOT "item_1"."active")|}
      );
    ]

let is_takes_a_parameter () =
  Alcotest.(check string)
    "is" {|"active" IS NOT DISTINCT FROM $1|}
    (sql (is (field "active") (bool true)));
  Alcotest.(check string)
    "is under =" {|("a" IS NOT DISTINCT FROM "b") = $1|}
    (sql (eq (is (field "a") (field "b")) (bool true)))

(* A constant is a parameter, and the server finds its type from what stands beside it.
   Where every operand of an operator is a constant there is nothing beside it -
   "operator is not unique: unknown + unknown" - so there the text says the type, by the
   kind of the value. Beside a column it does not: the value adapts to the column, which
   a type said would take away. PostgreSQL shifts by an [integer] and by nothing else:
   [bigint << bigint] is "operator does not exist", and a column is a [bigint] more often
   than not. A constant as the count is inferred by the server from the operator, and
   where nothing stands beside it was said an integer already; a column or an expression
   as the count has a type of its own, which the server will not convert, so it is cast.
   A cast binds tighter than any operator, so what is not an atom is parenthesised. *)
let the_count_of_a_shift_is_an_integer () =
  let a () = field "a" and b () = field "b" and c () = field "c" in
  check_sql
    [
      (* A column or an expression is cast. *)
      (left_shift (a ()) (b ()), {|"a" << "b"::integer|});
      (right_shift (a ()) (add (b ()) (int 1)), {|"a" >> ("b" + $1)::integer|});
      ( left_shift (a ()) (left_shift (b ()) (c ())),
        {|"a" << ("b" << "c"::integer)::integer|} );
      (left_shift (a ()) (neg (b ())), {|"a" << (-"b")::integer|});
      (left_shift (add (a ()) (b ())) (c ()), {|"a" + "b" << "c"::integer|});
      (* A constant is inferred, as it was. *)
      (left_shift (a ()) (int 3), {|"a" << $1|});
      (left_shift (int 1) (int 4), "$1::bigint << $2::integer");
      (right_shift (int 64) (b ()), {|$1 >> "b"::integer|});
    ]

let a_constant_with_nothing_beside_it_has_its_type_said () =
  check_sql
    [
      (* Beside a column, or beside what has a type already: as it was. *)
      (gt (field "price") (int 1), {|"price" > $1|});
      (gt (add (field "price") (int 1)) (int 2), {|"price" + $1 > $2|});
      (* Both operands constants. *)
      (gt (field "price") (add (int 1) (int 2)), {|"price" > $1::bigint + $2::bigint|});
      (lt (int 1) (value (Value.Float 2.5)), "$1::bigint < $2::double precision");
      (eq (text "a") (text "b"), "$1::text = $2::text");
      (* What was typed so is a type for what stands beside it. *)
      (mul (add (int 1) (int 2)) (int 3), "($1::bigint + $2::bigint) * $3");
      (* PostgreSQL shifts a bigint by an integer. *)
      (left_shift (int 1) (int 4), "$1::bigint << $2::integer");
      (* Alone under its operator. *)
      (neg (int 5), "-$1::bigint");
      (not_ (bool true), "NOT $1::boolean");
      (is_null (int 7), "$1::bigint IS NULL");
      (* A null has no kind. Beside a constant it takes that one's type from the server;
         alone, what its operator is of. *)
      (add null (int 1), "$1 + $2::bigint");
      (add null null, "$1::bigint + $2::bigint");
      (eq null null, "$1 = $2");
      (is_null null, "$1::text IS NULL");
      (neg null, "-$1::bigint");
      (not_ null, "NOT $1");
    ]

(* PostgreSQL's [text] holds no NUL: a parameter with one in it is "invalid byte sequence
   for encoding UTF8: 0x00" from the server, at execution - a failure of the query where
   the application expects one of the data. A text with a NUL is refused where every
   value meets the server, by the compiler; in memory it is a string like any other. *)
let a_text_with_a_nul_is_no_text_postgresql_has () =
  let nul = eq (field "name") (text "a\x00b") in
  Alcotest.check compiled "nul" (Error Pg.Nul_in_text) (Pg.compile nul);
  Alcotest.(check string)
    "message" "a text with a NUL (U+0000) in it is no text PostgreSQL has"
    (match Pg.compile nul with Error e -> Pg.error_to_string e | Ok _ -> "");
  Alcotest.check compiled "in an item" (Error Pg.Nul_in_text)
    (Pg.compile (any "items" (eq (item "name") (text "\x00"))));
  Alcotest.(check bool)
    "plain" true
    (Result.is_ok (Pg.compile (eq (field "name") (text "ab"))))

let a_name_that_is_not_an_identifier_is_refused () =
  let invalid name = Error (Pg.Invalid_identifier name) in
  Alcotest.check compiled "injection"
    (invalid "age; DROP TABLE users")
    (Pg.compile (field "age; DROP TABLE users"));
  Alcotest.check compiled "quote" (invalid {|a" OR "b|}) (Pg.compile (field {|a" OR "b|}));
  Alcotest.check compiled "empty" (invalid "") (Pg.compile (field "a..b"));
  Alcotest.check compiled "digit" (invalid "1st") (Pg.compile (field "1st"));
  Alcotest.check compiled "space" (invalid "items x")
    (Pg.compile (any "items x" (bool true)));
  let schema =
    Schema.(make "stores" |> foreign_key "items; --" "store_id" "stores" "id")
  in
  Alcotest.check compiled "table" (invalid "items; --")
    (Pg.compile ~schema (any "items; --" (bool true)))

let the_item_is_only_inside_a_collection () =
  Alcotest.check compiled "item" (Error Pg.No_current_item) (Pg.compile (item "price"));
  Alcotest.check compiled "item source" (Error Pg.No_current_item)
    (Pg.compile (any_at (Path.item "items") (bool true)));
  Alcotest.check compiled "outer" (Error Pg.No_current_item)
    (Pg.compile (any "items" (gt (item "price") (outer 1 "limit"))))

(* The item of an enclosing collection has an alias of its own, which the inner query
   names as SQL lets it: [Item 1] is that alias. *)
let the_item_of_an_enclosing_collection_is_its_alias () =
  let over_its_category =
    any "categories" (any_at (Path.item "products") (gt (item "price") (outer 1 "limit")))
  in
  Alcotest.(check string)
    "embedded"
    {|EXISTS (SELECT 1 FROM unnest("categories") AS "category_1" WHERE EXISTS (SELECT 1 FROM unnest("category_1"."products") AS "product_2" WHERE "product_2"."price" > "category_1"."limit"))|}
    (sql over_its_category);
  (* In tables of their own, the enclosing row is the one the keys point at, and its
     columns are named the same way. *)
  let schema =
    Schema.(
      make "shops"
      |> foreign_key "categories" "shop_id" "shops" "id"
      |> foreign_key "products" "category_id" "categories" "id")
  in
  Alcotest.(check string)
    "relational"
    {|EXISTS (SELECT 1 FROM "categories" AS "category_1" WHERE "category_1"."shop_id" = "shops"."id" AND EXISTS (SELECT 1 FROM "products" AS "product_2" WHERE "product_2"."category_id" = "category_1"."id" AND "product_2"."price" > "category_1"."limit"))|}
    (sql ~schema over_its_category);
  (* Two collections out, the candidate's own row beside. *)
  let three_deep =
    any "categories"
      (any_at (Path.item "products")
         (any_at (Path.item "tags")
            (and_
               (gt (item "weight") (outer 2 "limit"))
               (lt (outer 1 "price") (field "limit")))))
  in
  Alcotest.(check string)
    "three deep"
    {|EXISTS (SELECT 1 FROM unnest("categories") AS "category_1" WHERE EXISTS (SELECT 1 FROM unnest("category_1"."products") AS "product_2" WHERE EXISTS (SELECT 1 FROM unnest("product_2"."tags") AS "tag_3" WHERE "tag_3"."weight" > "category_1"."limit" AND "product_2"."price" < "shops"."limit")))|}
    (sql ~schema:(Schema.make "shops") three_deep)

(* Inside a collection's predicate the candidate's column is qualified with its row.
   Unqualified, PostgreSQL read it from the innermost row that has a column of that name:
   a category with a [limit] of its own hid the shop's, and the query selected other rows
   than the evaluator was satisfied by. The row is what the schema calls it, so without a
   schema there is no query. *)
let the_candidates_column_inside_a_predicate_is_qualified_with_its_row () =
  let over_the_shops_limit = any "categories" (gt (item "limit") (field "limit")) in
  Alcotest.(check string)
    "table"
    {|EXISTS (SELECT 1 FROM unnest("categories") AS "category_1" WHERE "category_1"."limit" > "shops"."limit")|}
    (sql ~schema:(Schema.make "shops") over_the_shops_limit);
  Alcotest.(check string)
    "alias"
    {|EXISTS (SELECT 1 FROM unnest("categories") AS "category_1" WHERE "category_1"."limit" > "s"."limit")|}
    (sql ~schema:Schema.(make "public.shops" |> alias "s") over_the_shops_limit);
  Alcotest.check compiled "no schema" (Error Pg.No_table)
    (Pg.compile over_the_shops_limit);
  (* A name of several parts the author qualified, and it stays as written; outside a
     collection's predicate a name is unqualified, as it was. *)
  Alcotest.(check string)
    "qualified by the author"
    {|EXISTS (SELECT 1 FROM unnest("categories") AS "category_1" WHERE "category_1"."limit" > "s"."limit")|}
    (sql (any "categories" (gt (item "limit") (field "s.limit"))));
  Alcotest.(check string) "outside" {|"limit" > $1|} (sql (gt (field "limit") (int 1)))

(* A schema is the foreign keys of a storage and nothing of any query. A tree names a
   collection by its table, and where two keys of that table reference the row it is
   named from - the transfers from an account and the transfers to it - by the key's
   name, which is what PostgreSQL calls it. An object is named by the key's column. A row
   of an array, which has no table, is named by the array's column; and what the compiler
   calls a row in a query is its own. *)
let a_schema_is_the_foreign_keys_of_the_storage () =
  let schema =
    Schema.(
      make "accounts" |> alias "a"
      |> foreign_key "transfers" "from_account_id" "accounts" "id"
      |> foreign_key "transfers" "to_account_id" "accounts" "id"
      |> foreign_key "accounts" "owner_id" "owners" "id"
      |> foreign_key "accounts.cards" "issuer_id" "banks" "id")
  in
  let over what = any what (gt (item "amount") (int 100)) in
  Alcotest.(check string)
    "from"
    {|EXISTS (SELECT 1 FROM "transfers" AS "transfer_1" WHERE "transfer_1"."from_account_id" = "a"."id" AND "transfer_1"."amount" > $1)|}
    (sql ~schema (over "transfers_from_account_id_fkey"));
  Alcotest.(check string)
    "to"
    {|EXISTS (SELECT 1 FROM "transfers" AS "transfer_1" WHERE "transfer_1"."to_account_id" = "a"."id" AND "transfer_1"."amount" > $1)|}
    (sql ~schema (over "transfers_to_account_id_fkey"));
  (* By the table alone, the name fits two keys. *)
  Alcotest.check compiled "ambiguous"
    (Error
       (Pg.Ambiguous_key
          "transfers has 2 keys to accounts: transfers_from_account_id_fkey, \
           transfers_to_account_id_fkey; name the key"))
    (Pg.compile ~schema (over "transfers"));
  (* A key given a name goes by it. *)
  let named =
    Schema.(
      make "accounts" |> alias "a"
      |> key
           Foreign_key.(
             make "transfers" "from_account_id" "accounts" "id" |> named "outgoing"))
  in
  Alcotest.(check string)
    "named"
    {|EXISTS (SELECT 1 FROM "transfers" AS "transfer_1" WHERE "transfer_1"."from_account_id" = "a"."id" AND "transfer_1"."amount" > $1)|}
    (sql ~schema:named (over "outgoing"));
  (* A key named where it does not go: the tree stands in the account's row. *)
  Alcotest.check compiled "wrong key"
    (Error (Pg.Wrong_key "the key accounts_owner_id_fkey references owners, not accounts"))
    (Pg.compile ~schema (any "accounts_owner_id_fkey" (item "x")));
  (* A key on a row of an array. *)
  Alcotest.(check string)
    "array row"
    {|EXISTS (SELECT 1 FROM unnest("cards") AS "card_1" WHERE (SELECT "bank_2"."name" FROM "banks" AS "bank_2" WHERE "bank_2"."id" = "card_1"."issuer_id") = $1)|}
    (sql ~schema
       (any "cards"
          (eq (field_at (Path.child (Path.item "issuer_id") "name")) (text "x"))));
  (* A column of two keys. *)
  let shared =
    Schema.(
      make "stores"
      |> key (Foreign_key.make "stores" "tenant_id" "tenants" "id")
      |> key
           Foreign_key.(
             make "stores" "tenant_id" "owners" "tenant_id" |> and_ "owner_id" "id"))
  in
  Alcotest.check compiled "column of two keys"
    (Error
       (Pg.Ambiguous_key
          "tenant_id is a column of 2 keys of stores: stores_tenant_id_fkey, \
           stores_tenant_id_owner_id_fkey; name the key"))
    (Pg.compile ~schema:shared (eq (field "tenant_id.name") (text "x")));
  Alcotest.(check string)
    "composite key"
    {|(SELECT "owner_1"."name" FROM "owners" AS "owner_1" WHERE "owner_1"."tenant_id" = "stores"."tenant_id" AND "owner_1"."id" = "stores"."owner_id") = $1|}
    (sql ~schema:shared (eq (field "owner_id.name") (text "x")))

let a_key_is_named_as_postgresql_names_it_unless_named () =
  Alcotest.(check string)
    "one column" "transfers_from_account_id_fkey"
    (Foreign_key.name (Foreign_key.make "transfers" "from_account_id" "accounts" "id"));
  Alcotest.(check string)
    "composite" "orders_tenant_id_customer_id_fkey"
    (Foreign_key.name
       Foreign_key.(
         make "public.orders" "tenant_id" "tenants" "tenant_id" |> and_ "customer_id" "id"));
  Alcotest.(check string)
    "named" "outgoing"
    (Foreign_key.name
       Foreign_key.(
         make "transfers" "from_account_id" "accounts" "id" |> named "outgoing"))

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "pg_compile"
    [
      ( "text",
        [
          case "fields, values and operators" fields_values_and_operators;
          case "parameters are numbered in order and from an offset"
            parameters_are_numbered_in_order_and_from_an_offset;
          case "parentheses keep the shape of the tree"
            parentheses_keep_the_shape_of_the_tree;
          case "IS takes a parameter" is_takes_a_parameter;
          case "the count of a shift is an integer" the_count_of_a_shift_is_an_integer;
          case "a constant with nothing beside it has its type said"
            a_constant_with_nothing_beside_it_has_its_type_said;
          case "a text with a NUL is no text PostgreSQL has"
            a_text_with_a_nul_is_no_text_postgresql_has;
          case "a name that is not an identifier is refused"
            a_name_that_is_not_an_identifier_is_refused;
        ] );
      ( "collections",
        [
          case "an embedded collection is unnested" an_embedded_collection_is_unnested;
          case "a member of an object inside an item is a member of a composite"
            a_member_of_an_object_inside_an_item_is_a_member_of_a_composite;
          case "a member of an object kept in a table of its own is read through the key"
            a_member_of_an_object_kept_in_a_table_of_its_own_is_read_through_the_key;
          case "a composite column of the candidate is declared"
            a_composite_column_of_the_candidate_is_declared;
          case "a null test of a declared composite is of the value as a whole"
            a_null_test_of_a_declared_composite_is_of_the_value_as_a_whole;
          case "a relational collection is joined by its keys"
            a_relational_collection_is_joined_by_its_keys;
          case "two collections of one name are two collections"
            two_collections_of_one_name_are_two_collections;
          case "a collection of the candidate inside another joins to the root"
            a_collection_of_the_candidate_inside_another_joins_to_the_root;
          case "the predicate of a relational collection stays inside its keys"
            the_predicate_of_a_relational_collection_stays_inside_its_keys;
          case "the item is only inside a collection" the_item_is_only_inside_a_collection;
          case "the item of an enclosing collection is its alias"
            the_item_of_an_enclosing_collection_is_its_alias;
          case "the candidate's column inside a predicate is qualified with its row"
            the_candidates_column_inside_a_predicate_is_qualified_with_its_row;
          case "a schema is the foreign keys of the storage"
            a_schema_is_the_foreign_keys_of_the_storage;
          case "a key is named as PostgreSQL names it unless named"
            a_key_is_named_as_postgresql_names_it_unless_named;
        ] );
    ]
