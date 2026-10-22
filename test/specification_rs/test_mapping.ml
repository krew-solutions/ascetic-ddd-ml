(* From the domain's terms to the storage's: the composite identity of the Python
   [test_infrastructure] and the Go [transform_visitor_test] - a [something.id] that is a
   [MemberSomethingId] in the domain and three columns in the table. *)

open Ascetic_specification
open Ast
module E = Evaluate.Make (Value)

type member_id = { tenant_id : int64; member_id : int64 } [@@deriving show, eq]

type member_something_id = { member : member_id; something_id : int64 }
[@@deriving show, eq]

(* The values of the domain: scalars, and the Value Objects it compares by. *)
type domain =
  | Scalar of Value.t
  | Member_id of member_id
  | Member_something_id of member_something_id
[@@deriving show, eq]

let something_id tenant_id member_id something_id =
  Member_something_id { member = { tenant_id; member_id }; something_id }

let int i : Value.t Ast.t = value (Value.of_int i)
let scalar i : domain Ast.t = value (Scalar (Value.of_int i))
let column path name = Mapping.Scalar (Field (Path.sibling path name))

let something : (domain, Value.t, string) Mapping.t =
  Mapping.make
    ~field:(fun path ->
      match Path.names path with
      | [ "something"; "id" ] ->
          Ok
            (Mapping.Composite
               [
                 Composite [ column path "tenant_id"; column path "member_id" ];
                 column path "something_id";
               ])
      | [ "something"; "member_id" ] ->
          Ok (Composite [ column path "tenant_id"; column path "member_id" ])
      | [ "something"; (("rank" | "deleted_at") as name) ] -> Ok (column path name)
      (* A collection is a member: where it is, by a path from the candidate. And a
           member of its item by its whole path, which starts with the collection's, in
           the domain's names and in the storage's. *)
      | [ "something"; "parts" ] -> Ok (Scalar (Field (Path.global "parts")))
      | [ "something"; "tags" ] ->
          Ok (Scalar (Field (Path.child (Path.global "document") "tags")))
      | [ "something"; "parts"; "weight" ] ->
          Ok (Scalar (Field (Path.child (Path.global "parts") "weight_grams")))
      | names -> Error ("unknown field: " ^ String.concat "." names))
    ~value:(fun value ->
      let scalar v = Mapping.Scalar (Value (Value.Int v)) in
      let member (id : member_id) =
        Mapping.Composite [ scalar id.tenant_id; scalar id.member_id ]
      in
      Ok
        (match value with
        | Scalar value -> Scalar (Value value)
        | Member_id id -> member id
        | Member_something_id id -> Composite [ member id.member; scalar id.something_id ]))
    ()

let spec = Alcotest.testable (Ast.pp Value.pp) (Ast.equal Value.equal)

let error =
  Alcotest.testable
    (Mapping.pp_error Format.pp_print_string)
    (Mapping.equal_error String.equal)

let transformed = Alcotest.(result spec error)
let satisfied = Alcotest.(result bool (testable Evaluate.pp_error Evaluate.equal_error))
let value_t = Alcotest.testable Value.pp Value.equal

let compiled query =
  match query with
  | Ok (query : Value.t Pg.query) -> query
  | Error e -> Alcotest.fail (Pg.error_to_string e)

let get = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Mapping.error_to_string Fun.id e)

let an_equality_of_composites_is_the_conjunction_of_the_equalities_of_their_parts () =
  let specification = eq (field "something.id") (value (something_id 10L 3L 5L)) in
  let result = Mapping.transform something specification in
  Alcotest.check transformed "transformed"
    (Ok
       (and_
          (and_
             (eq (field "something.tenant_id") (int 10))
             (eq (field "something.member_id") (int 3)))
          (eq (field "something.something_id") (int 5))))
    result;
  let query = compiled (Pg.compile (get result)) in
  Alcotest.(check string)
    "sql"
    {|"something"."tenant_id" = $1 AND "something"."member_id" = $2 AND "something"."something_id" = $3|}
    query.sql;
  Alcotest.(check (list value_t))
    "params"
    [ Value.Int 10L; Value.Int 3L; Value.Int 5L ]
    query.params

let composites_are_unequal_when_not_equal_in_every_part () =
  let specification = ne (field "something.id") (value (something_id 10L 3L 5L)) in
  let result = get (Mapping.transform something specification) in
  Alcotest.(check string)
    "sql"
    {|NOT ("something"."tenant_id" = $1 AND "something"."member_id" = $2 AND "something"."something_id" = $3)|}
    (compiled (Pg.compile result)).sql;
  (* The sources have NOT (t != $1 AND m != $2 AND s != $3), by which an identity is
     unequal to itself and equal to one that shares no part. *)
  let row tenant_id member_id something_id =
    Record.(
      to_context
        (object_
           [
             ( "something",
               object_
                 [
                   ("tenant_id", value (Value.of_int tenant_id));
                   ("member_id", value (Value.of_int member_id));
                   ("something_id", value (Value.of_int something_id));
                 ] );
           ]))
  in
  Alcotest.check satisfied "itself" (Ok false) (E.is_satisfied_by result (row 10 3 5));
  Alcotest.check satisfied "one part" (Ok true) (E.is_satisfied_by result (row 10 3 6));
  Alcotest.check satisfied "no part" (Ok true) (E.is_satisfied_by result (row 11 4 6))

let what_is_not_composite_passes_through_with_its_names_and_values_mapped () =
  let specification =
    and_
      (not_ (lt (field "something.rank") (scalar 3)))
      (is_null (field "something.deleted_at"))
  in
  Alcotest.check transformed "passes through"
    (Ok
       (and_
          (not_ (lt (field "something.rank") (int 3)))
          (is_null (field "something.deleted_at"))))
    (Mapping.transform something specification)

let the_predicate_of_a_collection_is_transformed_too () =
  let specification = any "something.parts" (gt (item "weight") (scalar 100)) in
  Alcotest.check transformed "collection"
    (Ok (any "parts" (gt (item "weight_grams") (int 100))))
    (Mapping.transform something specification)

(* A mapping is of the aggregate's members and knows nothing of any query: it is asked
   about a member of an item by the member's whole path from the candidate, and where the
   answer goes - from which item, how far out - is the tree's. The mapping's answer for a
   member of an item starts with its answer for the collection, and the rest is the
   member from the item. *)
let a_mapping_is_asked_by_the_whole_path_and_the_answer_is_put_where_the_member_was () =
  let shops : (domain, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun path ->
        Alcotest.(check bool)
          "asked from the candidate" true
          (Path.root path = Path.Global);
        match Path.names path with
        | [ "limit" ] -> Ok (Scalar (field "max_price"))
        | [ "categories" ] -> Ok (Scalar (field "cats"))
        | [ "categories"; "limit" ] -> Ok (Scalar (field "cats.max_price"))
        | [ "categories"; "products" ] -> Ok (Scalar (field "cats.goods"))
        | [ "categories"; "products"; "price" ] ->
            Ok (Scalar (field "cats.goods.price_cents"))
        | [ "categories"; "products"; "id" ] ->
            let id = Path.of_string "cats.goods.id" in
            Ok (Composite [ column id "tenant_id"; column id "product_id" ])
        | names -> Error ("unknown field: " ^ String.concat "." names))
      ~value:(function
        | Scalar value -> Ok (Scalar (Value value))
        | _ -> Error "not a scalar")
      ()
  in
  let specification =
    any "categories"
      (any_at (Path.item "products")
         (and_
            (gt (item "price") (outer 1 "limit"))
            (and_ (lt (outer 1 "limit") (field "limit")) (eq (item "id") (item "id")))))
  in
  Alcotest.check transformed "whole path"
    (Ok
       (any "cats"
          (any_at (Path.item "goods")
             (and_
                (gt (item "price_cents") (outer 1 "max_price"))
                (and_
                   (lt (outer 1 "max_price") (field "max_price"))
                   (and_
                      (eq (item "tenant_id") (item "tenant_id"))
                      (eq (item "product_id") (item "product_id"))))))))
    (Mapping.transform shops specification)

let what_the_mapping_puts_outside_its_collection_is_refused () =
  let astray : (domain, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun path ->
        Ok
          (Scalar
             (Field
                (match Path.names path with
                | [ _ ] -> path
                (* A member of an item answered with a column of the candidate. *)
                | _ -> Path.global "elsewhere"))))
      ~value:(fun _ -> Ok (Scalar (int 1)))
      ()
  in
  let valued : (domain, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun _ -> Ok (Scalar (int 1)))
      ~value:(fun _ -> Ok (Scalar (int 1)))
      ()
  in
  let specification = any "items" (gt (item "weight") (scalar 1)) in
  Alcotest.check transformed "astray" (Error Mapping.Outside_its_collection)
    (Mapping.transform astray specification);
  Alcotest.check transformed "valued" (Error Mapping.Collection_not_a_place)
    (Mapping.transform valued specification);
  Alcotest.check transformed "no item" (Error Mapping.No_current_item)
    (Mapping.transform astray (gt (item "weight") (scalar 1)))

(* What a repository does with a specification: the mapping says what the members are in
   the storage, the schema how the storage is laid out, and the schema names a collection
   as the mapping left it. *)
let a_mapping_and_a_schema_are_given_together () =
  let specification = any "something.parts" (gt (item "weight") (scalar 100)) in
  let schema =
    Pg.Schema.(make "things" |> alias "t" |> foreign_key "parts" "thing_id" "things" "id")
  in
  let query =
    compiled (Pg.compile ~schema (get (Mapping.transform something specification)))
  in
  Alcotest.(check string)
    "sql"
    {|EXISTS (SELECT 1 FROM "parts" AS "part_1" WHERE "part_1"."thing_id" = "t"."id" AND "part_1"."weight_grams" > $1)|}
    query.sql;
  Alcotest.(check (list value_t)) "params" [ Value.Int 100L ] query.params

let composites_of_different_shapes_do_not_compare () =
  let member_id = Member_id { tenant_id = 10L; member_id = 3L } in
  (* Of one length, but the first part of one is itself a composite. *)
  Alcotest.check transformed "shorter" (Error Mapping.Shape_mismatch)
    (Mapping.transform something (eq (field "something.id") (value member_id)));
  (* A composite against a scalar. *)
  Alcotest.check transformed "scalar" (Error Mapping.Not_composite)
    (Mapping.transform something (eq (field "something.id") (scalar 5)));
  Alcotest.check transformed "reversed" (Error Mapping.Not_composite)
    (Mapping.transform something (eq (scalar 5) (field "something.id")))

let a_composite_takes_only_equality () =
  let id () = value (something_id 10L 3L 5L) in
  Alcotest.check transformed ">" (Error (Mapping.Unsupported_operator Operator.gt))
    (Mapping.transform something (gt (field "something.id") (id ())));
  Alcotest.check transformed "and" (Error (Mapping.Unsupported_operator Operator.and_))
    (Mapping.transform something (and_ (field "something.id") (id ())));
  (* Nor can one stand where a single expression is needed. *)
  List.iter
    (fun specification ->
      Alcotest.check transformed "unexpected" (Error Mapping.Unexpected_composite)
        (Mapping.transform something specification))
    [ field "something.id"; is_null (field "something.id"); not_ (id ()) ]

let composites_of_different_lengths_do_not_compare () =
  let uneven : (domain, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun path -> Ok (Composite [ column path "a"; column path "b" ]))
      ~value:(fun _ -> Ok (Composite [ Scalar (int 1); Scalar (int 2); Scalar (int 3) ]))
      ()
  in
  Alcotest.check transformed "lengths" (Error Mapping.Shape_mismatch)
    (Mapping.transform uneven (eq (field "id") (scalar 1)))

let a_composite_has_parts () =
  let hollow : (domain, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun _ -> Ok (Composite []))
      ~value:(fun _ -> Ok (Composite []))
      ()
  in
  Alcotest.check transformed "empty" (Error Mapping.Empty_composite)
    (Mapping.transform hollow (eq (field "id") (scalar 1)))

let the_mappings_refusal_is_the_transformations () =
  Alcotest.check transformed "refusal"
    (Error (Mapping.Mapping "unknown field: something.colour"))
    (Mapping.transform something (eq (field "something.colour") (scalar 1)))

(* A value of the domain that the storage keeps as a null - a special case that answers
   for itself in the domain - is tested for where it is compared for equality:
   [owner = $1] with a null is true of nothing. It is the mapping that says a null is
   that, with [Mapping.Null]; a null that was one in the domain already stays compared. *)
type who =
  | Somebody of int64
  | Nobody  (** The special case: an owner that is nobody, equal to itself. *)
  | Unknown  (** Not known, in the domain as in the storage. *)
  | Pair of int64 * int64 option
      (** Known by two numbers, of which the second may be nobody's. *)
[@@deriving show, eq]

let equality_with_what_the_mapping_says_is_the_storages_null_is_the_null_test () =
  let owners : (who, Value.t, string) Mapping.t =
    Mapping.make
      ~field:(fun path ->
        Ok
          (match Path.name path with
          | "pair" -> Composite [ column path "a"; column path "b" ]
          | _ -> Scalar (Field path)))
      ~value:(fun value ->
        let known id = Mapping.Scalar (Value (Value.Int id)) in
        Ok
          (match value with
          | Somebody id -> known id
          | Nobody -> Null Value.Null
          | Unknown -> Scalar (Value Value.Null)
          | Pair (a, b) ->
              Composite
                [
                  known a;
                  (match b with
                  | None -> Null Value.Null
                  | Some b -> known b);
                ]))
      ()
  in
  let owner () = field "owner"
  and nobody () = value Nobody
  and null () = value Value.Null in
  List.iter
    (fun (name, specification, expected) ->
      Alcotest.check transformed name (Ok expected)
        (Mapping.transform owners specification))
    [
      (* Somebody is compared, as any value is. *)
      ("somebody", eq (owner ()) (value (Somebody 7L)), eq (field "owner") (int 7));
      ("nobody", eq (owner ()) (nobody ()), is_null (field "owner"));
      ("nobody reversed", eq (nobody ()) (owner ()), is_null (field "owner"));
      ("not nobody", ne (owner ()) (nobody ()), is_not_null (field "owner"));
      ("nobody = nobody", eq (nobody ()) (nobody ()), is_null (null ()));
      (* Under any other operator it is the null it carries. *)
      ("> nobody", gt (owner ()) (nobody ()), gt (field "owner") (null ()));
      (* A null of the domain's own stays compared. *)
      ("unknown", eq (owner ()) (value Unknown), eq (field "owner") (null ()));
      (* A part of a composite is tested for as a whole is. *)
      ( "pair",
        eq (field "pair") (value (Pair (1L, None))),
        and_ (eq (field "a") (int 1)) (is_null (field "b")) );
    ]

(* An element of a collection of values is not asked about by default: it is what the
   collection holds, where the mapping put the collection, and the constant it is
   compared with is the mapping's. A composite constant has no parts there to be compared
   with, and is refused. *)
let an_element_of_a_collection_of_values_is_where_its_collection_is () =
  let tagged = any "something.tags" (eq itself (scalar 5)) in
  Alcotest.check transformed "where the collection is"
    (Ok (any "document.tags" (eq itself (int 5))))
    (Mapping.transform something tagged);
  let composite = any "something.tags" (eq itself (value (something_id 1L 2L 3L))) in
  Alcotest.check transformed "composite" (Error Mapping.Not_composite)
    (Mapping.transform something composite);
  Alcotest.check transformed "no item" (Error Mapping.No_current_item)
    (Mapping.transform something itself)

(* Members kept as a tenant's documents: a member id is its tenant and its member; the
   storage keeps the member in an array, and the tenant once, in the row - of the members
   of the tenant, and of the members of each of its teams. *)
let members : (domain, Value.t, string) Mapping.t =
  let document names = Ok (Mapping.Scalar (field ("document." ^ names))) in
  Mapping.make
    ~field:(fun path ->
      match Path.names path with
      | [ "tenant_id" ] -> Ok (Scalar (field "tenant_id"))
      | [ "members" ] -> document "members"
      | [ "teams" ] -> document "teams"
      | [ "teams"; "members" ] -> document "teams.members"
      | names -> Error ("unknown field: " ^ String.concat "." names))
    ~value:something.value
    ~element:(fun collection ->
      match Path.names collection with
      | [ "members" ] | [ "teams"; "members" ] ->
          Ok (Composite [ Scalar (field "tenant_id"); Scalar itself ])
      | names -> Error ("no collection of values: " ^ String.concat "." names))
    ()

(* An element the storage keeps in part in the candidate's row is what the mapping says it
   is from the collection's predicate: the tenant in the row and the member in the array.
   An equality with a member id is then the tenant compared with the row's and the member
   with the element. An element of an enclosing collection is said from its own predicate
   and put where it was; the candidate's row is where it was. *)
let an_element_kept_in_part_in_the_candidates_row_is_compared_part_by_part () =
  let member tenant_id member_id = value (Member_id { tenant_id; member_id }) in
  let one = any "members" (eq itself (member 10L 3L)) in
  let result = Mapping.transform members one in
  Alcotest.check transformed "part by part"
    (Ok
       (any "document.members"
          (and_ (eq (field "tenant_id") (int 10)) (eq itself (int 3)))))
    result;
  let schema =
    Pg.Schema.(
      make "tenants" |> alias "t"
      |> document "tenants" "document"
           Pg.Layout.(
             empty |> values "members" Kind.Int
             |> collection "teams" (empty |> values "members" Kind.Int)))
  in
  Alcotest.(check string)
    "sql"
    {|EXISTS (SELECT 1 FROM jsonb_array_elements_text(CASE WHEN jsonb_typeof("t"."document" -> 'members') = 'array' THEN "t"."document" -> 'members' END) AS "member_1" WHERE "t"."tenant_id" = $1 AND "member_1"::bigint = $2)|}
    (compiled (Pg.compile ~schema (get result))).sql;
  (* A member of the tenant who is a member of one of its teams: the element two
     collections out, said from its own predicate. *)
  let in_a_team =
    any "members"
      (any "teams" (any_at (Path.item "members") (eq itself (outer_itself 2))))
  in
  Alcotest.check transformed "two out"
    (Ok
       (any "document.members"
          (any "document.teams"
             (any_at (Path.item "members")
                (and_
                   (eq (field "tenant_id") (field "tenant_id"))
                   (eq itself (outer_itself 2)))))))
    (Mapping.transform members in_a_team);
  (* A composite has no one value to be null: as of a member, refused. *)
  Alcotest.check transformed "null test" (Error Mapping.Unexpected_composite)
    (Mapping.transform members (any "members" (is_null itself)))

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "mapping"
    [
      ( "transform",
        [
          case
            "an equality of composites is the conjunction of the equalities of their \
             parts"
            an_equality_of_composites_is_the_conjunction_of_the_equalities_of_their_parts;
          case "composites are unequal when not equal in every part"
            composites_are_unequal_when_not_equal_in_every_part;
          case "what is not composite passes through with its names and values mapped"
            what_is_not_composite_passes_through_with_its_names_and_values_mapped;
          case "the predicate of a collection is transformed too"
            the_predicate_of_a_collection_is_transformed_too;
          case
            "a mapping is asked by the whole path and the answer is put where the member \
             was"
            a_mapping_is_asked_by_the_whole_path_and_the_answer_is_put_where_the_member_was;
          case "what the mapping puts outside its collection is refused"
            what_the_mapping_puts_outside_its_collection_is_refused;
          case "a mapping and a schema are given together"
            a_mapping_and_a_schema_are_given_together;
          case "composites of different shapes do not compare"
            composites_of_different_shapes_do_not_compare;
          case "a composite takes only equality" a_composite_takes_only_equality;
          case "composites of different lengths do not compare"
            composites_of_different_lengths_do_not_compare;
          case "a composite has parts" a_composite_has_parts;
          case "the mapping's refusal is the transformation's"
            the_mappings_refusal_is_the_transformations;
          case
            "equality with what the mapping says is the storage's null is the null test"
            equality_with_what_the_mapping_says_is_the_storages_null_is_the_null_test;
          case "an element of a collection of values is where its collection is"
            an_element_of_a_collection_of_values_is_where_its_collection_is;
          case "an element kept in part in the candidate's row is compared part by part"
            an_element_kept_in_part_in_the_candidates_row_is_compared_part_by_part;
        ] );
    ]
