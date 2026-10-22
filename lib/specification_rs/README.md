# Specification

The Specification pattern: a predicate on a domain object, kept as a tree, so
that one statement of a business rule answers two questions - *does this
object in memory satisfy it?* and *which rows of the table do?*

A port of the reference implementation in Rust, `ascetic-ddd-specification`,
itself a port of the Python and Go sources. The design, the requirements it
answers and the alternatives rejected are in
[ADR-0017](../../docs/src/adr/0017-what-a-specification-must-do.md),
[ADR-0018](../../docs/src/adr/0018-a-specification-is-a-tree-over-its-values-read-by-functions.md),
[ADR-0019](../../docs/src/adr/0019-a-null-is-tested-not-compared-where-null-is-a-value.md),
[ADR-0020](../../docs/src/adr/0020-a-name-is-the-storages-quoted-as-it-is-and-mapped-in-one-place.md)
[ADR-0021](../../docs/src/adr/0021-a-parameter-goes-to-the-server-as-text-of-no-declared-type.md),
[ADR-0022](../../docs/src/adr/0022-a-predicate-function-yields-its-tree-through-a-ppx.md),
[ADR-0026](../../docs/src/adr/0026-a-string-constant-is-read-as-the-kind-of-the-member-beside-it.md),
[ADR-0027](../../docs/src/adr/0027-a-member-is-read-where-the-mapping-puts-it-and-a-document-as-its-layout-says.md),
[ADR-0028](../../docs/src/adr/0028-an-equality-where-only-truth-counts-is-containment-where-the-document-has-a-gin-index.md),
[ADR-0029](../../docs/src/adr/0029-an-element-of-a-collection-of-values-is-the-item-itself.md),
[ADR-0030](../../docs/src/adr/0030-a-gin-index-on-a-path-of-a-document-serves-containment-of-the-value-there.md),
[ADR-0031](../../docs/src/adr/0031-an-element-is-what-the-mapping-says-from-the-collections-predicate.md),
[ADR-0032](../../docs/src/adr/0032-the-typed-terms-quantify-a-collection-with-a-boolean-term.md)
and
[ADR-0033](../../docs/src/adr/0033-a-collection-of-a-document-that-is-not-an-array-has-no-items-and-a-containment-pattern-folds-under-a-custom-plan-alone.md).

Three libraries: `ascetic_ddd.specification` is the tree, its values, the
evaluator, the typed terms, the templates, the mapping and the PostgreSQL
compiler, and depends on nothing; `ascetic_ddd.specification.caqti` sends a
compiled query's parameters through Caqti; `ascetic_ddd.specification.ppx`
is `let%specification`, a predicate function and its tree from one source.
Inside the first, `domain/` is the specification itself - the tree, the
values and the ways to build and to evaluate it, pure, with nothing of any
storage - and `infrastructure/` is a specification on its way to a storage:
the mapping, the compiler in `infrastructure/pg/`, and beside it, in
`infrastructure/pg/caqti/`, the Caqti adapter as a library of its own, as
the reference lays them out. The ppx is beside the library, as the
reference's macro is a crate beside the crate.

## Four ways to write a specification

All four build the same tree, `'v Ast.t`.

**By hand**, with the functions of `Ast`:

```ocaml
open Ascetic_specification

let specification : Value.t Ast.t =
  Ast.(and_ (field "active") (any "items" (gt (item "price") (value (Value.Int 500L)))))
```

**With typed terms**, `Dsl`, which refuse at compile time what the tree
would accept and the evaluator refuse later - a number conjoined, a text
multiplied, `is_null` of a member not declared nullable, a predicate of a
collection that is no truth value:

```ocaml
open Ascetic_specification
open Ascetic_specification.Dsl

let cheap = Infix.(Number.field "price" - Number.field "discount" < Number.of_int 100)
let listed = Infix.(Boolean.field "active" && is_null (Null_datetime.field "deleted_at"))
let dear = any "items" Infix.(Number.field_at (Path.item "price") > Number.of_int 500)
let on_sale = any "tags" Infix.(Text.itself = Text.value "sale")
let specification = expr Infix.(cheap && listed && dear && on_sale)
```

`Infix` redefines OCaml's `&&`, `||`, `=`, `<`, `+`, `*` and their kin over
terms, for the scope that opens it: OCaml gives an operator its precedence by
its first character, so these are the only spellings that bind as SQL's do.
Every operator is a named function as well, `and_`, `lt`, `add`.

**As a JSONPath template**, `Jsonpath`, parsed once and bound to parameters
for each use:

```ocaml
open Ascetic_specification

let dear = Jsonpath.Template.parse_exn "$.items[*][?@.price > %(price)f && @.active == true]"

let store =
  Record.(
    to_context
      (object_
         [ ("items", collection [ object_ [ ("price", value (Value.Float 999.0)); ("active", value (Value.Bool true)) ] ]) ]))

let () =
  assert (Jsonpath.Template.matches dear store (Jsonpath.Params.named [ ("price", Value.Float 500.0) ]) = Ok true)
```

**As an OCaml function**, with `let%specification`. The function stays what
it is, the fastest way to check an object in memory; beside it appears
`<name>_ast`, the same predicate as a tree. What the Python source does at
run time by reading the lambda's source, and the Go source with a generator
before compilation, a ppx does during it:

```ocaml
type item = { price : int64; active : bool }
type store = { active : bool; items : item list }

let%specification has_dear_items (store : store) (price : int64) =
  store.active && List.exists (fun (item : item) -> item.price > price && item.active) store.items

let () =
  let store = { active = true; items = [ { price = 999L; active = true } ] } in
  assert (has_dear_items store 500L);
  match Pg.compile (has_dear_items_ast 500L) with
  | Ok { sql; _ } ->
      assert (sql = {|"active" AND EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."price" > $1 AND "item_1"."active")|})
  | Error _ -> assert false
```

The first parameter is the candidate; the others are the constants of the
specification, each annotated with its type - `int`, `int64`, `float`,
`string`, `bool`, `Value.t`, or an `option` of one - and `<name>_ast` takes
the same, less the candidate. The body is one expression of: members of the
candidate and of the item; literals and the parameters, which become values;
`= <> < <= > >=`, `&& || not`, `+ - * / mod lsl asr` and their `Int64`,
`Int` and `Float` spellings, `Int64.add s.a 1L`; `String.equal`,
`Int64.equal` and their kin, which is how a Value Object compares;
`Option.is_none` and `Option.is_some`, and `= None`, which is the same;
`Some x`, which is `x`; `Option.fold ~none:false ~some:(fun held -> ...)`
and `match x with None -> false | Some held -> ...` of a member or a
parameter that is an option, where the name stands for it, and `~none:true`,
`None -> true` the same with `||`, which are how what an option holds is
ordered - `<` with `Some x`, `None` or an option parameter is refused;
`List.exists (fun item -> ...)` and `List.for_all`, nested as deep as the
collections are, the item a member's owner or, of a collection of values, the
value itself, `fun tag -> tag = x`; `List.mem x collection`, which is
`List.exists` of that. Anything else is a compile error at the place it
stands, and the function stays as it was. The ppx has no types: a member that
is an option is not seen to be one, as a parameter is.

## Two ways to read one

**In memory**: `Evaluate.Make (Value)`, or the same functor over a domain's
own value type, gives `is_satisfied_by` against anything that is a
`Context.t` - a domain object saying which of its members are values, which
objects, which collections, and whether the items of a collection are objects
or values, `Context.element`; or a `Record.t` of plain data.

## A collection of values

The groups a policy targets are a collection of identifiers, whose elements
have no members to name. The element is the item itself, `Ast.Item`
(ADR-0029): `any "tags" (eq itself (value (Value.Text "sale")))`,
`$.tags[*][?@ == %s]` in a template, `List.mem tag store.tags` or
`List.exists (fun t -> t = tag) store.tags` in an OCaml function. A domain
object gives the elements by value, `Context.Value`: an element is often a
type of the domain's own, for which no `Context.t` exists where it is used.
In SQL an element of an array of a scalar type is the alias `unnest` gives
it, `EXISTS (SELECT 1 FROM unnest("tags") AS "tag_1" WHERE "tag_1" = $1)`;
of a document's array of values, declared by `Pg.Layout.values`, the text
`jsonb_array_elements_text` gives it, read as its kind. A row of a table and
an object of a document have members, and are no value: the item itself of
either is refused.

**As SQL**: `Pg.compile`, to a condition with numbered parameters; with a
`Pg.Schema.t` for the collections kept in tables of their own and the
columns that hold a document. The parameters go to Caqti through
`Ascetic_specification_caqti.Params`:

```ocaml
module Params = Ascetic_specification_caqti.Params

let selected (module C : Caqti_eio.CONNECTION) specification =
  match Pg.compile specification with
  | Error error -> Error (`Compile error)
  | Ok { sql; params } -> (
      let text = "SELECT id FROM stores WHERE " ^ sql in
      match Params.request Caqti_type.int64 Caqti_mult.zero_or_more text params with
      | Error Params.Nul_in_text -> Error `Nul
      | Ok (Params.R (request, args)) -> Result.map_error (fun e -> `Caqti e) (C.collect_list request args))
```

The two agree, nulls included: the logic of the evaluator is PostgreSQL's
three-valued one, and `test/specification_rs/test_pg.ml` holds them against
each other on a live database - the same value or the same error for each
constant expression, the same rows selected for each specification, in both
storages of a collection.

Between the two stands `Mapping.transform`, for a domain whose members and
values are not the table's columns and scalars: a composite identity compared
as one value in the domain and column by column in the query, a Value Object
kept as a null.

## An aggregate kept as a document

A table may keep an aggregate as one `jsonb` document, with a few of its
members beside it as columns a listing filters by (ADR-0027). Where each
member is read from is the `Mapping.t`'s to say, member by member: a column,
`aggregate_version`, or a path in the document, `document.status`. A member
moved from the document to a column is one answer of the mapping changed,
and no specification. What the document holds is the `Pg.Schema.t`'s to say,
by a `Pg.Layout.t`: a document does not say the kinds of its members - a
point in time and a UUID are strings in it - so the layout gives each its
`Pg.Kind.t`, and the compiler reads it as that.

```ocaml
open Ascetic_specification

let schema =
  Pg.Schema.(
    make "policies" |> alias "p"
    |> document "policies" "document"
         Pg.Layout.(
           empty
           |> value "status" Kind.Text
           |> collection "versions" (empty |> value "number" Kind.Int)))

(* As the mapping left it: [published] a column, the rest in the document. *)
let specification : Value.t Ast.t =
  Ast.(
    and_ (field "published")
      (and_
         (eq (field "document.status") (value (Value.Text "active")))
         (any "document.versions" (gt (item "number") (value (Value.Int 2L))))))

let () =
  match Pg.compile ~schema specification with
  | Ok { sql; _ } ->
      assert (
        sql
        = {|"published" AND (("p"."document" ->> 'status') COLLATE "C") = $1 AND EXISTS (SELECT 1 FROM jsonb_array_elements(CASE WHEN jsonb_typeof("p"."document" -> 'versions') = 'array' THEN "p"."document" -> 'versions' END) AS "version_1" WHERE ("version_1" ->> 'number')::bigint > $2)|})
  | Error _ -> assert false
```

A member is read by `->>`, which has the null of SQL for a JSON null, and
then cast to its kind, or for a text ordered by code point: a value of a
type, as a column's is, so the logic of nulls, the arithmetic and the
reading of a string constant beside it are what they are for a column. The
query is not a JSONPath one: there a null is a value, `!($.a == 5)` is true
of a null `a`, `7 / 2` is `3.5`, an enclosing item has no name, and an index
serves equality alone.

A member that a listing compares by order is a column beside the document:
one the server generates from it, `priority bigint GENERATED ALWAYS AS
((document ->> 'priority')::bigint) STORED`, is what the document says by
construction; or an index on the expression the compiler writes,
`((document ->> 'priority')::bigint)`, `((document ->> 'status') COLLATE
"C")`. A point in time or a date has neither: its cast depends on the
session's settings, and the server refuses it in an index - it is a column
the repository writes, of its type.

An equality is served by a GIN index on the document, where the schema says
the document has one, `Pg.Schema.gin "policies" "document"` (ADR-0028). Where
only truth counts - the condition as a whole, `AND` and `OR` there, the
predicate of `any` - an equality of a text, an integer or a boolean member
with a constant is what the document contains, `"p"."document" @>
jsonb_object(ARRAY['status'], ARRAY[$1::text])`; and `any` over a collection
of the document has the equalities of its predicate beside its `EXISTS`, as
containment. A GIN index on a path of the document, as `CREATE INDEX ...
USING gin ((document -> 'subgroups'))` has it, is `Pg.Schema.gin_at "groups"
"document" "subgroups"` (ADR-0030): containment of what is under the path is
written of the value there, `("g"."document" -> 'subgroups') @> ...`, the
expression the index is on, which containment of the whole document is not.
Under `NOT`, with a null constant, of a member of another kind, an equality
is what it was: containment is false where the equality is null, and agrees
with it only for a kind with one spelling in JSON. The pattern is made of
immutable functions, which the server folds into a constant under a custom
plan, where the parameters are known; under a generic plan - a named prepared
statement after its fifth execution - it is evaluated for every row a
sequential scan reads, and once for an index scan (ADR-0033).
`jsonb_build_object` is stable, and is called again for every row under
either plan.

## What maps to what

| Reference (Rust) | here |
| --- | --- |
| `Expr<V>`, an enum | `'v Ast.t`, a variant; a reader is a function that matches on it |
| `Expr::Item(up)`, `ast::item()`, `ast::outer_item(up)` | `Ast.Item up`, `Ast.itself`, `Ast.outer_itself up` |
| `Path`, `Root` | `Path.t`, `Path.root` |
| `Operand`, a trait | `Operand.S`, a module signature; `Value` satisfies it, a domain's value type does the same |
| `evaluate`, `is_satisfied_by`, generic in `V: Operand` | `Evaluate.Make (O)`, a functor over the value type |
| `Context<V>`, a trait, `&dyn Context` | `'v Context.t`, a record of three functions |
| `Element::Object(&dyn Context)`, `Element::Value(v)` | `Context.Object context`, `Context.Value v` |
| `Record<V>` | `'v Record.t`, `Record.to_context` |
| `ast::equal`, `not_equal`, `greater_than`, ... | `Ast.eq`, `ne`, `gt`, `lt`, `ge`, `le`: `Ast.equal` is structural equality, as everywhere in OCaml |
| `dsl::Term<S, N>`, operators by trait | `('s, 'n) Dsl.t`, phantom sort and nullability; named functions, and `Dsl.Infix` |
| `dsl::Number::field`, `NullText::value(None)` | `Dsl.Number.field`, `Dsl.Null_text.value None` |
| `dsl::any`, `dsl::all`, `Text::item()`, `Text::outer_item(1)` | `Dsl.any`, `Dsl.all`, `Dsl.Text.itself`, `Dsl.Text.outer_itself 1` |
| `jsonpath::Template`, `Params`, `Slot` | `Jsonpath.Template`, `Params`, `Slot` |
| `null_test::equal`, `throughout` | `Null_test.Make (O)` |
| `Mapping`, a trait with an error type | `('d, 's, 'e) Mapping.t`, a record of three functions; `Mapping.make ~field ~value ()` answers `element` by default, as the trait's default method does |
| `pg::Compiler::new().schema(s).offset(n).compile(e)` | `Pg.compile ~schema ~offset e` |
| `pg::Schema::new("t").alias("a").foreign_key(..)` | `Pg.Schema.(make "t" \|> alias "a" \|> foreign_key ..)` |
| `Schema::document(t, c, layout)`, `gin`, `gin_at` | `Pg.Schema.document t c layout`, `gin`, `gin_at`, in the same pipe |
| `Layout::new().value(n, Kind::Text).collection(n, l).values(n, k)` | `Pg.Layout.(empty \|> value n Kind.Text \|> collection n l \|> values n k)` |
| `ToSql for Value`, feature `pg` | `Ascetic_specification_caqti.Params` |
| `#[specification]`, `<name>_ast(params)` | `let%specification`, `<name>_ast params`; a predicate without constants has `<name>_ast` a value |
| `.is_some_and(\|x\| ..)`, `.is_none_or(\|x\| ..)` | `Option.fold ~none:false ~some:(fun x -> ..)`, `~none:true`; or a `match` with `None -> false`, `None -> true` |
| `.iter().any(\|item\| ..)`, `.all(..)` | `List.exists (fun item -> ..)`, `List.for_all` |
| `.contains(&x)` | `List.mem x xs` |
| a specification type, `self.min` a constant | the constants are parameters: `let%specification f (s : store) (min : int64) = ..` |

## What the reference established, and this port keeps

The reference ran the Python and Go sources, found where they were wrong, and
carried the corrections back; this port keeps every one of them, and its
differential test holds them against the server as the reference's does:

* `!=` of two composites is `NOT (a1 = b1 AND a2 = b2)`, not
  `NOT (a1 != b1 AND a2 != b2)`, by which a composite is unequal to itself.
* `all` means "no item fails", written `NOT any(NOT p)`, so the tree has no
  second quantifier.
* SQL parentheses follow associativity: `a - (b - c)` stays so, and
  `(a = b) = c` is not written `a = b = c`, which PostgreSQL does not parse.
* `IS` compiles to `IS NOT DISTINCT FROM`; `x IS $1` is a syntax error.
* The predicate of a relational collection is parenthesised after its keys;
  `fk AND p OR q` selects rows of other parents.
* A collection of the candidate named inside another collection's predicate
  joins to the root row; a column of the candidate there is qualified with
  the candidate's row, which the schema names - without a schema it is
  `Pg.No_table`.
* The item of an enclosing collection is named from an inner predicate by how
  far out it is, `Path.outer 1 "limit"`: the evaluator keeps the items of the
  enclosing predicates, the compiler their aliases.
* A template's placeholders are bound in the order they stand; mixing the
  styles is a syntax error, as it is in Python's `%`.
* A null is tested, not compared, where a notation has null for a value
  (ADR-0019): `@.a == null` of a template, spelled out or bound, and `eq` of a
  typed term with a null constant are `IS NULL`, `!=` is `IS NOT NULL`. Written
  into the tree as they stand they are `a = NULL`, true of nothing once nulls
  follow SQL - which is what the tree built by hand still means.
* The template grammar is closed: what the sources skip over "if present" is
  required or refused. A placeholder's letter is checked: `%d` takes an
  integer, `%f` a number, `%s` anything. An unbound placeholder is an error.
* The evaluator is three-valued throughout, and `AND`, `OR` and `any` stop as
  soon as they are decided.
* Arithmetic is PostgreSQL's and checked: no wrapping, no `inf`, truncating
  integer division; booleans are ordered, false first; a float result too
  small to be one is out of range as one too large is; a NaN divided by zero
  is a NaN.
* A schema is the foreign keys of the storage, as `\d` shows them, and
  nothing of any aggregate or query. A tree names a collection by its table,
  and where two keys of that table reference the row it is named from, by the
  key's name; an object kept in a table of its own by the key's column; a row
  of an array by the array's column, `"stores.items"`.
* A mapping is of the aggregate's members and knows nothing of any query: it
  is asked about a member by its whole path from the candidate, a collection
  being a member like any other, and `transform` puts the answer where the
  member was.
* A name is written into the query between double quotes, as it is, and one
  that is not of ASCII letters, digits and `_` is refused (ADR-0020).
* A constant with nothing but constants beside it has its type said in the
  text, `$1::bigint + $2::bigint`; beside a column it is not, and the value
  adapts to the column. The one column that is cast is the count of a shift,
  `"a" << "b"::integer`.
* A string constant beside a value of a kind that has no literal of its own -
  a point in time, a date, a UUID - is read as that kind (ADR-0026), and a
  date is a kind of its own, `Value.Date`, days since the Unix epoch as
  `Timestamp` is microseconds.
* A member is read where the mapping puts it, and a document as its layout
  says (ADR-0027): a `jsonb` column declared in the schema with a `Pg.Layout.t`
  is read by `->>` and a cast or a collation, its arrays with
  `jsonb_array_elements`.
* An equality where only truth counts is containment, where the document has
  a GIN index (ADR-0028); of the value at a path, where the index is on that
  path (ADR-0030).
* An element of a collection of values is the item itself (ADR-0029), and is
  what the mapping says it is from the collection's predicate (ADR-0031) - a
  tenant-scoped identity kept as its key in the array and its tenant in the
  row is compared part by part.
* The typed terms quantify a collection with a boolean term (ADR-0032).
* A collection of a document that is not an array has no items (ADR-0033):
  the array is read through `CASE WHEN jsonb_typeof(...) = 'array'`, so that
  what a row is does not depend on the plan.
* Declared and never used in the sources, so not here: `IN`, `BETWEEN`,
  `ASC`, `DESC`, `PERIOD`; the collection "slice" other than `*`.

## Where this port differs from the reference

* **An integer is `Int64.t`.** OCaml's `int` is 63 bits wide and PostgreSQL's
  `bigint` 64: the two readers must overflow at the same value, and the
  differential test computes `Int64.min_int` and `1 << 63` on both.
* **The comparison builders are `eq`, `ne`, `gt`, `lt`, `ge`, `le`**, not
  `equal`, `not_equal`, `greater_than`: `Ast.equal` is the structural equality
  of trees, which every derived equality in the repository is called.
* **Generic code is a functor or a record of functions.** What evaluation
  needs of a value is the signature `Operand.S` and the evaluator the functor
  `Evaluate.Make`; a candidate is a record of three functions, `Context.t`; a
  mapping a record of two, `Mapping.t`. Nothing is an object or a class.
* **The typed terms' operators live in `Dsl.Infix`**, which redefines OCaml's
  own. An operator of the library's own spelling, `&&:`, would bind as a
  comparison does, and `a ||: b &&: c` would be `(a || b) && c`.
* **A parameter goes to the server as text of no declared type**
  (ADR-0021). The reference's driver asks the server for the types it
  inferred and writes each value in that type; Caqti's driver declares a type
  for every typed field, which beside a column takes away what the compiler
  counts on. So the adapter sends every value as text with no type, and the
  server reads it in the type it inferred. What this gives up: the driver
  does not refuse a text where a number is asked for - `"40"` is read as
  `40` - where the reference's driver refuses it.
* **A text with a NUL is refused by the adapter as well as by the
  compiler**: a C string would end at the NUL in silence.
* **The template's positions count Unicode code points**, as the reference's
  count `char`s; a byte sequence that is not UTF-8 is read as the replacement
  character.
* **A constant of a predicate is a parameter with a type.** The reference's
  macro takes any expression from outside as a value, `limits::DEAR`, and
  Rust's `Into<Value>` converts it; OCaml has no such conversion by type, so
  the ppx converts a parameter by the type it is annotated with, and refuses
  a name that is neither a member nor a parameter. A specification type whose
  fields are the constants is not read: its constants are parameters.
* **`&&` and `||` group to the right** in the tree of a predicate, as OCaml
  groups them, where the reference's group to the left. The connectives
  regroup freely, so the compiled text is the same.
* **`lsr` and `Int64.shift_right_logical` are refused**: PostgreSQL's `>>`
  is arithmetic, `asr`.

## Limits

* A template's tree has at most 128 levels, and a template nests at most 32
  deep - groups, `!`, the filters of collections: a text that is not trusted
  must not be a stack overflow. A chain of `&&` or `||` nests to the left, so
  it has at most 128 operands. A template is at most 262 144 bytes of UTF-8,
  `Jsonpath.max_length`, looked at before anything is read. A tree built by
  hand is as deep as its author made it.
* Text is ordered by code point here and by the column's collation in the
  database; equality agrees, order may not.
* A name is the column's to the letter: `createdAt` is the column created as
  `"createdAt"` and not `createdat`, which is what PostgreSQL makes of the
  word without quotes. Where the domain's names are not the storage's, a
  `Mapping.t` says what they are. A name with a space or a letter beyond ASCII
  cannot be written.
* An object on the way to a member - `@.owner_id.name` - is looked up in the
  schema as a collection is: by the key its name is a column of. Kept in a
  table of its own, it is read through the key, by a subquery in the column's
  place. Not mentioned, it is a composite kept in the item's row,
  `("item_1"."maker")."name"` - a Value Object; one kept as columns with a
  prefix is for the `Mapping.t` to say. From the candidate an object not
  mentioned is a qualifier, `"s"."price"`, so a composite column of the
  candidate's own row is declared, `Pg.Schema.composite "stores" "address"`,
  and read as one, `("s"."address")."city"`. A null test of a declared
  composite is of the value as a whole, `IS DISTINCT FROM NULL`.
* An object kept by a key and not said to be is taken for a composite, and
  PostgreSQL reads a member called like a type it can cast to - `name`,
  `text` - of a column that is no composite as that cast, in silence.
* A value of the domain that the storage keeps as a null - a special case
  that answers for itself, `discount = No_discount` - is tested for where it
  is compared for equality: the `Mapping.t` says it is the storage's null,
  `Mapping.Null`. What stays PostgreSQL's own is a null compared with a
  value: `discount > 10` is unknown to it of such a row, and so is
  `NOT discount > 10`, where the special case answers false and true.
* A Value Object is compared as a whole, by the `Operand.S` its type of values
  implements, and the `Mapping.t` says what it is in the storage; a
  specification does not reach for a number inside it.
* A path from the candidate of several parts is written into the query as it
  is: `s.price` is `"s"."price"`, the author's qualifier.
* An embedded collection is read with `unnest`, an array of a composite or
  of a scalar type, or with `jsonb_array_elements` and
  `jsonb_array_elements_text`, an array of objects or of values in a
  document. An array of arrays is not read: an item itself is a value or an
  object, not a collection.
* An element the domain holds as a composite and the storage keeps in part
  elsewhere - a tenant-scoped identity kept as its local key in the array
  and its tenant once, in the candidate's row - is what the mapping's
  `element` says it is from the collection's predicate,
  `Composite [ Scalar (field "tenant_id"); Scalar itself ]` (ADR-0031): its
  equality with an identity is the tenant compared with the row's and the key
  with the element. A mapping that maps the identity to its key alone
  compares other values than the evaluator - the groups of another tenant
  with the same key - in silence; one that says nothing of the element has a
  composite constant beside it refused, `Mapping.Not_composite`.
* A member of a document that is not in it is the null of SQL: `->>` reads
  a missing member as it reads a JSON null, where the evaluator says it is
  missing. The layout is what the encoding writes; a layout that is not makes
  a query that selects nothing, in silence. What holds the two together is a
  test of the repository that writes an aggregate and selects it by every
  member the layout declares. A document of an older shape, read into the
  aggregate by an upcast, is the same: it is rewritten before a query goes
  into it.
* A point in time in a document is read at microseconds. The server rounds
  what is finer - `.999999999Z` is the next second - and the evaluator's
  `Timestamp` has none of it: an encoding that writes nanoseconds and is
  compared at a boundary has the two disagree.
* A collection of a document that is not an array - a JSON null, an object,
  a scalar, a member that is not there - has no items: `any` is false of it
  and `all` true, as of an empty array, where the evaluator refuses such a
  candidate as it refuses a member that is not there. The array is read
  through `CASE WHEN jsonb_typeof(...) = 'array'`: `jsonb_array_elements`
  fails on a scalar, and whether a row reaches it is the plan's to decide -
  the planner evaluates the cheaper operand of an `AND` first - so the same
  query failed under one schema and selected under another (ADR-0033).
* With a GIN index declared, the compiled condition is what goes after
  `WHERE` and nothing else: containment is false where the equality it
  stands for is null, so the text negated, `NOT (...)`, selects the rows where
  the specification is null. A specification is negated in the tree. And a
  member declared an integer, a boolean or a text is a JSON number without a
  fraction, a boolean, a string: an encoding that writes an integer as a
  string, as the JSON mapping of proto3 writes an `int64`, is read by `->>`
  and not contained; one that writes it with a fraction, `100.0`, is
  contained by `100` and fails the cast `->>` is read with. Either way the
  layout is not what the encoding writes, and the repository's test runs
  under the schema it compiles with.
* `numeric` parameters are text like any other, and the server reads them as
  the column's type; a `numeric` constant with nothing beside it is said to
  be a `bigint` or a `double precision`, by the kind of its value.
* The text of a template has one `@`, the nearest item, as RFC 9535 has it;
  the item of an enclosing collection is named in the tree only.
* The null test is of constants, not of data: `@.a == @.b` with both members
  null is null, where RFC 9535 has true. The equality in which null is a value
  is `IS`, which a template cannot spell.
* A control character stands in a string literal only as its escape, as RFC
  9535 has it; raw, it is a syntax error.
* A string constant compared with a point in time, a date or a UUID is read
  as one, as the server reads an untyped parameter by the column (ADR-0026,
  ADR-0015 of the reference): ISO 8601 - a date, or that with a time, a fraction and `Z`
  or an offset, UTC without one - and the canonical UUID in either case; a
  `Value.Date` takes the date of a full timestamp, as the server does. Caqti
  hands the text to the server, which reads it by the column, so what the
  server reads beyond the subset, `'yesterday'`, is an error in memory and a
  row on the server - never the other way round. A `Timestamp` has no zone of
  its own: a string beside a `timestamp` column without zone is read as one
  with, and an offset is applied where the server would drop it.

## Tests

```text
dune test test/specification_rs
TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test \
    dune exec test/specification_rs/test_pg.exe
```

The differential test is skipped without the URL; `docker compose up -d
postgres` starts the database it expects. `test_ppx.ml` holds the trees of
the predicates, the agreement of each function with its tree on the same
candidates, and the errors of what cannot be a tree.
