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
[ADR-0021](../../docs/src/adr/0021-a-parameter-goes-to-the-server-as-text-of-no-declared-type.md)
and
[ADR-0022](../../docs/src/adr/0022-a-predicate-function-yields-its-tree-through-a-ppx.md).

Three libraries: `ascetic_ddd.specification` is the tree, its values, the
evaluator, the typed terms, the templates, the mapping and the PostgreSQL
compiler, and depends on nothing; `ascetic_ddd.specification.caqti` sends a
compiled query's parameters through Caqti; `ascetic_ddd.specification.ppx`
is `let%specification`, a predicate function and its tree from one source.
Inside the first, `domain/` is the specification itself - the tree, the
values and the ways to build and to evaluate it, pure, with nothing of any
storage - and `infrastructure/` is a specification on its way to a storage:
the mapping and the compiler, as the reference lays them out.

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
multiplied, `is_null` of a member not declared nullable:

```ocaml
open Ascetic_specification.Dsl

let cheap = Infix.(Number.field "price" - Number.field "discount" < Number.of_int 100)
let listed = Infix.(Boolean.field "active" && is_null (Null_datetime.field "deleted_at"))
let specification = expr Infix.(cheap && listed)
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
collections are. Anything else is a compile error at the place it stands,
and the function stays as it was. The ppx has no types: a member that is an
option is not seen to be one, as a parameter is.

## Two ways to read one

**In memory**: `Evaluate.Make (Value)`, or the same functor over a domain's
own value type, gives `is_satisfied_by` against anything that is a
`Context.t` - a domain object saying which of its members are values, which
objects, which collections; or a `Record.t` of plain data.

**As SQL**: `Pg.compile`, to a condition with numbered parameters; with a
`Pg.Schema.t` for the collections kept in tables of their own. The parameters
go to Caqti through `Ascetic_specification_caqti.Params`:

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

## What maps to what

| Reference (Rust) | here |
| --- | --- |
| `Expr<V>`, an enum | `'v Ast.t`, a variant; a reader is a function that matches on it |
| `Path`, `Root` | `Path.t`, `Path.root` |
| `Operand`, a trait | `Operand.S`, a module signature; `Value` satisfies it, a domain's value type does the same |
| `evaluate`, `is_satisfied_by`, generic in `V: Operand` | `Evaluate.Make (O)`, a functor over the value type |
| `Context<V>`, a trait, `&dyn Context` | `'v Context.t`, a record of three functions |
| `Record<V>` | `'v Record.t`, `Record.to_context` |
| `ast::equal`, `not_equal`, `greater_than`, ... | `Ast.eq`, `ne`, `gt`, `lt`, `ge`, `le`: `Ast.equal` is structural equality, as everywhere in OCaml |
| `dsl::Term<S, N>`, operators by trait | `('s, 'n) Dsl.t`, phantom sort and nullability; named functions, and `Dsl.Infix` |
| `dsl::Number::field`, `NullText::value(None)` | `Dsl.Number.field`, `Dsl.Null_text.value None` |
| `jsonpath::Template`, `Params`, `Slot` | `Jsonpath.Template`, `Params`, `Slot` |
| `null_test::equal`, `throughout` | `Null_test.Make (O)` |
| `Mapping`, a trait with an error type | `('d, 's, 'e) Mapping.t`, a record of two functions |
| `pg::Compiler::new().schema(s).offset(n).compile(e)` | `Pg.compile ~schema ~offset e` |
| `pg::Schema::new("t").alias("a").foreign_key(..)` | `Pg.Schema.(make "t" \|> alias "a" \|> foreign_key ..)` |
| `ToSql for Value`, feature `pg` | `Ascetic_specification_caqti.Params` |
| `#[specification]`, `<name>_ast(params)` | `let%specification`, `<name>_ast params`; a predicate without constants has `<name>_ast` a value |
| `.is_some_and(\|x\| ..)`, `.is_none_or(\|x\| ..)` | `Option.fold ~none:false ~some:(fun x -> ..)`, `~none:true`; or a `match` with `None -> false`, `None -> true` |
| `.iter().any(\|item\| ..)`, `.all(..)` | `List.exists (fun item -> ..)`, `List.for_all` |
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
* An embedded collection is read with `unnest`, so it is an array of a
  composite type. A `jsonb` array is not supported.
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
