# ADR-0029: An element of a collection of values is the item itself

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0023, 2026-10-21); ported in code and
written down here on 2026-10-23. Discharges the step ADR-0027 named among its
consequences. The measurements below are the reference's, on PostgreSQL
14.24; the port's tests hold on PostgreSQL 16.13.

Amended by ADR-0030: the consequence on a GIN index declared at a path of a
document is discharged - `Pg.Schema.gin_at`.

Amended by ADR-0031: the consequence on an element the domain holds as a
composite and the storage keeps in part in the candidate's row is discharged
- `Mapping.element`; decision 6 is amended: the mapping is asked about an
element, and says the element itself unless it says otherwise.

Amended by ADR-0032: the consequence on the typed terms is discharged -
`Dsl.any`, `Dsl.all`, `Dsl.itself`.

## Context

The commonest array of the target services holds identifiers: the device
groups a policy targets, the subgroups of a device group, the members of a
cohort. A specification could not say "some group a policy targets is G".
A `Path.t` names a member of an item and never the item itself;
`Context.collection` gave contexts, and an element that is a string has
no members to give; a template required a name after `@`; the ppx read the
members of the closure's item.

What the storages do, measured on PostgreSQL 14.24:

- `unnest` of an array of a scalar type names the element by its alias,
  `"tag_1"`, of the array's type; a null element is a null row.
- `jsonb_array_elements_text` gives an element of a document's array as
  text, a JSON null as the null of SQL; an object or an array in it as its
  text, which a layout rules out.
- Containment holds for arrays of scalars, `{"g": ["a", "b"]} @> {"g":
  ["a"]}`, and its element is built of immutable functions,
  `jsonb_object(ARRAY['v'], ARRAY[$1::text]) -> 'v'`; `to_jsonb` and
  `jsonb_build_array` are stable.
- `?` is no equality of an element: it is true of the key of an object, and
  false of a number, `'[1, 2]'::jsonb ? '1'`.

A domain's element is often a type of its own - a tenant-scoped identifier -
and a `Context.t` is a record of functions over a value type: an element
that is a plain string, or a type the domain does not own, has no context to
be made of, and a context for it would say nothing but the value.

## Decision

1. **The item itself is a node of the tree**, `Ast.Item up`: the item `up`
   collections out, JSONPath's `@` alone. Built by `Ast.itself` and
   `Ast.outer_itself up`. A member of the item stays a `Field`, `Ast.item
   "price"`. The port keeps `Ast.item name` for the member, as it had it, and
   names the node's builders `itself`: a name the Rust reference has free,
   `ast::item()`, is taken here.

2. **`Context.collection` gives `Context.element`s**: `Object context`,
   whose members a path names, or `Value v`, by value, as `Context.field`
   gives a value. One question, one function.

3. **The evaluator** reads `Item` of a value, and refuses it of an object,
   `Context.Not_a_value "items[*]"`; a member of a value is `Not_an_object
   "tags[*]"`; outside a collection, `No_current_item`.

4. **A template** reads `@` alone in the predicate of a collection as the
   item itself, `$.tags[*][?@ == %s]`, and bound to a null as the null test.
   `@` alone of the candidate, `$` alone, and `@[*]` are refused.

5. **The ppx** reads the closure's item named alone as the item itself,
   `fun tag -> tag = x`; `List.mem x collection` as `any` of the equality,
   with `x` seen from inside the collection, so an enclosing item is one
   further out; and `Option.fold` or a `match` of an element. `List.memq` is
   refused, as `==` is: physical equality has no meaning in a specification.

6. **`transform`** does not ask the mapping about an element: the element is
   what the collection holds, where the mapping put the collection; a
   constant beside it is the mapping's `value`, and a composite one there is
   `Mapping.Not_composite`. Amended by ADR-0031.

7. **The compiler** writes the element of an array of a scalar type as its
   `unnest` alias; of a document's array of values, declared
   `Pg.Layout.values name kind`, as the text of `jsonb_array_elements_text`
   read as its kind, `("device_group_1" COLLATE "C")`, `"rank_1"::bigint`.
   The item itself of a row of a table, or of an object of a document, is
   `Pg.Not_a_value`; a member of a value of a document is
   `Undeclared_member`. Where the document has a GIN index (ADR-0028), the
   equalities of an element with constants of a text, an integer or a
   boolean are what its array contains, beside the `EXISTS`.

Held by: the evaluator in `test_specification.ml`, templates in
`test_jsonpath.ml`, `transform` in `test_mapping.ml`, the ppx in
`test_ppx.ml` (the trees, and each function and its tree agreeing on every
candidate), the text of each storage and each refusal in
`test_pg_compile.ml`; on a live server in `test_pg.ml`, the stores' tags -
an array of text, and an array of strings in their documents - selected by
thirteen specifications, nulls among the elements and the constants, in each
of the six storages against the evaluator, and the GIN index used with the
pattern of an element folded.

## Consequences

- Breaking: an exhaustive `match` on `Ast.t` outside the library stops
  compiling; every `Context.t` gives `collection` anew, an object as
  `Context.Object item`. `Pg.error` has a new variant.
- An element the domain holds as a composite and the storage as a part of
  it - a tenant-scoped identifier kept as its local key, its tenant in the
  candidate's row - is given by the domain's `Context.t` as the part, and is
  queried within its tenant. A mapping that maps a composite constant to a
  part compares other values than the evaluator, in silence. Saying where
  each part is kept is ADR-0031.
- The services index an array by an expression, `GIN ((document ->
  'subgroups'))`, which containment of the whole document does not use. A
  GIN index declared at a path of a document is ADR-0030.
- `List.mem` is of lists alone in OCaml, where Rust's `.contains` is of a
  text as well: the consequence the reference records, a substring taken for
  `any`, has no counterpart here. `String.contains` is refused by the ppx as
  it was.
- The typed terms of `Dsl` have no quantifier, of objects or of values:
  ADR-0032.
- An array of arrays is not read.

## Alternatives rejected

**A `Path.t` without names for the item itself.** Every reader that takes a
path apart would do something with an empty one, silently; the mapping
would be asked about an element by the path of its collection. A node of
its own makes each reader say what it does with it, and the compiler showed
which.

**A second function beside `collection`, `values name`, with the evaluator
trying one and then the other.** Two functions for one question, and a
decision taken on an error.

**A `Context.t` for each element.** A record of three functions that answer
`Missing` to every name, made for a string: nothing of the element is in it
but the element.

**Renaming `Ast.item name` to free the name for the node.** Every test, the
README and the reference's own `Path::item` spell the member of the item
that way; `itself` names the node as the reference's prose does, and the
compiler's and evaluator's own functions for it are called so there too.

**`?`** for an element of a text array of a document: true of a key, false
of a number, and not served by `jsonb_path_ops`.
