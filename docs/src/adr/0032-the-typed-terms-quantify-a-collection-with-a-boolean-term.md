# ADR-0032: The typed terms quantify a collection with a boolean term

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0026, 2026-10-21); ported in code and
written down here on 2026-10-23. Discharges the step ADR-0029 named among its
consequences.

## Context

The typed terms of `Dsl` (ADR-0017, requirement 7) refuse at compile time a
tree that means nothing: a number conjoined, a text multiplied, the null
test of a member not declared nullable. They had no quantifier. A
collection was quantified by leaving them for the tree and coming back,
`of_expr (Ast.any "items" (expr term))`, and at that border nothing was
checked: a number for the predicate compiled, and failed when evaluated, "a
boolean was expected"; `of_expr` takes any tree for the sort it is told. The
item itself of a collection of values (ADR-0029) had no term at all. The
Python and Go sources the terms were ported from have no quantifier either.

## Decision

1. **`Dsl.any source predicate` and `Dsl.all source predicate`**, with
   `any_at` and `all_at` of a `Path.t` as `Ast` has them, the predicate a
   boolean term, nullable or not, the result a `Boolean.t`: never null, as
   `any` and `all` of the tree are.

2. **`Dsl.itself` and `Dsl.outer_itself up`**, and the same in every sort
   module: the item itself, declared of the term's sort as a field is -
   `Text.itself` of the tags, `Null_text.itself` where a tag may be null. A
   member of an item stays a field of a path from it, `Number.field_at
   (Path.item "price")`.

Held by: `test_dsl.ml`, "a collection is quantified by a boolean term", the
trees built against the tree's own and evaluated and compiled. That a
number for a predicate does not type-check is the signature's,
`(boolean, 'n) t -> (boolean, not_null) t`: OCaml has no compile-fail test,
and none is needed where the type says it.

## Consequences

- What the terms check is the sorts of a specification among themselves,
  not against the candidate: `Text.itself` of a collection of numbers is
  the author's declaration, as `Number.field "name"` is.
- An item of an enclosing collection is named by how far out it is,
  `Text.outer_itself 1`, as the tree names it.

## Alternatives rejected

**A collection as a term of its own, quantified with a closure that is
given the item**, `tags |> any (fun tag -> tag = Text.value "sale")`: the
sort of the element said once, at the collection, and the item named as
OCaml names it. The item given to a closure is the item of the nearest
collection, `Item 0`; a term of an outer item, taken into the closure of
an inner collection, would still be `Item 0` there and name the inner
item, in silence - unless the tree carried a mark for each item to be
resolved when the quantifier is built, which it does not. The ppx does
that resolution with the names OCaml gives (ADR-0022), and is the way to
write such a predicate with OCaml's own types.
