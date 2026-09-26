# ADR-0019: A null is tested, not compared, where null is a value

## Status

Accepted (2026-09-26).

## Context

ADR-0018 gave the evaluator PostgreSQL's semantics, so `a = NULL` is null in
both readers and true of nothing. That is right for the tree, and it takes a
capability away from the notations in which null is a value.

A JSONPath template finds a null by `@.deleted_at == null`; RFC 9535 has null
for a value like any other, and the grammar has no `IS NULL` to write
instead. A typed term compares with a nullable constant,
`eq email (Null_text.value None)`, that is none or not only when the term is
built. Written into the tree word for word, each finds nothing, in memory and
in the database alike, and says nothing of it. The reference met this in its
Python port first: carrying the null logic back broke two of its tests, which
had matched a `None` by Python's own equality.

## Decision

1. **The frontends of notations in which null is a value build the null
   test.** `left = right` with the null constant on either side is
   `IS NULL` of the other, and `!=` is `IS NOT NULL`: `Null_test.Make (O)`
   gives `equal` and `not_equal` over any value type that knows its null.
   SQLAlchemy reads `column == None` the same way.

2. **Each frontend applies it where its constants become known.** A
   template when it is bound, `Null_test.throughout` over the bound tree - a
   literal `null` and a placeholder bound to null are the same value by then,
   and `Template.expr` stays what was written. The typed terms in `eq` and
   `ne`. The ppx in the code it generates, which runs with the parameters:
   there `None` is the null constant, `Some x` is `x`, and a parameter of an
   option type that is none when the tree is asked for makes the null test
   too, so that `s.closed_at = at` means the same in the function and in its
   tree.

3. **The tree, the evaluator and the compiler are untouched.** `Ast.eq a
   null` built by hand is `a = NULL` and is null. The rule is a frontend's
   reading of its notation, not a meaning of the tree.

## Objections considered

1. *Two spellings of equality is one too many; read every `==` of a template
   as `IS`, which is RFC 9535's equality exactly.* Every equality of every
   template would then compile to `IS NOT DISTINCT FROM`, which PostgreSQL
   does not serve from an index: the reference measured an index scan for
   `a = 5` and for `a IS NULL` and a sequential scan for
   `a IS NOT DISTINCT FROM 5`. The rule chosen compiles to the first two.
2. *The rule is of constants, so `@.a == @.b` with both null is null, where
   RFC 9535 has true.* It is, and it is the one place a template departs from
   the RFC; the equality in which null is a value is `IS`, which a template
   cannot spell. A rule of data would need the evaluator to read `=` as `IS`
   and the compiler to write it so, and the index is lost again.
3. *The text of a query now depends on whether a parameter is null, `a = $1`
   or `a IS NULL`, so a statement cache keyed by the text holds both.* It
   does; two entries for two shapes of a query is what a cache is for. The
   Caqti requests the adapter makes are one-shot for this reason.

## Consequences

- An order with null, `@.a < null`, stays null, which RFC 9535 has as false:
  the same at the top of a filter, and different under `!`.
- `test_jsonpath.ml` holds a template with a null, spelled out and bound,
  against a candidate with a null and one with a value; `test_pg.ml` holds
  the bound templates against the database in both storages of a collection.
- A value of the domain that the storage keeps as a null - a special case
  that answers for itself - is the same rule read by the mapping,
  `Mapping.Null`: `transform` writes the null test where such a value is
  compared for equality, and the null it carries anywhere else.

## Alternatives rejected

**Keep SQL's meaning in the frontends too.** Consistent, and it leaves a
template unable to ask whether a member is null at all.

**Apply the rule in the tree**, so that `Ast.eq a null` is the null test.
The tree would stop being what the compiler writes, and the agreement of the
two readers would be of something other than SQL.
