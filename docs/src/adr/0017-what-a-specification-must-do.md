# ADR-0017: What a specification must do

## Status

Accepted (2026-09-26).

## Context

`lib/specification_rs` ports the specification crate of the Rust reference
implementation, which is itself a port of `ascetic_ddd/specification` in
Python and `asceticddd/specification` with `cmd/specgen` in Go. A word-for-word
port was not asked for, so what is carried over had to be said first: the
capabilities, not the modules. The reference said it once (its ADR-0011) after
running the Python and Go sources and listing where they were wrong; this
port takes that list as read and holds the reference's tests as its own.

This repository already has a library called specification,
`ascetic_ddd.spec` in `lib/specification`: another design, with a text
syntax of its own, function calls, `IN`, many-to-many relations, a truthiness
in which `0` and `""` are false, and no null logic. The reference named it as
a source it rejected. It stays where it is, untouched, until its one user
moves; this library goes beside it.

## Decision

The library must do the following. Each line names the test that holds it.

**The tree** (`test/specification_rs/test_specification.ml`)

1. A specification is a tree of: a constant; the value of a member reached
   from the candidate, or from the item under test, through nested objects;
   a prefix operator (`NOT`, unary `-`), an infix one (`= != > < >= <=`, `IS`,
   `AND`, `OR`, `+ - * / %`, `<< >>`), a postfix one (`IS NULL`,
   `IS NOT NULL`); "some item of a collection satisfies a predicate", nested
   to any depth, the item of an enclosing collection named by how far out it
   is.
2. It can be built by hand, several operands of `AND`/`OR` nesting to the
   left, and compared with another tree for equality.

**Evaluation** (`test_specification.ml`, `test_pg.ml`)

3. A tree is evaluated against a candidate that gives, by name, a value, a
   nested object, or the items of a collection; a member that is not there
   is an error. A candidate made of plain data is provided.
4. Nulls follow SQL: null operands make null results; `AND`, `OR`, `NOT` are
   three-valued; `IS`, `IS NULL`, `IS NOT NULL` and the collection predicate
   are never null; a candidate satisfies what evaluates to true.
5. Values are booleans, 64-bit integers, floats, text, points and spans of
   time - a point less a point is a span, a point and a span make a point -
   and any type of the domain's own that says how it compares and computes.
6. An operator applied to what it is not defined for, a division by zero and
   an overflow are errors that name the operator and the operands.

**Typed terms** (`test_dsl.ml`)

7. Terms declared boolean, number, text or datetime, nullable or not, offer
   only the operators of their sort, and build the same tree.

**Templates** (`test_jsonpath.ml`)

8. A specification is parsed from a JSONPath filter: `$[?p]` on the
   candidate, `$.a.b[*][?p]` on a collection, the same nested inside `p`;
   `== != < <= > >=`, `&&` over `||`, `!`, parentheses; numbers, strings,
   `true`, `false`, `null`.
9. Placeholders `%s %d %f` and `%(name)s ...` stand for values; a template is
   parsed once and bound to positional or named parameters for each match.
10. A syntax error gives its position, what was expected, and the template
    with a caret. Text outside the grammar is refused, not read as something
    else; parameters that do not fit are refused, not left unbound. A
    template that is not trusted is bounded in length, height and nesting.

**Host-language predicates** (`test_ppx.ml`)

11. A predicate written as an ordinary function yields the tree of the same
    predicate without being written twice, the function staying the native
    check: comparisons, logic, arithmetic, nested members, `exists`/`for_all`
    over collections nested to any depth, null tests, what an option holds
    under a name, the equality a Value Object compares by.
12. What cannot be a tree is reported where it stands, not replaced by a
    null.

**From the domain's terms to the storage's** (`test_mapping.ml`)

13. A mapping says what each member and each value becomes; one of either
    may become several, a composite, nested if need be. Equality of two
    composites becomes the conjunction of the equalities of their parts,
    inequality its negation; shapes that differ and other operators on
    composites are errors. A value the storage keeps as a null is tested for
    where it is compared for equality.

**SQL** (`test_pg_compile.ml`, `test_pg.ml`)

14. A tree compiles to a PostgreSQL condition with numbered parameters,
    numbered from a given offset if asked, parenthesised so that the text
    means what the tree means.
15. A collection predicate is `EXISTS` over `unnest` of an embedded
    collection, or over a child table joined by a simple or composite
    foreign key, as a schema says; aliases are distinct and readable; nested
    collections join to the enclosing item.
16. A row is selected by the compiled condition exactly when its object
    satisfies the tree in memory; a constant expression has the same value,
    or the same error, in both readers.

## Objections considered

1. *Requirements 11 and 12 name a capability whose mechanism differs by
   language - a procedural macro in Rust, a ppx here - so a port of them is
   not a port.* The reference's ADR-0012 already separates the capability
   from its mechanism, and requirement 11 lists what the function may say,
   not how it is read. The ppx is a decision of its own (ADR-0022), and its
   test holds the same predicates the reference's test holds, translated.
2. *The differential test, requirement 16, is what the rest rests on, and it
   needs a database, so it will be skipped and forgotten.* It is skipped
   without `TEST_DATABASE_URL`, as every integration test of this repository
   is, and printed as skipped; it runs in the same way as the outbox's and the
   KMS's. The reference has it behind a feature flag and a separate command,
   which is one more step than here.
3. *Requirement 5 says 64-bit integers where OCaml has 63.* It does so on
   purpose: `Int64.t`, so that both readers overflow at the same value, which
   the differential test computes (ADR-0018).

## Consequences

- The defects the reference found in the Python and Go sources are not
  rediscovered here; the README lists what it established and this port
  keeps, and the differential test holds every item against the server.
- `ascetic_ddd.spec` and `ascetic_ddd.specification` coexist. The name of
  the new library is the plain one, so that when the old one goes nothing is
  renamed.
- The ppx meets requirements 11 and 12 under ADR-0022, with the constants of
  a predicate as typed parameters where the reference takes any value.

## Alternatives rejected

**Extend `ascetic_ddd.spec` to the reference's capabilities.** Its tree has a
placeholder as a node, a call as a node, a relation of three tables in its
schema and Python's truthiness in its evaluator; each is a mechanism the
reference removed for a defect it carried. A port of the capabilities into
that tree would be a rewrite under an old name.

**Port the reference's modules one to one.** Traits, `Box<dyn Context>`,
builder structs and a procedural macro are how Rust gets the capabilities; a
module signature, a record of functions, optional arguments and a ppx are how
OCaml does (ADR-0018).
