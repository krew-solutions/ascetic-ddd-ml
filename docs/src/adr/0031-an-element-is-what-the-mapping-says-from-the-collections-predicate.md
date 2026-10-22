# ADR-0031: An element is what the mapping says it is, from the collection's predicate

## Status

Accepted (2026-10-23). Follows the decision taken first in the Rust
reference implementation (its ADR-0025, 2026-10-21); ported in code and
written down here on 2026-10-23. Discharges the step ADR-0029 named among its
consequences; amends its decision 6.

## Context

A policy targets device groups, and a group's identity is a pair, its
tenant and its key: an identity is the pair as one value. The policy's
document keeps the keys in an array and the tenant once, in the row - every
group a policy targets is its tenant's. So an element of the collection is a
composite in the domain and two places in the storage: one of them the
element of the array, the other a column of the candidate's row.

Under ADR-0029 the mapping was not asked about an element. A mapping had
two ways with "the policy targets G", and both were wrong. It could map `G`
to its two parts, a composite that had no parts to be compared with beside
one element: `Mapping.Not_composite`, loud. Or it could map `G` to its key
alone, which compiled and selected, for a group of tenant 3, the policies of
tenant 1 that target a group of the same key - where the evaluator selected
none. The reference measured it on a prototype: `{p1, p4}` against `{}`. An
identity of another tenant arriving in a request finds groups by their key:
the very confusion an identity of two parts is there to prevent.

The reference weighed two designs, from different premises, that differed in
where the answer is said. The first had the mapping answer as it answers
`field`, by paths from the candidate, with a new case of `mapped` to mark a
part kept in the candidate's row, and a rule that a path equal to the
collection's place is the element. The second had the mapping answer from
where the collection's predicate stands, as the tree itself says it there:
the element is `itself`, a column of the row is a path from the candidate.

## Decision

1. **`Mapping.t` has a third function, `element : Path.t -> ('s mapped, 'e)
   result`**, asked with the collection's whole path from the candidate, as
   `field` is. `Mapping.make ~field ~value ()` answers the element itself,
   `Scalar Ast.itself`: a mapping that says nothing is what it was. A record
   has no default field in OCaml, so the default the reference gives its
   trait method is the smart constructor's.

2. **The answer is said from the collection's predicate**, in the tree's own
   terms there: `Ast.itself` is the element, `outer_itself n` and a path from
   it the item `n` collections further out, a path from the candidate the
   candidate's. A tenant-scoped group kept as its key in the array and its
   tenant in the row is `Composite [ Scalar (field "tenant_id"); Scalar
   itself ]`. No new case of `mapped`, and no rule about paths.

3. **`transform` asks it for `Item up`** and puts the answer where the
   element was: every item named in it `up` collections further out, but
   those of a collection inside it; a path from the candidate where it is.
   An equality with a constant mapped to the same parts is the conjunction
   of the equalities of the parts (ADR-0017, requirement 13):
   `"t"."tenant_id" = $1 AND "member_1"::bigint = $2` in the `EXISTS`.

Held by: `test_mapping.ml`, an element of the tenant's members and of the
members of its teams compared part by part, the element two collections out
put where it was, and the null test of a composite refused, as of a member;
on a live server, `test_pg.ml`, "an element kept in part in the candidate's
row is compared part by part": policies of two tenants with groups of the
same key, a group of a third tenant, and a negation, against the evaluator.

## Consequences

- `Mapping.t` has a field more: a record literal written elsewhere stops
  compiling until it says what the element is, or is built by
  `Mapping.make`. Nothing built by `make` changes what it compiles to.
- Two conventions in one record: `field` answers by paths from the
  candidate, which `transform` places; `element` answers from the
  collection's predicate, which `transform` only moves out. The second is
  how the tree is written there, which is where the author of the answer
  stands.
- A mapping that maps an identity to its key alone still compiles and still
  selects the groups of another tenant. The guard against it is the mapping
  saying where the tenant is; the README says so.
- A composite element has no one value to be null: `IS NULL` of it is
  `Unexpected_composite`, as of a composite member.
- The domain's `Context.t` gives the group as the domain holds it, the pair;
  it no longer has to give the storage's part.

## Alternatives rejected

**The answer by paths from the candidate, with a `Candidate` case of
`mapped` to mark a part kept in the row and a rule that a path equal to the
collection's place is the element.** Two concepts to say what the tree
already says: a new case every reader of `mapped` must match, and a path
that means the element only by being equal to another.

**The element given by the domain's `Context.t` as the storage keeps it**,
the key, and the tenant left to the repository's filter (ADR-0029 as it
was). Exact within a tenant, and the knowledge of the storage in the domain;
an identity of another tenant in a specification finds groups by their key.

**An optional field**, `element : (Path.t -> …) option`, so that a literal
without it still compiles. Every reader would unwrap it, and a mapping that
answers `None` and one that answers the element itself would be two
spellings of one thing.
