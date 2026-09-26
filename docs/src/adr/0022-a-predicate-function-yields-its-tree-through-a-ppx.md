# ADR-0022: A predicate function yields its tree through a ppx

## Status

Accepted (2026-09-26).

## Context

Requirement 11 of ADR-0017: a predicate written as an ordinary function
yields the tree of the same predicate without being written twice. The
Python source reads the lambda's source at run time with `inspect` and
`ast`; the Go source runs a generator before compilation; the reference has
a procedural macro, `#[specification]`, which reads the function's syntax
tree during compilation and writes `<name>_ast` beside it. OCaml cannot read
a closure's source at run time either, and a generator run before
compilation is what a ppx is, with the compiler's locations for its errors.

Two things the reference's macro leans on are Rust's and not OCaml's. A
constant from outside the function - a parameter, a module constant,
`limits::DEAR` - becomes a value by `Into<Value>`, a conversion chosen by
the type at the use site; OCaml has no conversion by type. And a
specification type whose fields are the constants, read through `self`,
rests on the same conversion for each field.

## Decision

1. **`let%specification name (candidate : t) (p1 : t1) ... = body`** keeps
   the `let` as it was written and writes beside it `name_ast p1 ...`, which
   returns `body` as a `Value.t Ast.t`; a predicate without constants has
   `name_ast` a value. The extension is on the structure item, in
   `ascetic_ddd.specification.ppx`, a `ppx_rewriter` on ppxlib, with the
   tree library as its runtime dependency.
2. **The constants of a predicate are its parameters, each annotated with its
   type**: `int`, `int64`, `float`, `string`, `bool`, `Value.t`, or an
   `option` of one. The annotation is what the ppx converts by; a parameter
   without one, and a name that is neither a member nor a parameter, are
   refused where they stand. A specification record whose fields are the
   constants is not read.
3. **The body is the reference's subset, in OCaml's spelling.** Members of
   the candidate and of the item by field access; the comparison and
   arithmetic operators, and the same operations as `Int64.add`,
   `Int.rem`, `Float.neg`; `String.equal` and `Int64.equal` as the equality
   a Value Object compares by; `Option.is_none`, `Option.is_some`, `= None`,
   `Some x`; `Option.fold ~none:false ~some:(fun held -> ...)` and the
   two-armed `match` on an option, which are `is_some_and` and `is_none_or`;
   `List.exists` and `List.for_all` with a `fun`. Equality goes through
   `Null_test` at run time (ADR-0019), so a parameter of an option type that
   is none makes the null test. An order with `Some _`, `None` or an option
   parameter is refused, as the reference refuses it: OCaml has a none below
   every `Some`, the storage a null that is neither.
4. **What cannot be a tree is an error at the place it stands, and the
   function stays.** The expansion is the original binding and either the
   tree or an `[%%ocaml.error]` with the location of the offending
   expression, so that the one error reported is the right one, not followed
   by "unbound value name_ast" for every use.

## Objections considered

1. *Annotating every constant's type is a burden the reference does not
   have.* A parameter of a predicate is annotated in idiomatic OCaml more
   often than not, and the alternative - a conversion the ppx cannot choose
   - is a tree that does not type-check, reported at the generated code and
   not at the parameter. The test holds a parameter without an annotation to
   its message.
2. *`&&` groups to the right in OCaml and to the left in Rust, so the trees
   differ from the reference's.* They do, and the test's expectations say so;
   the connectives regroup freely in the compiler, so the query is the same
   text, and the evaluator's early stopping is the same in either grouping.
3. *A `match` in a predicate is a general construct, and reading two of its
   shapes as the null test invites a third to be read wrongly.* Only the
   exact two are read - `None -> false | Some x -> e` and `None -> true |
   Some x -> e`, either order, no guards - and any other `match` is refused
   with a message that names them. `Option.fold` with a literal `~none` is
   the same pair.
4. *Tests of a ppx's errors are usually expect tests of the driver's
   output.* Here the translator is a plain function, `Translate
   .structure_items`, and the test parses each source with ppxlib's parser,
   expands it and reads the error node's message: twenty cases, in the same
   executable as the trees.

## Consequences

- `test_ppx.ml` holds the trees of the reference's predicates, translated;
  the agreement of each function with its tree on the same candidates, over
  every value of every parameter the reference tries; the compiled queries;
  and the errors.
- A domain with `int` fields writes `u.age >= 18` and the tree has
  `Value.of_int 18`; with `int64` fields, `18L` and `Value.Int 18L`. Both are
  `bigint` to the compiler.
- `lsr` and `Int64.shift_right_logical` are refused: PostgreSQL's `>>` is
  arithmetic, which is `asr`.
- `ppxlib` is a dependency of the package.

## Alternatives rejected

**An attribute, `let f ... = ... [@@specification]`.** ppxlib's
`Deriving` is for types; an attribute on a value binding would be a
whole-structure pass that looks for it, and the extension point is what
ppxlib gives a `let`.

**Read the constants without types and let the generated code fail.** The
error would point into generated code the author never sees.

**A specification record, `let%specification is_satisfied_by (spec : t) (s
: store) = s.price > spec.min`.** Its fields' types are in the type
declaration, which the ppx does not see; a record of typed parameters is
the same thing written where the ppx can read it.
