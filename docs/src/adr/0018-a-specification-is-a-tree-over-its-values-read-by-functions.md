# ADR-0018: A specification is a tree over its values, read by functions

## Status

Accepted (2026-09-26).

## Context

ADR-0017 says what `lib/specification_rs` must do; this says how, in the
functional style asked of the port. The reference's ADR-0012 answered the two
questions that decide the API - what is a value, what is a specification -
and this port keeps its answers: values are a parameter of the tree, and the
tree is data. What remains is how each of the reference's mechanisms is
spelled in OCaml, and three of those choices are not forced by the language.

## Decision

1. **The tree is a variant, `'v Ast.t`, and a reader is a function that
   matches on it.** A path is a root and names, never empty. Operators are
   grouped in the type by what they need of their operands, so no reader has
   a case for "cannot happen". Precedence and associativity are not in the
   tree: they belong to a notation, and the parser and the SQL compiler each
   have their own. Equality and printing are derived, so a test compares
   trees; the comparison builders are therefore `eq`, `ne`, `gt`, `lt`, `ge`,
   `le`, and `Ast.equal` is what every `equal` in this repository is.

2. **The tree is generic in its values, and the stage of a specification is
   in its type.** A parsed template is a `Jsonpath.Slot.t Ast.t`, a bound one
   a `Value.t Ast.t`: binding is `Ast.try_map_values`, and a tree with
   placeholders cannot reach the evaluator. A domain specification is a
   `'d Ast.t`, a transformed one an `'s Ast.t`.

3. **What evaluation needs of a value is a module signature, `Operand.S`,
   and the evaluator is a functor over it, `Evaluate.Make`.** So is the null
   test, `Null_test.Make`, and the compiler over what it needs of a value,
   `Pg.Make`; `Pg.compile` is the compiler over `Value`. A candidate is a
   record of three functions, `Context.t`; a mapping a record of two,
   `Mapping.t`. Nothing is an object, a class or a mutable field: passive
   dependency injection, as the repository has it everywhere.

4. **An integer is `Int64.t`.** OCaml's `int` is 63 bits wide, PostgreSQL's
   `bigint` 64. The evaluator checks its arithmetic - no wrapping - and must
   refuse at the value the server refuses at; `Int64.max_int + 1` and
   `Int64.min_int / -1` are in the differential test, as is `1 << 63`.

5. **One semantics, PostgreSQL's, for both readers**, held by a differential
   test on a live database. Where OCaml and PostgreSQL disagree - `compare`
   puts a NaN below every float, the server above; `Int64.rem min_int (-1)`
   is a trap in C and 0 on the server - the evaluator does what the server
   does.

6. **The template parser is written by hand, recursive descent, one function
   per rule**, each taking the input left and returning what it read with the
   input left after; what `@` means is an argument. It is not a menhir
   grammar, though menhir is in the repository for the Gherkin runner.

7. **Nothing is mutated across calls.** The compiler takes the count of
   parameters and aliases so far and returns the count after; a lexer's
   buffer is local to one string literal.

## Objections considered

1. *A functor per value type is heavier than a trait bound: every user
   writes `module E = Evaluate.Make (Value)`.* One line, once, and only for a
   value type the library does not know; the templates and the typed terms are
   fixed to `Value` and instantiate it themselves. A first-class module
   argument on every call would be the same dictionary passed by hand each
   time.
2. *`Int64.t` makes every constant `Value.Int 500L` where an `int` would do.*
   It does; `Value.of_int` takes an `int`, and the typed terms and the
   templates take `int`s and floats where a literal is written. The
   alternative, `int` in the tree and `Int64` at the edges, would make the
   evaluator's overflow a different value from the server's, and requirement
   16 of ADR-0017 false by one bit.
3. *menhir is here already, and a generated parser is what OCaml does.* The
   grammar is twelve rules and twenty-three tokens. The reference's errors
   name the position, what was found and what was expected, with a caret,
   and the tests hold every message to the character; menhir's messages
   would be a `.messages` file kept beside the grammar. The bounds on the
   tree's height and the parser's nesting are counted in the rules and are
   what a text that is not trusted is held by; in a generated parser they
   would be a wrapper around it. The hand parser is 260 lines.
4. *A record of functions for `Context.t` builds three closures per object
   the evaluator enters.* It does, as the reference builds a trait object;
   `Record.to_context` is for tests and documents, and a domain object makes
   its context once. No measurement was made, and none is claimed.

## Consequences

- The tree is recursive and so are its readers. A template is refused beyond
  128 levels of tree and 32 of nesting; a tree built by hand is as deep as
  its author made it. OCaml 5 gives a fiber a growable stack, so the bound
  here is for the reader that is not on one.
- A reader of a tree over a domain's own values instantiates a functor once
  and passes nothing after.
- `Path.Item up` is the item of the collection `up` levels out, as the
  reference has it after its amendment of 2026-09-23.

## Alternatives rejected

**Tagless final: a signature with a function per operator, a reader per
implementation.** `transform` rewrites a tree, tests compare trees, a template
is kept between matches: each needs the specification as data, and a final
encoding would reify it for them anyway.

**An object type for the context, `< field : ...; object_ : ...; collection :
... >`.** Structurally typed and open, as the trait object is; but the
repository has no objects, and a record of functions closes over the same
state with less machinery.

**A closed `Value.t` with a tuple variant for composites.** An unbound
placeholder becomes a node the evaluator must refuse at run time, and a Value
Object cannot be a constant of a domain specification - the case the mapping
exists for.
