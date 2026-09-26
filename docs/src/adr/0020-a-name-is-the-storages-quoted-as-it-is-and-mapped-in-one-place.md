# ADR-0020: A name is the storage's, quoted as it is, and mapped in one place

## Status

Accepted (2026-09-26).

## Context

The reference's compiler once wrote a name into the query as it stood, and
PostgreSQL reads a word it knows as what it knows. `field "user"` compiled to
`user = $1`, which parses, compares the user of the session, and selects
other rows than the evaluator is satisfied by: requirement 16 of ADR-0017
broken in silence. `order` did not parse. The reference measured on
PostgreSQL 14: of its 100 reserved words, 14 are read so in silence and 86
are syntax errors. Which words these are is a property of the server asked,
and an open-source library does not know the servers of its users.

The same question has a second half. In the Python and Go sources the entry
point that takes a mapping took no schema, and the one that takes a schema
no mapping; and Go's generator compiled a predicate under the names of Go's
fields, which found the column `price` only because PostgreSQL folds an
unquoted word, and only a word of one part.

## Decision

1. **Every name is written between double quotes, as it is**, a quote of its
   own doubled; there is no list of words. The alphabet of names - ASCII
   letters, digits, `_` - stays, so neither guard rests on the other. A name
   is quoted where it is written, `Pg_identifier`: a `Pg.Schema.t` keeps its
   names as given, and the aliases the compiler makes are quoted like any
   name.
2. **A name in the tree the compiler is given is the storage's name.** What a
   member of the domain is called in the storage is said by the `Mapping.t`,
   and nowhere else; the compiler has no option for names. A naming
   convention, if one is wanted, is a mapping.
3. **A query is compiled by what knows the table**: the repository, which
   applies `Mapping.transform` with its mapping and then `Pg.compile` with
   its schema. The schema names a collection as the mapping left it. A
   frontend yields the tree and no SQL.

Held by `test_pg.ml` (columns `"user"`, `"order"`, `"createdAt"` on a live
server), `test_pg_compile.ml` (a name outside the alphabet, a quote inside
one) and `test_mapping.ml` (a mapping and a schema given together).

## Objections considered

1. *A column named `createdAt` and compiled without a mapping no longer finds
   a folded column.* It fails with "column does not exist", where it used to
   depend on folding; the mapping says `created_at`, once.
2. *A mapping written as a list of members is a list to maintain.* It is
   also the list of what a specification may filter by, which matters for a
   tree that arrives as data: a member the mapping does not know is its error,
   not a column that happens to exist. A mapping by convention maps every
   name; the library ships none.
3. *A name with a space or a letter beyond ASCII cannot be written.* Lifting
   the alphabet is a decision of its own: a dot in a table's name is a
   separator, and the adapters of this repository quote identifiers the same
   way.

## Consequences

- The text of a query is longer by two characters for each name.
- The session's own `Identifier` module in `ascetic_ddd.session.caqti` and
  `Pg_identifier` here quote the same alphabet; the specification library
  keeps its own so as to depend on nothing.

## Alternatives rejected

**Refuse reserved words.** Needs the list, and forbids a column named
`order`.

**Quote reserved words only.** Keeps the text of other queries unchanged, and
a list gone stale fails in silence on the very words that are dangerous.

**An option of the compiler for how a name becomes a column's.** A second
place for what the mapping already says, and a rule from name to name cannot
say `owner.name -> owner_name` or `id -> three columns`.
