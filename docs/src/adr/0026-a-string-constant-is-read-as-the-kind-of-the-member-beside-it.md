# ADR-0026: A string constant is read as the kind of the member beside it, and a date is a kind of its own

## Status

Accepted (2026-10-15). Follows the decision taken first in the Rust
reference implementation (its ADR-0015, 2026-09-30); ported in code on
2026-10-01 and written down here, since the port's convention is a
decision per ported decision. Held by `test/specification_rs/test_specification.ml`
("a string constant is read as the kind of the member beside it", "a date
is days since the Unix epoch"), `test_jsonpath.ml` ("a string is read as
the kind of the member beside it", in either of RFC 9535's quotes) and
`test_pg.ml` ("a string constant is read as the kind of the column beside
it": rows of `timestamptz`, `date` and `uuid` columns against a live
server, with the session's time zone set).

## Context

A template has the literals of RFC 9535, a string, a number, `true`,
`false`, `null`, and no others: a point in time, a date or a UUID in a
template is a string, `@.created_at > '2026-09-01'`. The two readers then
part. PostgreSQL reads an untyped parameter by the type of the column
beside it, which is what ADR-0021 counts on, so the query selects the
rows; the evaluator compared a string with a `Timestamp` and refused.
Requirement 16 of ADR-0017, a row is selected exactly when its object
satisfies the tree in memory, was false for every filter with a date in it.

A date bound as a named parameter agrees in both readers, but in a REST
API that means a typed parameter beside the filter for every date,
declared in the contract: a burden on every client for a value it can
write in the filter. Numbers and booleans have literals of their own, in
the template and in JSON; a string beside a number is the author's choice.

The reference measured on PostgreSQL 14 what `timestamptz`, `date` and
`uuid` read: ISO 8601 with `T` or a space, fractions, `Z` or an offset,
and beyond these `20260901`, `Sep 1 2026`, `yesterday`; a UUID in the
canonical form in either case, without hyphens, in braces.

## Decision

1. **A string compared with a value of a kind that has no literal of its
   own, a point in time, a date, a UUID, is read as that kind by the
   evaluator**, on either side of the operator, under a comparison or `IS`,
   before the comparison, as the server reads an untyped parameter by the
   column: `Operand.S.read_beside`, asked of a constant of the tree and of
   nothing else. A string that does not read is an error, as the server's
   "invalid input syntax". A string beside a number or a boolean stays a
   string, and does not compare. A type of values with no such kinds
   answers that there is nothing to read.
2. **What is read is a subset of what the server reads**, `Reading`, so
   that nothing the evaluator accepts fails on the server: for a point in
   time and a date, ISO 8601, `YYYY-MM-DD`, or that followed by `T` or a
   space and `HH:MM[:SS[.ffffff]]`, then nothing, `Z`, or `±HH:MM`; a date
   takes the date of a full timestamp, as the server does. A string without
   an offset is read as UTC: the server reads it in the session's time
   zone, so the two agree where the session's zone is UTC, and the
   agreement test sets it. For a UUID, the canonical `8-4-4-4-12` in either
   case.
3. **A UUID is a kind of value**, `Value.Uuid of Uuidm.t`, the library the
   rest of the repository already holds its UUIDs in; the Caqti adapter
   writes it canonically, and a UUID constant with nothing beside it has
   its type said in the text, `$1::uuid`, as a timestamp has (ADR-0021).
4. **A date is a kind of its own**, `Value.Date`, days since the Unix
   epoch as `Timestamp` is microseconds, so that a `date` column is a date
   to the evaluator and not a midnight that would agree with the server on
   a string that is a date alone and part from it on
   `< '2026-09-02T12:00'` in silence. The one civil calendar, days to
   year, month and day and back, lives in `Value.Date`; the Caqti adapter
   writes a date as `YYYY-MM-DD` through it and a timestamp through the
   same arithmetic, so no calendar library bounds the range (ADR-0021).
   The typed terms gain the sorts `date` and `uuid`, each offering the
   comparisons of its sort and nothing else.
5. **The compiler is not touched**: the text of the query is as it was,
   and the server keeps reading the parameter by the column.

## Objections considered

1. *`Uuidm` is one more dependency of a library that depends on nothing.*
   It is a dependency of the package already, for the identifiers of the
   core, and the one UUID type the repository has; a UUID of sixteen bytes
   of the library's own would be a second type every user converts to and
   from, which is what the reference refused in taking the `uuid` crate.
   The tree, its readers and the parser still depend on nothing of the
   database.
2. *A civil calendar written by hand, beside `ptime`, invites an error in
   a leap-year rule.* `ptime` bounds its dates to the years 0000 to 9999
   and the server does not; the adapter already carried the arithmetic for
   its timestamp text for that reason (ADR-0021), and now one algorithm
   serves both kinds from one place, `Value.Date.to_civil` and `of_civil`,
   held by the test of days since the epoch at the edges of the Gregorian
   cycle, and by the differential test, where the server writes the same
   date back.
3. *A `Timestamp` has no zone, so a string beside a `timestamp` column
   without zone is read as one with, and an offset is applied where the
   server would drop it.* It is the divergence the reference lists too,
   since only Python has a time without a zone; the README says so, and a
   string without an offset, the commonest filter, agrees in a session
   whose zone is UTC, which the agreement test sets and a deployment
   should.
4. *Reading a string by the kind beside it makes the evaluator's answer
   depend on the candidate's type, not on the tree.* It depends on the
   kind of the member, which the storage fixes, as the server's answer
   depends on the column's type: the reading is of constants, which are
   the author's, never of the candidate's data; a member holding a string
   compared with a member of another kind is still an error, and the
   server refuses two columns of those types likewise.

## Consequences

- A filter from a REST client carries its dates and identifiers inline,
  and the same text means the same rows in memory and on the server.
- Outside the subset the evaluator is loud where the server is not:
  `'yesterday'` is an error in memory and a row on the server. Never the
  other way.
- `Value.t` has two more constructors, `Date` and `Uuid`: an exhaustive
  match outside the library stops compiling, which is how a consumer
  learns of them. `Operand.S` has one more function, which a domain's own
  value type answers with `Ok None` when it has no such kinds.
- The Caqti adapter writes a `Date` as `YYYY-MM-DD` and a `Uuid` as its
  canonical text, both as text of no declared type (ADR-0021), so the
  server reads them by the column as it reads every parameter here.

## Alternatives rejected

**Dates as parameters only.** Agrees today, and costs every client a typed
parameter per date beside the filter.

**A typed literal in the template, as OData writes
`2026-09-30T12:00:00Z` bare.** A syntax RFC 9535 does not have; to be
decided with an OData frontend, if one comes, not for this.

**Read a string beside a number or a boolean as well.** They have
literals; a string there is meant, and the server's coercion of `'100'` is
an accident of untyped parameters, not a meaning to copy.

**Copy the server's input syntax.** Unbounded and a property of the
server's version; `'yesterday'` is not a value.

**Require an offset on a point in time.** Agrees in every session zone,
and refuses the commonest filter, `> '2026-09-01'`.

**A date as a midnight `Timestamp`.** Agrees on a string that is a date
alone and parts from the server on a string with a time, in silence: the
one place the reading would have been quiet where the server is not.
