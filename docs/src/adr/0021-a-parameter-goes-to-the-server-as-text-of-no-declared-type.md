# ADR-0021: A parameter goes to the server as text of no declared type

## Status

Accepted (2026-09-26).

## Context

The compiler makes every constant a numbered parameter and says a type in the
text only where the server has nothing to infer one from,
`$1::bigint + $2::bigint`; beside a column it says nothing, and the value
adapts to the column. That rests on the driver: the reference's
`tokio-postgres` prepares a statement with no types, asks the server which it
inferred, and writes each value in that type - an integer as `int2`, `int4`
or `int8`, a point in time as a timestamp with zone or without.

Caqti's PostgreSQL driver declares a type for every typed field of a
request: `Int64` is sent as `int8`, `Float` as `float8`, `Ptime` as
`timestamptz`, `Bool` as `bool`; only `String` is sent as `unknown`. A type
declared beside a column takes away what the compiler counts on. Two cases
are in the differential test: `"at" = $1` of a `timestamp` column, which with
`$1` declared `timestamptz` is compared in the session's time zone and finds
no row; and `"a" << $1` with `$1` declared `int8`, which is "operator does not
exist: bigint << bigint", since PostgreSQL shifts by an `integer` and by
nothing else.

## Decision

1. **Every value goes to the server as a text of no declared type**, a Caqti
   `string option` field, which the driver sends as `unknown`: the null as
   none; a boolean as `true` or `false`; an integer as its digits; a float as
   seventeen significant digits, or `NaN`, `Infinity`, `-Infinity`; a text as
   it is; a point in time as `YYYY-MM-DDTHH:MM:SS.ffffffZ`, with ` BC` before
   year 1, from a civil-date algorithm of the adapter's own so that no
   calendar library bounds the range; a span of time as microseconds. The
   server reads each in the type it inferred, as it read the reference's
   binary values in the type it inferred.
2. **The compiler is unchanged**, and the text of every query is the
   reference's to the character: `test_pg_compile.ml` holds it.
3. **A text with a NUL in it is refused by the adapter**, as it is by the
   compiler: PostgreSQL has no such text, and a C string would end at the NUL
   in silence.
4. **The adapter is a library of its own**, `ascetic_ddd.specification.caqti`,
   so that the tree, its readers and the compiler depend on nothing.

Held by `test_pg.ml`: every constant expression of the differential test
goes through the adapter, and so does every row selected; the timestamp
column without zone and the `bigint` count of a shift are cases of their own.

## Objections considered

1. *The driver no longer refuses a value of the wrong kind: `Value.Text "40"`
   where an integer is asked for is read as `40`, where the reference's
   driver refuses it.* It is, and `test_pg.ml` says so beside the case. What
   is lost is a check the compiler does not rely on, and the evaluator never
   had: in memory `"40" = 40` is an error of the operand, and a mismatch is
   caught there or by the server, which refuses a text that spells no number.
   A typed check on the client would be a second reading of the query, by
   the adapter, of what the server reads anyway.
2. *A float written as text may not come back as the same double.*
   Seventeen significant digits always do; the server's `float8in` is exact,
   and the differential test computes `1e-300 * 1e-300`, `f64::MAX * 2` and
   every pair of edges under every operator from text parameters, and reads
   the server's answer against the evaluator's. `NaN` and the infinities are
   spelled as the server spells them.
3. *Text is slower than binary on the wire.* A parameter is parsed once by
   the server, and the queries this library writes are planned; no
   measurement was made and no claim is. What binary would give, a typed
   parameter, is the very thing declined.

## Consequences

- A `numeric` column takes a parameter as any other does, which the reference
  could not write at all.
- A `timestamp` column and a `timestamptz` column are compared as the
  reference compares them: the value adapts to the column.
- `Params.request` builds a one-shot Caqti request, since the text of a query
  depends on the specification (ADR-0019); the parameter tuple is packed
  behind an existential, `Params.R`, and the caller runs it with the
  connection's own `collect_list`, `find` or `exec`.

## Alternatives rejected

**Typed Caqti fields, and a cast in the compiler where they fail.** `<<`
could cast a constant count to `::integer`; the timestamp beside a column
without zone has no such cast - `$1::timestamp` beside a `timestamptz` column
is the same defect the other way - and the text of the query would part from
the reference's, which its tests hold to the character.

**The `postgresql` library directly, whose `exec` takes untyped text
parameters as well.** A second driver in a repository whose sessions,
outbox, inbox and key stores run on Caqti, and a connection of its own
outside the session's transaction.

**Send an integer typed and the rest as text.** `bigint << bigint` fails on
the typed integer, and a rule of which kinds are typed is a rule to
remember.
