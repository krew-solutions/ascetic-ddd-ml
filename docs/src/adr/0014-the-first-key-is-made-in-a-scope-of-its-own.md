# ADR-0014: The first key is made in a scope of its own

## Status

Accepted (2026-09-19).

## Context

A tenant's first key-encryption key, and a resource's first data-encryption
key, are made on first contact: the adapter reads, finds nothing, takes
`pg_advisory_xact_lock` on the tenant or the resource, reads again, and
inserts version 1. The lock is what makes two callers meeting a new tenant
at once share one key instead of both inserting version 1 and one failing on
the primary key. Rotation takes the same lock around its read and insert.

An advisory lock of the `xact` kind is held to the end of the transaction
that took it. The reference takes it on the caller's session and opens no
scope of its own, so how long it holds is the caller's doing: to the end of
the caller's transaction where there is one, and to the end of the statement
that took it where there is none, since a statement outside a transaction is
a transaction of its own. In the second case the lock is gone before the
read that follows it, and serializes nothing.

The second case is not hypothetical. The envelope stage reaches the KMS
through a session pool of its own and opens no transaction, deliberately:
the KMS's session is not the data's. So on the path the library itself
recommends, sealing every outgoing message, first contacts are not
serialized, and the loser's failure is the publish's, and with it the
command's transaction.

This was measured on this port, with the adapter written as the reference
writes it. Eight sessions with no transaction open, their connections
already established, draw a key for a new tenant at once, for ten tenants in
turn. Five runs of five failed, on the first tenant, with `duplicate key
value violates unique constraint`. With the making in a scope of its own,
five runs of five passed. Without the connections established first the
test passed either way, the pool connecting on demand and the first caller
finishing before the others had a connection; the test warms the pool for
that reason. The reference itself was read, not run: that it behaves the
same follows from its source and from how PostgreSQL holds the lock.

## Decision

Making a first key, and rotating, run in a scope of the adapter's own:
`Session.atomic` around the lock, the second read and the insert, in
`Pg_kms` and in `Pg_dek_store`.

Inside the caller's transaction the scope is a savepoint. A lock taken in a
savepoint that is released passes to the transaction and is held to its end,
which is what the reference has there and what its tests, ported, still
show: a second transaction waits until the first commits. Where the caller
has no transaction, the scope is one, and the lock holds for as long as the
making does. The first read stays outside the scope, so the common path, a
key that exists, opens nothing.

## Objections considered

1. *The caller should open the transaction; an adapter that opens scopes on
   its own hides what the caller ought to see.* The port's contract is that
   an operation runs "in the caller's session", not in a transaction, and
   the stage is a caller that rightly has none. Requiring one would push the
   fix into every caller and leave the adapter wrong for the one that
   forgets, failing only under a race. A scope is what the session is for,
   and nesting is the operation it is closed under (ADR-0003).
2. *A savepoint on every first contact costs two statements inside
   transactions that did not need them.* On first contact only: once per
   tenant, once per resource, and once per rotation, each of which already
   generates a key and inserts a row. The path taken every other time is one
   read.
3. *An `INSERT ... ON CONFLICT DO NOTHING` and a read would need no lock and
   no scope.* It would make the loser discard a key it generated and read
   the winner's, which works outside a transaction; inside one, under READ
   COMMITTED, the loser blocks on the winner's uncommitted row just as it
   blocks on the lock now, so nothing is gained there. Rotation cannot be
   written that way at all: the next version is read before it is written,
   and two rotations at once must make two versions, which takes a lock. One
   mechanism for both was preferred to two.
4. *If the session is abandoned, or a scope is already open on it, the
   making now fails where it would not have.* A session abandoned cannot
   commit anything, so the key would not have lasted; and a scope already
   open on the session handed in is a defect of the caller that the guard
   reports, `Scope_already_open`, rather than a second scope silently
   sharing the first one's savepoints.

## Consequences

- Two tests beside the reference's: eight callers with no transaction meet a
  new tenant, and a new resource of a new tenant, at once, and make one key.
  They warm the pool first; without that they pass whatever the adapter
  does.
- The envelope stage needs nothing of the session but the pool: the scope is
  the adapter's.
- The same change is proposed for the reference. By its source, a
  deployment of it that seals through the stage can fail a publish, and the
  command's transaction with it, when two commands meet a new tenant at
  once; the command tried again goes through, the key being there by then.
  That was read from the reference, not run on it.
