# ascetic-ddd-ml

Reusable DDD building blocks for OCaml.

A lightweight library providing foundational types and patterns for
Domain-Driven Design in a functional style.

## What's included

- **Core** (`ascetic_ddd`): `Result_ext`, `Decimal`, `Bounded_int`,
  `Entity_id`, `Clock`, `Domain_event`, `Aggregate_root`.
- **Unit of Work** (`ascetic_ddd.unit_of_work`): the abstract
  `Unit_of_work.S` signature plus a Caqti-backed implementation.
- **Session** (`ascetic_ddd.session`, `.session.caqti`, `.session.memory`,
  `.session.composite`, `.session.rest`): the unit of work as an opaque
  handle with one operation, `atomic` — nested scopes as savepoints,
  rollback under cancellation, PostgreSQL through Caqti, a journal-recording
  session for tests, two sessions acting as one, a session over an HTTP
  client whose requests are timed. The successor of `unit_of_work`; see
  [`lib/session/README.md`](./lib/session/README.md).
- **Outbox** (`ascetic_ddd.outbox`): transactional outbox on PostgreSQL
  over the session — a message committed with the state change it
  announces, dispatched in `(xid8, position)` order at least once; slots
  stored with the row, dispatchers without identity sharing a selection
  through locks alone, loops that wait on an error of the moment and stop
  on a defect, an observer in the vocabulary of the protocol model. See
  [`lib/outbox/README.md`](./lib/outbox/README.md).
- **Inbox** (`ascetic_ddd.inbox`): transactional inbox on PostgreSQL over
  the session — idempotent on
  `(tenant_id, stream_type, stream_id, stream_position)`, processed once in
  the transaction that marks it, causal dependencies waited for out of the
  queue and woken by the dependency's mark, failed messages retried with a
  backoff and parked after their attempts, slots by URI or by stream. See
  [`lib/inbox/README.md`](./lib/inbox/README.md).
- **Trace** (`ascetic_ddd.trace`): an observer of the outbox and the inbox
  that records every event as a line of JSON, so a test run can be checked
  against the protocol models. See [`lib/trace/README.md`](./lib/trace/README.md).
- **Bus** (`ascetic_ddd.bus`, `ascetic_ddd.bus.in_memory`,
  `ascetic_ddd.bus.kafka`): scheme-dispatched publish/subscribe over opaque
  wire messages with a key and headers — URI-routed adapters, consumer
  groups, stages of the wire, a messaging bridge, errors as values, an
  `Eio`-based in-memory broker for single-process deployments, and an
  optional Kafka adapter on `kafka-eio`; the outbox and the inbox are
  channels of it. See [`lib/bus/README.md`](./lib/bus/README.md).
- **KMS** (`ascetic_ddd.kms`, `.kms.pg`, `.kms.vault`,
  `.kms.vault.cohttp`): key management for a tenant's keys — a master key
  over a tenant's versioned key-encryption keys, those over data-encryption
  keys, AES-256-GCM with the tenant and, per channel of the bus, the
  derivation context as associated data, rotation, rewrapping,
  crypto-shredding; custody of the key apart from wrapping with it; digital
  signatures and MACs under the tenant's keys; keys in PostgreSQL tables or
  in HashiCorp Vault Transit behind four ports, a cache in front of either,
  keys in memory for the tests of a consumer. See
  [`lib/kms/README.md`](./lib/kms/README.md).
- **DEK** (`ascetic_ddd.dek`, `.dek.pg`, `.dek.envelope`): data-encryption
  keys per resource, versioned, wrapped through the KMS — a store on
  PostgreSQL, ciphers that carry the key version along, the envelope stage
  of the bus, which seals a payload under a key of its own carried in a
  header, so that the outbox and the inbox hold nothing but ciphertext, and
  the sealing of a service's channels, one stage per channel, chosen by the
  row at an inbox of several. See [`lib/dek/README.md`](./lib/dek/README.md).
- **Saga** (`ascetic_ddd.saga`): routing-slip saga pattern for
  long-running workflows with compensation.
- **Specification** (`ascetic_ddd.specification`, `.specification.caqti`,
  `.specification.ppx`): the Specification pattern as a tree over values
  with two readers that agree, an in-memory evaluator and a PostgreSQL
  compiler - nulls, arithmetic, collections in arrays or in tables of their
  own; built by hand, with typed terms, from a JSONPath template, or from an
  OCaml predicate by `let%specification`; a mapping from the domain's names
  and values to the storage's; the parameters to Caqti. See
  [`lib/specification_rs/README.md`](./lib/specification_rs/README.md).
- **Specification, the earlier design** (`ascetic_ddd.spec`): a DSL with
  its own parser, evaluator and SQL translator; another design, kept until
  its users move to `ascetic_ddd.specification`.
- **Encryption** (`ascetic_ddd.encryption`): the earlier crypto-shredding
  primitives (KEK/DEK, forgettable payloads); `kms` and `dek` are their
  successors.
- **Gherkin** (`ascetic_ddd.gherkin`): pure-OCaml `.feature` parser and
  step runner, built on `ocamllex`/`menhir`.

## Decisions

Architecture decisions that shape these blocks are recorded in
[`docs/src/adr`](./docs/src/adr). Those taken first in the Rust reference
implementation say so in their status; the rest are this port's own.

- [ADR-0001](docs/src/adr/0001-serialize-and-encrypt-before-the-outbox.md) — a message is serialized and encrypted before the outbox; the tables hold bytes.
- [ADR-0002](docs/src/adr/0002-structured-errors-in-the-messaging-ports.md) — the messaging ports return structured errors, the subscriber's own type among them.
- [ADR-0003](docs/src/adr/0003-a-session-is-an-opaque-handle-with-one-operation.md) — a session is an opaque handle with one operation, `atomic`.
- [ADR-0004](docs/src/adr/0004-a-failed-message-holds-its-slot-and-is-parked-after-its-attempts.md) — a failed message holds its slot, and is parked after its attempts.
- [ADR-0005](docs/src/adr/0005-a-message-ahead-of-its-dependency-waits-out-of-the-queue.md) — a message ahead of its dependency waits out of the queue, woken by the dependency's mark *(the wait's transaction is amended by ADR-0008)*.
- [ADR-0006](docs/src/adr/0006-a-dispatcher-owns-a-slot-by-locking-its-position.md) — an outbox dispatcher owns a slot by locking its position, and has no identity.
- [ADR-0007](docs/src/adr/0007-the-inbox-takes-a-slot-by-locking-its-row.md) — the inbox takes a slot by locking its row *(the head's read is amended by ADR-0008)*.
- [ADR-0008](docs/src/adr/0008-a-wait-is-a-transaction-of-its-own-under-a-lock-on-the-dependency.md) — a wait is a transaction of its own, serialized with the dependency's mark by a lock on its identity.
- [ADR-0009](docs/src/adr/0009-which-failures-stop-the-loops-which-wait-and-which-park-at-once.md) — which failures stop the loops, which wait, and which park at once.
- [ADR-0010](docs/src/adr/0010-the-bus-carries-wire-messages-and-its-errors-are-values.md) — the bus carries wire messages with a key and headers, and its errors are values.
- [ADR-0011](docs/src/adr/0011-the-outbox-and-the-inbox-as-channels-of-the-bus.md) — the outbox and the inbox as channels of the bus, joined by bridges *(refined by ADR-0025)*.
- [ADR-0012](docs/src/adr/0012-the-kafka-adapter-stands-on-kafka-eio.md) — the Kafka adapter stands on `kafka-eio`, and is optional.
- [ADR-0013](docs/src/adr/0013-envelope-encryption-on-mirage-crypto-behind-signatures.md) — envelope encryption stands on `mirage-crypto`, behind signatures and functors.
- [ADR-0014](docs/src/adr/0014-the-first-key-is-made-in-a-scope-of-its-own.md) — the first key is made in a scope of its own *(carried back to the reference)*.
- [ADR-0015](docs/src/adr/0015-the-connection-pool-outlives-the-loops.md) — the connection pool outlives the loops that take sessions from it.
- [ADR-0016](docs/src/adr/0016-a-subscription-cancelled-is-a-handler-that-is-not-running.md) — a subscription cancelled is a handler that is not running *(carried back to the reference)*.
- [ADR-0017](docs/src/adr/0017-what-a-specification-must-do.md) — what a specification must do.
- [ADR-0018](docs/src/adr/0018-a-specification-is-a-tree-over-its-values-read-by-functions.md) — a specification is a tree over its values, read by functions.
- [ADR-0019](docs/src/adr/0019-a-null-is-tested-not-compared-where-null-is-a-value.md) — a null is tested, not compared, where null is a value.
- [ADR-0020](docs/src/adr/0020-a-name-is-the-storages-quoted-as-it-is-and-mapped-in-one-place.md) — a name is the storage's, quoted as it is, and mapped in one place.
- [ADR-0021](docs/src/adr/0021-a-parameter-goes-to-the-server-as-text-of-no-declared-type.md) — a parameter goes to the server as text of no declared type.
- [ADR-0022](docs/src/adr/0022-a-predicate-function-yields-its-tree-through-a-ppx.md) — a predicate function yields its tree through a ppx.
- [ADR-0023](docs/src/adr/0023-a-tenants-keys-are-one-per-use.md) — a tenant's keys are one per use: the KEK, a signing key per algorithm, a MAC key per purpose.
- [ADR-0024](docs/src/adr/0024-the-bus-key-is-derived-per-channel-on-a-mount-of-its-own.md) — the bus's key is derived per channel on a mount of its own; the adapter makes no key, custody is a port beside, a refusal is read once.
- [ADR-0025](docs/src/adr/0025-the-intake-names-the-row-and-the-stage-is-chosen-by-it.md) — the intake names the row, and at an inbox that receives several channels the stage is chosen by the row.
- [ADR-0026](docs/src/adr/0026-a-string-constant-is-read-as-the-kind-of-the-member-beside-it.md) — a string constant is read as the kind of the member beside it, and a date is a kind of its own.

## Install

```sh
opam install ascetic_ddd
```

Or pin from source:

```sh
opam pin add ascetic_ddd .
```

## Use

```ocaml
(* dune *)
(library
 (name my_domain)
 (libraries ascetic_ddd))
```

```ocaml
open Ascetic_ddd

module Score = Bounded_int.Make (struct
  let min_value = 0
  let max_value = 100
  let name = "Score"
end)
```

## Build

```sh
dune build
dune runtest
```

## Verification

`verify/tla/` holds TLA+ models of the outbox and inbox protocols, of the
inbox's statements against PostgreSQL under READ COMMITTED, of the
composition of the two through a bridge, and of the cancelling of a
subscription of the bus, checked with TLC: only committed messages are
delivered, nothing is passed over, order per URI survives, effects happen
exactly once and exactly when a message is marked, a message waits only for
a dependency not yet processed, every committed message is eventually
processed end to end, and a subscription cancelled is a handler that is not
running and will not be run. Fourteen configurations are mutants that must
fail, so the properties are known to bite. The test suites record what
their outbox and inbox reported as JSON lines, and `check.sh` replays every
recorded run through the model: trace validation, the code following the
protocol on the runs the tests exercise. See
[`verify/tla/README.md`](./verify/tla/README.md).

```sh
./verify/tla/check.sh    # needs Java, tla2tools.jar and Python 3, see the script
```

## Tests

Most tests are in-process and run with no external dependencies. The
outbox, inbox, PostgreSQL session, key management and DEK suites
(`test/outbox/`, `test/inbox/`, `test/session/test_pg.ml`,
`test/kms/test_pg.ml`, `test/dek/test_pg.ml`, `test/dek/test_stage.ml`) are
integration tests against a real PostgreSQL — they are skipped
automatically when `TEST_DATABASE_URL` is not set, so `dune runtest` is
always green out of the box. The Vault Transit suite
(`test/kms/vault/test_vault.ml`) is skipped likewise without
`TEST_VAULT_ADDR`.

### Local run with Docker

A `docker-compose.yml` at the repo root spins up a Postgres 16 instance
on `localhost:55432` (host port chosen to avoid colliding with a system
PG on the default 5432; user `test`, password `test`, database `test`,
ephemeral `tmpfs` storage):

```sh
docker compose up -d
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
export TEST_KAFKA_BROKERS=localhost:59092   # only where kafka-eio is installed
export TEST_VAULT_ADDR=http://localhost:58200 TEST_VAULT_TOKEN=test-root-token
dune runtest
docker compose down
```

The same file starts a Redpanda broker on `localhost:59092` for the tests of
the optional Kafka adapter; without `kafka-eio` installed they are not
built, and without `TEST_KAFKA_BROKERS` they are skipped. It also starts a
HashiCorp Vault dev server on `localhost:58200` for the tests of the Vault
Transit adapter, which enable the `transit` engine themselves.

### Continuous integration

`.github/workflows/test.yml` runs the full suite on every push to `main`
and on pull requests. It:

1. Starts a `postgres:16` service container with the same credentials as
   `docker-compose.yml`, and a Vault dev server.
2. Installs `libpq-dev` (needed by `caqti-driver-postgresql`).
3. Sets up OCaml 5.4 via `ocaml/setup-ocaml@v3`.
4. Runs `opam install . --deps-only --with-test`, then `dune build` and
   `dune runtest` with `TEST_DATABASE_URL` and `TEST_VAULT_ADDR` pointing
   at the service containers.

## License

MIT — see [LICENSE](LICENSE).
