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
  `.session.composite`): the unit of work as an opaque handle with one
  operation, `atomic` — nested scopes as savepoints, rollback under
  cancellation, PostgreSQL through Caqti, a journal-recording session for
  tests, two sessions acting as one. The successor of `unit_of_work`; see
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
- **Bus** (`ascetic_ddd.bus`, `ascetic_ddd.bus.in_memory`):
  scheme-dispatched publish/subscribe over opaque wire payloads —
  URI-routed adapters, consumer groups, and an `Eio`-based in-memory
  adapter for single-process deployments. See
  [`lib/bus/README.md`](./lib/bus/README.md) for usage.
- **Saga** (`ascetic_ddd.saga`): routing-slip saga pattern for
  long-running workflows with compensation.
- **Specification** (`ascetic_ddd.spec`): specification-pattern DSL with
  parser, evaluator and SQL translator.
- **Encryption** (`ascetic_ddd.encryption`): GDPR-friendly crypto-shredding
  primitives (KEK/DEK, forgettable payloads).
- **Gherkin** (`ascetic_ddd.gherkin`): pure-OCaml `.feature` parser and
  step runner, built on `ocamllex`/`menhir`.

Architecture decisions that shape these blocks are recorded in
[`docs/src/adr`](./docs/src/adr).

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
inbox's statements against PostgreSQL under READ COMMITTED, and of the
composition of the two through a bridge, checked with TLC: only committed
messages are delivered, nothing is passed over, order per URI survives,
effects happen exactly once and exactly when a message is marked, a message
waits only for a dependency not yet processed, and every committed message
is eventually processed end to end. Ten configurations are mutants that
must fail, so the properties are known to bite. The test suites record what
their outbox and inbox reported as JSON lines, and `check.sh` replays every
recorded run through the model: trace validation, the code following the
protocol on the runs the tests exercise. See
[`verify/tla/README.md`](./verify/tla/README.md).

```sh
./verify/tla/check.sh    # needs Java, tla2tools.jar and Python 3, see the script
```

## Tests

Most tests are in-process and run with no external dependencies. The
outbox, inbox, bridge and PostgreSQL session suites (`test/outbox/`,
`test/inbox/`, `test/trace/`, `test/session/test_pg.ml`) are integration
tests against a real PostgreSQL — they are skipped automatically when
`TEST_DATABASE_URL` is not set, so `dune runtest` is always green out of the
box.

### Local run with Docker

A `docker-compose.yml` at the repo root spins up a Postgres 16 instance
on `localhost:55432` (host port chosen to avoid colliding with a system
PG on the default 5432; user `test`, password `test`, database `test`,
ephemeral `tmpfs` storage):

```sh
docker compose up -d
export TEST_DATABASE_URL=postgresql://test:test@localhost:55432/test
dune runtest
docker compose down
```

### Continuous integration

`.github/workflows/test.yml` runs the full suite on every push to `main`
and on pull requests. It:

1. Starts a `postgres:16` service container with the same credentials as
   `docker-compose.yml`.
2. Installs `libpq-dev` (needed by `caqti-driver-postgresql`).
3. Sets up OCaml 5.4 via `ocaml/setup-ocaml@v3`.
4. Runs `opam install . --deps-only --with-test`, then `dune build` and
   `dune runtest` with `TEST_DATABASE_URL` pointing at the service
   container.

## License

MIT — see [LICENSE](LICENSE).
