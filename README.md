# ascetic-ddd-ml

Reusable DDD building blocks for OCaml.

A lightweight library providing foundational types and patterns for
Domain-Driven Design in a functional style.

## What's included

- **Core** (`ascetic_ddd`): `Result_ext`, `Decimal`, `Bounded_int`,
  `Entity_id`, `Clock`, `Domain_event`, `Aggregate_root`, `Unit_of_work`.
- **Specification** (`ascetic_ddd.spec`): specification-pattern DSL with
  parser, evaluator and SQL translator.
- **Encryption** (`ascetic_ddd.encryption`): GDPR-friendly crypto-shredding
  primitives (KEK/DEK, forgettable payloads).
- **Gherkin** (`ascetic_ddd.gherkin`): pure-OCaml `.feature` parser and
  step runner, built on `ocamllex`/`menhir`.

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

## License

MIT — see [LICENSE](LICENSE).
