(lang dune 3.16)

(name ascetic_ddd)

(generate_opam_files true)

(version 0.1.0)

(license MIT)

(authors "Ivan")

(maintainers "Ivan <emacsway@gmail.com>")

(source
 (uri "git+https://github.com/krew-solutions/ascetic-ddd-ml.git"))

(homepage "https://github.com/krew-solutions/ascetic-ddd-ml")

(bug_reports "https://github.com/krew-solutions/ascetic-ddd-ml/issues")

(package
 (name ascetic_ddd)
 (synopsis "Reusable DDD building blocks for OCaml")
 (description
  "A lightweight library providing foundational types and patterns for Domain-Driven Design in a functional style: bounded integers, decimals, entity IDs, clocks, domain events, aggregate roots, unit-of-work (with a Caqti-backed implementation) and its successor, a session with nested transaction scopes over PostgreSQL or an HTTP client, a transactional outbox and inbox on PostgreSQL over that session with a JSON trace recorder for checking test runs against the TLA+ models, a scheme-dispatched message bus with an in-memory adapter, key management for envelope encryption over PostgreSQL or HashiCorp Vault Transit, data-encryption keys per resource with a sealing stage for the bus, a specification DSL, GDPR-friendly encryption primitives, and a pure-OCaml Gherkin runner.")
 (depends
  (ocaml
   (>= 5.4.0))
  (dune
   (>= 3.16))
  (ptime
   (>= 1.2))
  (uuidm
   (>= 0.9))
  (fmt
   (>= 0.9))
  (re
   (>= 1.11))
  (yojson
   (>= 1.4))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (logs
   (>= 0.7))
  (caqti
   (>= 2.1))
  (caqti-eio
   (>= 2.1))
  (caqti-driver-postgresql
   (>= 2.1))
  postgresql
  (mtime
   (>= 2.0))
  (base64
   (>= 3.0))
  eqaf
  menhir
  ppx_deriving
  alcotest
  (mirage-crypto
   (>= 1.0))
  (mirage-crypto-rng
   (>= 1.0))
  (cohttp-eio
   (and
    (>= 6.0)
    :with-test))
  (qcheck
   (and
    (>= 0.21)
    :with-test)))
 (depopts kafka-eio cohttp-eio))

(using menhir 3.0)
