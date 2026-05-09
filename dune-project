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
  "A lightweight library providing foundational types and patterns for Domain-Driven Design in a functional style: bounded integers, decimals, entity IDs, clocks, domain events, aggregate roots, unit-of-work (with a Caqti-backed implementation), a transactional outbox, a specification DSL, GDPR-friendly encryption primitives, and a pure-OCaml Gherkin runner.")
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
  (caqti
   (>= 2.1))
  (caqti-eio
   (>= 2.1))
  (caqti-driver-postgresql
   (and
    (>= 2.1)
    :with-test))
  menhir
  ppx_deriving
  alcotest
  mirage-crypto
  mirage-crypto-rng
  (qcheck
   (and
    (>= 0.21)
    :with-test))))

(using menhir 3.0)
