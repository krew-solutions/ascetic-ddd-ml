(** Ascetic DDD — reusable DDD building blocks for OCaml.

    A lightweight library providing foundational types and patterns
    for Domain-Driven Design in a functional style. *)

module Result_ext = Result_ext
module Decimal = Decimal
module Bounded_int = Bounded_int
module Entity_id = Entity_id
module Clock = Clock
module Domain_event = Domain_event
module Aggregate_root = Aggregate_root

(* Sub-libraries [ascetic_ddd.unit_of_work], [ascetic_ddd.session] with its
   [.caqti], [.memory], [.composite] and [.rest] sub-libraries,
   [ascetic_ddd.outbox], [ascetic_ddd.inbox], [ascetic_ddd.trace],
   [ascetic_ddd.bus], [ascetic_ddd.bus.in_memory], [ascetic_ddd.kms] with its
   [.pg] and [.vault] sub-libraries, [ascetic_ddd.dek] with its [.pg] and
   [.envelope] sub-libraries, [ascetic_ddd.saga], [ascetic_ddd.encryption],
   [ascetic_ddd.spec], and [ascetic_ddd.gherkin] are exposed separately; add
   them to the [libraries] field of your dune file individually. *)
