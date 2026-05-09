(** PostgreSQL-backed Transactional Outbox.

    Concrete implementation of {!Outbox_port.S}, with [uow] pinned to
    {!Ascetic_unit_of_work.Caqti_unit_of_work.t}. Application code that
    wants to stay driver-agnostic should depend on
    [Outbox_port.S with type uow = ...] instead of on this module
    directly.

    See [init.sql] in this directory for the schema and a full description
    of the design (transaction_id ordering, visibility rules, consumer
    groups, URI-based partitioning). *)

include
  Outbox_port.S with type uow = Ascetic_unit_of_work.Caqti_unit_of_work.t

(* Implementation-specific construction. The application layer would
   typically receive an already-constructed [t] from a composition root
   that knows about Caqti; the abstract port intentionally omits this. *)

val create :
  ?outbox_table:string ->
  ?offsets_table:string ->
  ?batch_size:int ->
  provider:Connection_provider.t ->
  unit ->
  t
(** [create ~provider ()] builds an outbox.

    @param outbox_table  Defaults to ["outbox"].
    @param offsets_table Defaults to ["outbox_offsets"].
    @param batch_size    Maximum number of messages fetched per dispatch
                         call. Defaults to [100]. *)
