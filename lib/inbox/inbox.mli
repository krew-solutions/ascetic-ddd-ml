(** PostgreSQL-backed Transactional Inbox.

    Concrete implementation of {!Inbox_port.S}, with [uow] pinned to
    {!Ascetic_unit_of_work.Caqti_unit_of_work.t}. Application code that
    wants to stay driver-agnostic should depend on
    [Inbox_port.S with type uow = ...] instead of on this module
    directly.

    See [init.sql] in this directory for the schema and a full description
    of the design (idempotency, causal dependencies, partition
    strategies). *)

include
  Inbox_port.S with type uow = Ascetic_unit_of_work.Caqti_unit_of_work.t

val create :
  ?table:string ->
  ?sequence:string ->
  ?partition:Partition_strategy.t ->
  provider:Ascetic_unit_of_work.Caqti_connection_provider.t ->
  unit ->
  t
(** [create ~provider ()] builds an inbox.

    @param table     Defaults to ["inbox"].
    @param sequence  Defaults to ["inbox_received_position_seq"].
    @param partition Strategy used by {!dispatch}/{!run} when
                     [num_workers > 1]. Defaults to
                     {!Partition_strategy.uri}; pass
                     {!Partition_strategy.stream} when ordering follows
                     stream identity rather than URI. *)
