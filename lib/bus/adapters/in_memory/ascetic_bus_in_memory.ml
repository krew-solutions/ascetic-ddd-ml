(** Process-local adapter for [ascetic_ddd.bus].

    {!In_memory} implements {!Ascetic_bus.Bus.Adapter} on top of [Eio.Stream] — one
    dispatch fiber per topic, consumer groups with fan-out across groups and a single
    consumer per group. *)

module In_memory = In_memory
