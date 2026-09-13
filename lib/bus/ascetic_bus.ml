(** Scheme-dispatched message bus.

    {!Bus} is the transport-agnostic dispatcher: a per-instance registry of adapters keyed
    by URI scheme. Concrete transports are separate sub-libraries that implement
    {!Bus.Adapter} — see [ascetic_ddd.bus.in_memory] for the process-local one. Usage is
    described in [README.md]. *)

module Bus = Bus
