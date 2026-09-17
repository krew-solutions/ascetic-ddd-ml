(** Observers that record what the outbox and the inbox report as lines of JSON, for
    validation against the protocol models in [verify/tla].

    {!Json_trace} is both an outbox observer and an inbox observer, so one recorder can
    watch an outbox feeding an inbox and keep the order across the two. Each line names
    the observer that reported it, ["observer": "outbox"] or ["inbox"], and otherwise
    carries the event's own fields. {!Trace_file} writes a recorder's lines to a file when
    closed, if [ASCETIC_DDD_TRACE_DIR] is set, so that a test suite attaches one
    unconditionally and records only when asked. *)

module Json_trace = Json_trace
module Trace_file = Trace_file
