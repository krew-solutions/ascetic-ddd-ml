# Trace

Observers that record what the outbox and the inbox report as lines of
JSON, for validation against the protocol models in `verify/tla`.

`Json_trace` is both an outbox observer and an inbox observer,
`Json_trace.outbox_observer` and `Json_trace.inbox_observer` over one
recorder, so one recorder can watch an outbox feeding an inbox and keep the
order across the two. Each line names the observer that reported it,
`"observer": "outbox"` or `"inbox"`, and otherwise carries the event's own
fields, in the shape the Rust reference implementation writes, so that
`verify/tla/trace2tla.py` reads both. `Trace_file` writes a recorder's lines
to `$ASCETIC_DDD_TRACE_DIR/<stem>.jsonl` when closed, if the variable is
set, so that a test suite attaches one unconditionally and records only when
asked:

```ocaml
let trace = Trace_file.from_env "outbox-roundtrip" in
let outbox = Pg_outbox.create ~observer:(Json_trace.outbox_observer (Trace_file.recorder trace)) pool in
Fun.protect ~finally:(fun () -> Trace_file.close trace) (fun () -> (* the run *) ...)
```

The file name's prefix, before the first dash, tells `trace2tla.py` which
model to check against: `outbox-`, `inbox-` or `bridge-`. `Trace_file.off`
is for a run with several dispatchers at once, whose events are logged in an
order that is not the order of their commits, and which the trace check
therefore does not cover. See `verify/tla/README.md`.
