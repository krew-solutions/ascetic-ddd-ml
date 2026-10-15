# ADR-0025: The intake names the row, and at an inbox that receives several channels the stage is chosen by the row

## Status

Accepted (2026-10-15). Follows the decision taken first in the Rust
reference implementation (its ADR-0018, 2026-10-14). Held by
`test/inbox/test_bridge.ml` (a message from a broker carrying a producer's
`destination` lands under the intake's uri; the relay out of an outbox
still lands under the destination the outbox stamped, which is the same
text) and `test/dek/test_stage.ml` (one stage opens the rows of two
channels by their destination, the key left aside; a row under another
channel's destination does not open and is refused for good; a destination
outside the map, or none, is refused before any KMS is asked; a map naming
a channel the sealing lacks is refused at construction).

## Context

Three decisions met at the first inbox of the reference to receive another
producer's channel beside its own commands, and what each had left unsaid
became visible only together. This port had all three as the reference
had them.

**The inbox has one subscriber for every row.** The Python source first had
a route by address inside the inbox and dropped it, for a reason worth more
than the convenience it refused: causal dependencies cross addresses. A
message on `kafka://shipments` may depend on one on `kafka://orders`; a
worker that saw `shipments` alone would wait for ever on a dependency it
never processes. This port kept the decision, `Inbox_channel.consumer`
takes no address and the row's `uri` is a column and not a filter, and
kept the reason nowhere: the README said "the whole inbox is one channel"
without saying why, which is how the next person would have added the
filter and broken the wait of ADR-0005, resumed as it is by the
transaction that marks the dependency, which needs one loop over every row.

**The stage is the channel's.** ADR-0024 decision 2: one envelope stage per
channel, over a KMS instance serving the channel's address as the
derivation context, at both ends; a consumer applies its stage with
`through`, one stage per consumer. For an inbox of one channel that is one
stage. For an inbox of three it is a stage that must be chosen per row,
and nothing chose it: the commands' stage would meet a foreign row, fail to
unwrap, and park it as `Permanent`, every one.

**The row was named by the sender.** ADR-0011: "the destination travels in
the message"; an outbox row keeps its final uri, the wire carries it as a
`destination` header, a bridge reads it, and the inbox intake took it as
the row's `uri` whenever it was present, falling back to the channel the
intake's producer was built for. That was written for the relay out of an
outbox, where the bridge's target *is* the header, so the producer is
built for it and the two never differ. The Kafka adapter forwards headers
both ways. So a producer on this seedwork stamps `kafka://orders/<key>` on
what it publishes to a broker, the receiver's intake takes that as the
row's name, and a producer on another stack, stamping nothing, leaves the
row under `inbox://orders/<key>`: one channel, two names for its rows, and
which one a row gets is the sender's doing. The only bridge test of this
port ran from an outbox into an inbox, where the two names coincide, so
the suite could not see it.

## Decision

1. **The intake names the row.** A row's `uri` is the inbox channel the
   intake published it to, the channel the producer was built for, with
   the message's key, or the channel's own key when the message has none;
   a `destination` header on an incoming message is the producer's address
   and is not kept, neither as the name nor as metadata. For the relay out
   of an outbox nothing changes: the bridge's target is the header, the
   producer is built for it, the row's name is the same text. ADR-0011 is
   refined, not reversed: the header is the only source *for the relay*;
   at the intake the receiver's channel is the source, and the header the
   row goes out with is the receiver's name, as before.
2. **At an inbox that receives several channels, the stage is chosen by
   the row.** `Sealing.by_destination` in `ascetic_ddd.dek.envelope`: for
   each `(destination, channel)`, a row whose `destination`, key left aside,
   is the first opens with the stage of the second, the channel it was
   sealed under at the producer's end. Built up front, as the sealing is: a
   channel the sealing was not built for is refused at construction,
   `unsealed_channel`, so a composition root does not start with a
   destination routed to nothing. A row without the header, or with a
   destination the stage was not built for, is refused for good before any
   stage, and so before any KMS, is asked: nothing is opened under a
   guessed context. `outbound` delegates the same way, so
   `By_destination.stage` is a stage in full. The convention for the
   intake's bridges: `Bridge.Fixed "inbox://<the producer's channel
   address>"`, one destination per channel received.
3. **The inbox keeps one subscriber and no filter by address, and says
   why.** The reason of the Python source is written into
   `lib/inbox/README.md` and the channel's interface: a consumer that saw
   one address would wait for ever on a causal dependency published under
   another. Routing is the subscriber's, by the row's `destination` and
   `stream_type`.

## Objections considered

1. *The change breaks a receiver that relied on the producer's header
   naming its rows.* No receiver in this repository or in the reference's
   consumers read it: the relay's header equals the channel, and the one
   bridge test asserting the uri asserts the same text before and after. A
   receiver that wants a producer's address has the channel it bridged
   from.
2. *`By_destination` is a module with `stage` and `destinations`, not a
   stage itself as the reference's is.* A stage here is a record of two
   functions (ADR-0010), which cannot carry the list of its destinations; a
   composition root reads the list to check its wiring and hands the bus
   the record. The reference's struct does both because a struct can.
3. *The map's keys drop the destination's key, so two destinations
   differing only in key collapse into one.* By design: the key is the
   message's and varies per row, the destination without it is the
   channel's name, and a stage is the channel's. A row's key of either
   origin, the message's or the channel's own, maps alike; the test opens
   rows keyed by the tenant under destinations declared without one.

## Consequences

- A row's name is the receiver's and deterministic per channel, whatever
  the sender's stack; the intake's bridges and the stage's map are written
  from one list, the channels the receiver's contracts say it receives.
- A producer's `destination` header is lost at the intake. Nothing read it.
- One header read and one lookup per row before the stage; a row the map
  does not know costs no KMS call.
- A consumer composes its intake as one subscriber over one
  `By_destination.stage`, and branches by `stream_type` after the row is
  open: own commands to their channel, foreign events to their
  anti-corruption handlers.
- `Inbox_channel` still excludes `destination` from the metadata it keeps,
  so the header is neither the name nor a field.

What can go wrong, and what holds it:

- *Two channels bridged to one destination.* Their rows would share a name
  and one stage, and one of them would never open. Held by the convention
  of decision 2, the destination is the producer's channel address under
  `inbox://`, and by a receiver's declaration that names each received
  channel once.
- *A row's key is not the message's.* The intake takes the message's key
  first, the channel's second, exactly as before; the stage leaves the key
  aside.
- *A producer relied on the receiver routing by its header.* No producer
  may: the receiver's route is its bridge's target. A bridge with
  `Bridge.Header` into an inbox still builds the producer for that header,
  so even that row carries it.

## Alternatives rejected

**A filter by address on the inbox's consumer.** The Python source's
rejection stands: cross-address causal dependencies stall, and the wait of
ADR-0005 is resumed by one loop. Two loops over one table would also take
two slot locks for one slot.

**A map with both names per channel, `kafka://…` and `inbox://…`.** Makes
the receiver's routing depend on the producer's stack, and silently so: a
producer that starts stamping headers renames every row it sends.

**Stripping the scheme and guessing.** `inbox://policy-commands` is a
catalogue address with its scheme; `device.lifecycle` is one without. A
rule that strips would have to know which is which; that knowledge is the
map.

**One inbox per channel.** A table, a loop, a slot lock and a bridge per
channel, and a causal dependency across channels made impossible by
construction, the thing the single subscriber exists to allow.

**Keeping the producer's destination in the row's metadata.** The row
would go out with two `destination` headers, and nothing reads the second.
