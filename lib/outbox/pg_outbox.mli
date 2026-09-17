(** The outbox over PostgreSQL, through the session of [ascetic_ddd.session.caqti].

    {2 Ordering and visibility}

    A [BIGSERIAL] position does not order messages across concurrent transactions: a
    transaction may take position 1, run for a while, and commit after the one that took
    position 2. A dispatcher that has passed position 2 would never see 1. So every row
    also records the inserting transaction, [pg_current_xact_id()], and a dispatcher reads
    only rows whose transaction is older than every transaction still running,
    [transaction_id < pg_snapshot_xmin(pg_current_snapshot())], in
    [(transaction_id, position)] order.

    {2 Consumer groups and slots}

    A row carries its slot, [hashtext(uri) % slots] with the sign bit cleared, [hashtext]
    being signed and [%] keeping the sign of the dividend, so that no URI is left to
    nobody, computed once at insert and stored; the number of slots is fixed for the life
    of the table (ADR-0006). Each [(consumer_group, uri, slot)] keeps its own position. A
    dispatcher has no identity: a fetch takes whichever slot of the selection has visible
    work and is held by nobody, locks that slot's position row for the length of the
    batch, [FOR UPDATE SKIP LOCKED], and so two dispatchers never process a message twice
    and any number of them share a selection without being told who they are. A dispatcher
    that dies releases its slot with its transaction. One slot, the default, is one
    position per group and the order of the whole selection; more slots are parallelism,
    and order within a URI still, since a URI is in one slot.

    {2 Delivery}

    At least once. The subscriber runs inside the dispatcher's transaction and the
    position is acknowledged after the batch; a crash in between redelivers the batch.
    Consumers deduplicate on [metadata.message_id].

    {2 Observing}

    [?observer] attaches an {!Outbox_observer.t}: it is told of every message published
    and of every step a dispatcher takes, in the vocabulary of the protocol model. The
    outbox is typed by the subscriber's error, ['e], which the observer sees as it is. *)

module Session = Ascetic_session_caqti.Caqti_session
module Session_pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

type 'e t

type 'e subscriber = Outbox_message.t -> (unit, 'e) result
(** What a dispatcher hands each message of a batch to. [Error e] rolls the batch back; it
    is delivered again by the next dispatch, and [e] reaches the caller as
    [Outbox_error.Subscriber e]. *)

val default_batch_size : int
(** Messages fetched per dispatch by default: 100. *)

val create :
  ?observer:'e Outbox_observer.t ->
  ?outbox_table:Identifier.t ->
  ?offsets_table:Identifier.t ->
  ?batch_size:int ->
  ?slots:int ->
  Session_pool.t ->
  'e t
(** An outbox over the pool: in tables [outbox] and [outbox_offsets] unless named
    otherwise, observed by nobody, fetching {!default_batch_size} messages per dispatch,
    cut into one slot. The names go into SQL as text, so they are {!Identifier.t}s: parsed
    once, safe after. [slots], [1..32767], is read by {!setup} when it creates the table;
    a table already cut otherwise is refused, because the rows carry their slot for good
    (ADR-0006). *)

val slots : _ t -> int
(** How many slots this outbox cuts its table into. *)

val setup : _ t -> Session.t -> (unit, 'e Outbox_error.t) result
(** Creates the tables and indexes if they do not exist, in one transaction under an
    advisory lock on the table's name, so that two processes may run it at once; and
    refuses to go on, with [Malformed], when the table exists with another number of
    slots: that is a migration, not a restart.

    The primary key [(transaction_id, position)] is the order a fetch reads in. The index
    on [(slot, transaction_id, position)] serves a slot's range from its position on; the
    one on [(uri, transaction_id, position)] serves a selection by URI, equal or by
    prefix. [<outbox>_meta] holds the number of slots the table was cut into. *)

val publish : _ t -> Session.t -> Outbox_message.t -> (unit, 'e Outbox_error.t) result
(** Writes the message inside the session's transaction, stamped with that transaction's
    id, and tells the observer where it landed. The port, {!Outbox_port.S}. *)

val dispatch : 'e t -> Selection.t -> 'e subscriber -> (bool, 'e Outbox_error.t) result
(** Dispatches the next batch of the selection to the subscriber: the batch of whichever
    slot has visible work and is held by nobody, least recently served first.

    Returns whether there was anything to dispatch. The subscriber runs inside the
    dispatcher's transaction, under the lock of the slot's position; if it fails, the
    batch is rolled back and redelivered. A dispatcher has no identity: any number of
    them, in any number of processes, share a selection through the locks alone, and one
    that dies releases its slot with its transaction. *)

val run :
  'e t ->
  clock:_ Eio.Time.Mono.t ->
  ?loops:Loops.t ->
  shutdown:unit Eio.Promise.t ->
  Selection.t ->
  'e subscriber ->
  (unit, 'e Outbox_error.t) result
(** Dispatches the selection until [shutdown] is resolved, with [loops.concurrency] loops
    as fibers of the calling fiber. A loop that finds nothing waits [loops.poll_interval].
    Shutdown is cooperative: a loop finishes its batch, commits, and only then stops. A
    loop whose subscriber failed, the batch rolled back, to be delivered again, or that
    met an error of the moment in the database, a connection lost, a server going down,
    waits, longer with each failure in a row up to [loops.max_pause], and goes on. On any
    other error of the database, a defect, the loops are all stopped and the error is
    returned (ADR-0009). *)

val positions :
  _ t -> Session.t -> Selection.t -> (Position.t list, 'e Outbox_error.t) result
(** Where the selection is, by slot: the last acknowledged transaction and offset of each,
    zero before anything was acknowledged; empty before the first dispatch. The minimum
    over the slots is a watermark for the whole selection. *)

val set_position :
  _ t -> Session.t -> Selection.t -> Position.t -> (unit, 'e Outbox_error.t) result
(** Moves every slot of the selection to the position: back to {!Position.zero} to replay,
    or ahead to skip. Creates the position rows if the selection has none. *)
