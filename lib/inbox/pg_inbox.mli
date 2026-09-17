(** The inbox over PostgreSQL, through the session of [ascetic_ddd.session.caqti].

    {2 Idempotency}

    A message is a row whose primary key is its identity,
    [(tenant_id, stream_type, stream_id, stream_position)]. Receiving the same message
    again is [INSERT ... ON CONFLICT DO NOTHING]. A [message_id] in the metadata is unique
    in the table too.

    {2 Slots}

    A row carries its slot, [hashtext(<partition key>) % slots] with the sign bit cleared
    so that no key is left to nobody, computed once at insert and stored; the number of
    slots and the key are fixed for the life of the table (ADR-0007). A dispatcher has no
    identity: a call takes whichever slot has a due head and is held by nobody, least
    recently served first, locks that slot's row for the length of its transaction, and so
    one dispatcher at a time works a slot: the order of arrival within a slot is a
    guarantee, not a condition. A dispatcher that dies releases its slot with its
    transaction. With causal dependencies, cut by stream, so that a message and what it
    depends on land in one slot.

    {2 Dependencies}

    The walk looks at the head of the queue only. A head whose causal dependencies are not
    all processed is set aside to wait for the first missing one, [waiting_for], out of
    the queue, and that is the whole transaction, {!Outcome.Set_aside}; the transaction
    that marks that dependency processed puts every row waiting for it back, in the same
    statement. Nothing polls for a dependency. The wait and the mark take a
    transaction-level advisory lock on the dependency's identity, so that a mark cannot
    slip between the check and the wait (ADR-0008). With [max_wait] a row waiting longer
    is parked with the dependency named; by default it waits for ever (ADR-0005).

    {2 Failures}

    The subscriber runs in a savepoint. When it fails, its writes roll back to the
    savepoint and the transaction goes on to record the attempt: [attempts], [last_error],
    [next_attempt_at]. Until that time the row is not taken, and it holds its slot, so
    that the order of arrival survives the failure. After [Retries.max_attempts] failures
    the row is parked, [parked_at]: out of the queue, in the table, so that a later
    arrival of the same message is still a duplicate. An operator lists the parked rows
    and either {!unpark}s one or {!resolve}s it, marking it processed without the
    subscriber's effects. Off by default: unlimited attempts, no backoff (ADR-0004).

    A subscriber's error is a {!Failure.t}: [Transient] is tried again as above;
    [Permanent] is the verdict that no retry will ever succeed, and the message is parked
    at once. Errors of the database are the loop's business, not the subscriber's: a loop
    of {!run} that meets an error of the moment, a lock cycle the server broke, a
    connection lost, waits and goes on; one that meets a defect stops every loop
    (ADR-0009).

    {2 Observing}

    [?observer] attaches an {!Inbox_observer.t}: it is told of every message received and
    of every step a dispatcher takes, in the vocabulary of the protocol model. *)

module Session = Ascetic_session_caqti.Caqti_session
module Session_pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

type t

type subscriber = Session.t -> Inbox_message.t -> (unit, Failure.t) result
(** What a dispatcher hands a message to, with the transaction the mark commits in: the
    subscriber's writes through it and the mark are one transaction. *)

val create :
  ?observer:Inbox_observer.t ->
  ?table:Identifier.t ->
  ?sequence:Identifier.t ->
  ?partition:Partition_key.t ->
  ?slots:int ->
  ?retries:Retries.t ->
  ?max_wait:float ->
  Session_pool.t ->
  t
(** An inbox over the pool: in table [inbox] with the sequence
    [inbox_received_position_seq] unless named otherwise, one slot partitioned by URI,
    observed by nobody, retrying a failed message for ever and letting a message wait for
    its dependencies for ever. The names go into SQL as text, so they are
    {!Identifier.t}s: parsed once, safe after. [slots], [1..32767], and [partition] are
    read by {!setup} when it creates the table; a table already cut otherwise is refused,
    because the rows carry their slot for good (ADR-0007). [max_wait], in seconds, parks a
    message that has waited longer for a dependency, with the dependency named. *)

val with_retries : t -> Retries.t -> t
(** The same inbox, treating a failed message as [retries] says. *)

val with_max_wait : t -> float -> t
(** The same inbox, parking a message that has waited longer than [max_wait] seconds for a
    dependency. *)

val slots : t -> int
(** How many slots this inbox cuts its table into. *)

val setup : t -> Session.t -> (unit, Inbox_error.t) result
(** Creates the sequence, the table, the indexes and the tables of the cut and of the
    slots if they do not exist, in one transaction under an advisory lock on the table's
    name, so that two processes may run it at once; and refuses to go on, with
    [Malformed], when the table is cut into another number of slots or by another key:
    that is a migration, not a restart.

    The head index, [(slot, received_position)] over the queue, the rows neither
    processed, parked nor waiting, is the walk's order, and the only index in
    [received_position] order on purpose: beside a second one over the whole column the
    planner walked that one from the first row, past the whole processed history. The
    sequence keeps [received_position] unique without a constraint. [<table>_meta] holds
    the number of slots and the key the table was cut by, and [<table>_slots] one row per
    slot, the row a dispatcher locks to hold the slot. *)

val publish : t -> Inbox_message.t -> (unit, Inbox_error.t) result
(** Stores the message in a transaction of its own and tells the observer where it landed,
    or that it was already there. The port, {!Inbox_port.S}. *)

val dispatch : t -> subscriber -> (Outcome.t, Inbox_error.t) result
(** Processes the next eligible message with the subscriber, in one transaction.

    Takes whichever slot has a due head and is held by nobody, least recently served
    first, and holds it for the length of the transaction, so that one dispatcher at a
    time works a slot and the order of arrival within it is kept. A head whose dependency
    is not processed is set aside to wait for it, and that is the whole transaction: the
    wait is committed at once, under a lock on the dependency's identity that its mark
    takes too, so that neither misses the other (ADR-0008); the next call takes the slot's
    next head. Otherwise the subscriber runs inside the transaction that marks the message
    processed, and is given that transaction, in a savepoint of its own: if it fails, its
    writes are rolled back to the savepoint, and the attempt is recorded and committed
    instead of the mark. [Error] is for the database; a failing subscriber is an
    {!Outcome.t}. A dispatcher has no identity: any number of them, in any number of
    processes, share the slots through the locks alone. *)

val run :
  t ->
  clock:_ Eio.Time.Mono.t ->
  ?loops:Loops.t ->
  shutdown:unit Eio.Promise.t ->
  subscriber ->
  (unit, Inbox_error.t) result
(** Processes messages until [shutdown] is resolved, with [loops.concurrency] loops as
    fibers of the calling fiber. A loop that finds nothing waits [loops.poll_interval];
    one that processed, failed or set a message aside goes on at once. Shutdown is
    cooperative: a loop finishes its message, commits, and only then stops. A loop that
    meets an error of the moment, a lock cycle the server broke, a connection lost, a
    server going down, waits, longer with each such error in a row up to
    [loops.max_pause], and goes on: its transaction was rolled back and the message comes
    back. On any other error of the database, a defect, the loops are all stopped and the
    error is returned (ADR-0009). A failing subscriber is neither: it is an {!Outcome.t}.
*)

val parked : t -> Session.t -> (Inbox_message.t list, Inbox_error.t) result
(** The parked messages, oldest first, with their attempts and last error. *)

val unpark : t -> Session.t -> Inbox_message.t -> (bool, Inbox_error.t) result
(** Gives a parked message another go: attempts reset, due at once. Returns whether the
    message was parked. *)

val resolve : t -> Session.t -> Inbox_message.t -> (bool, Inbox_error.t) result
(** Marks a parked message processed by hand, without the subscriber's effects, and puts
    what waited for it back into the queue. Runs in a transaction of the session, under
    the lock a mark takes (ADR-0008), so a wait for this message set beside it is woken
    all the same. Returns whether the message was parked. *)
