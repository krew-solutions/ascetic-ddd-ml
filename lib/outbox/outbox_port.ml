(** Port (abstract interface) for the Transactional Outbox.

    The application layer depends on this signature; the infrastructure
    layer (e.g. {!Outbox} for PostgreSQL via Caqti) provides a concrete
    implementation by satisfying [S with type uow = ...].

    This is the OCaml mirror of the Python [IOutbox] abstract base class:
    it lets callers stay agnostic of the underlying database driver. *)

module type S = sig
  type t
  (** An outbox handle. Construction is implementation-specific (see
      {!Outbox.create}); from this point on it is abstract to the
      application layer. *)

  type uow
  (** The unit of work used by [publish] / [setup] / [get_position] /
      [set_position]. The implementation pins this to a concrete type
      (e.g. [Caqti_unit_of_work.t]); the application layer treats it as
      opaque and passes it through. *)

  type subscriber = Outbox_message.t -> (unit, string) result
  (** Callback invoked by the dispatcher for each message read from the
      outbox. Returning [Error] aborts the current batch and rolls back
      the dispatcher transaction so the message is redelivered next time. *)

  val publish : t -> uow -> Outbox_message.t -> (unit, string) result
  (** Insert a message inside the caller's unit of work. The message
      becomes visible to dispatchers only after the surrounding
      transaction commits. *)

  val dispatch :
    ?consumer_group:string ->
    ?uri:string ->
    ?worker_id:int ->
    ?num_workers:int ->
    t ->
    subscriber ->
    (bool, string) result
  (** Dispatch the next batch of pending messages. [Ok true] means at
      least one message was processed; [Ok false] means there was nothing
      to do. *)

  val run :
    ?consumer_group:string ->
    ?uri:string ->
    ?process_id:int ->
    ?num_processes:int ->
    ?concurrency:int ->
    ?poll_interval:float ->
    ?stop:(unit -> bool) ->
    t ->
    clock:_ Eio.Time.Mono.t ->
    subscriber ->
    (unit, string) result
  (** Continuously dispatch messages until [stop ()] returns [true], with
      [concurrency] loops in this process.

      Returns [Ok ()] once stopped. The first error of any loop, a
      database failure or a subscriber returning [Error], stops every
      loop, a loop in the middle of a batch finishing it first, and comes
      back as [Error]. Retrying is the caller's policy: a supervisor that
      logs the error, waits and calls [run] again redelivers the rolled
      back batch. *)

  val setup : t -> uow -> (unit, string) result
  (** Create the outbox / offsets tables and indexes if they do not
      already exist. *)

  val cleanup : t -> uow -> (unit, string) result
  (** Release any resources held by the outbox. *)

  val get_position :
    ?consumer_group:string ->
    ?uri:string ->
    t ->
    uow ->
    (string * int64, string) result
  (** Read the current [(transaction_id, offset_acked)] for a consumer
      group. Returns [("0", 0L)] when no row exists yet. *)

  val set_position :
    t ->
    uow ->
    consumer_group:string ->
    uri:string ->
    transaction_id:string ->
    offset:int64 ->
    (unit, string) result
  (** Force-set the position for a consumer group. *)

  (** Async-generator-style iterator with per-message ack.

      Each batch is fetched in one transaction; a message is acknowledged
      when the consumer asks for the next one, so the acknowledgement
      always follows the processing. A database failure at any step, from
      ensuring the consumer group to acknowledging, ends the iterator with
      that error rather than looking like an empty outbox. *)
  module Iter : sig
    type iter

    val start :
      ?consumer_group:string ->
      ?uri:string ->
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      iter

    val next : iter -> (Outbox_message.t option, string) result
    (** The next message; [Ok None] once the iterator has stopped or been
        closed; [Error] with the failure that ended it. After an error the
        iterator is closed and its open transaction rolled back: call
        {!start} again to resume from the last acknowledged position. *)

    val close : iter -> unit

    val iter :
      ?consumer_group:string ->
      ?uri:string ->
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      (Outbox_message.t -> unit) ->
      (unit, string) result
    (** Run [f] on every message until [stop ()] returns [true], giving
        [Ok ()], or a database failure ends the iteration, giving
        [Error]. *)
  end
end
