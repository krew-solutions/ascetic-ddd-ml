(** Port (abstract interface) for the Transactional Inbox.

    The application layer depends on this signature; the infrastructure
    layer (e.g. {!Inbox} for PostgreSQL via Caqti) provides a concrete
    implementation by satisfying [S with type uow = ...]. *)

module type S = sig
  type t
  (** An inbox handle. Construction is implementation-specific (see
      {!Inbox.create}); from this point on it is abstract to the
      application layer. *)

  type uow
  (** The unit of work in which the subscriber callback runs. The
      implementation pins this to a concrete type (e.g.
      [Caqti_unit_of_work.t]); the application layer treats it as opaque. *)

  type subscriber = uow -> Inbox_message.t -> (unit, string) result
  (** Callback invoked by the dispatcher with the in-flight transaction
      and the message. Returning [Error] aborts the transaction so that
      the message is not marked processed and gets retried. *)

  val publish : t -> Inbox_message.t -> (unit, string) result
  (** Receive and persist an incoming message.

      Idempotent on
      [(tenant_id, stream_type, stream_id, stream_position)] — duplicate
      submissions are silently ignored via [INSERT ... ON CONFLICT DO
      NOTHING]. *)

  val dispatch :
    ?worker_id:int -> ?num_workers:int -> t -> subscriber -> (bool, string) result
  (** Process the next eligible message: skips messages whose causal
      dependencies are not yet processed, runs the subscriber inside a
      database transaction, marks the message processed on success.

      [Ok true] = a message was processed, [Ok false] = no eligible
      messages right now. *)

  val run :
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
      loop, a loop in the middle of a message finishing it first, and
      comes back as [Error]. Retrying is the caller's policy: a supervisor
      that logs the error, waits and calls [run] again retries the
      unprocessed message. *)

  val setup : t -> uow -> (unit, string) result
  (** Create the inbox sequence + table + indexes if they do not
      already exist. *)

  val cleanup : t -> uow -> (unit, string) result
  (** Release any resources held by the inbox. *)

  (** Async-generator-style iterator that yields each eligible message
      with the transaction it was fetched in, and marks it processed when
      the consumer asks for the next one, so the mark always follows the
      processing and commits with the consumer's writes. A database
      failure at any step ends the iterator with that error rather than
      looking like an empty inbox. *)
  module Iter : sig
    type iter

    val start :
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      iter

    val next : iter -> ((uow * Inbox_message.t) option, string) result
    (** The next message with its transaction; [Ok None] once the iterator
        has stopped or been closed; [Error] with the failure that ended
        it. After an error the iterator is closed and its open transaction
        rolled back: call {!start} again to retry the message. *)

    val close : iter -> unit

    val iter :
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      (uow -> Inbox_message.t -> unit) ->
      (unit, string) result
    (** Run [f] on every message until [stop ()] returns [true], giving
        [Ok ()], or a database failure ends the iteration, giving
        [Error]. *)
  end
end
