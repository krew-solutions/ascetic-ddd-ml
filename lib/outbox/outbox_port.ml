(** Port (abstract interface) for the Transactional Outbox.

    The application layer depends on this signature; the infrastructure
    layer (e.g. {!Outbox} for PostgreSQL via Caqti) provides a concrete
    implementation by satisfying [S with type uow = ...].

    This is the OCaml mirror of the Python [IOutbox] abstract base class:
    it lets callers stay agnostic of the underlying database driver.

    Errors are values of {!Error.t}, so that a caller can tell a database
    that went away from a statement it refused and from a message the
    subscriber declined. Operations that take no subscriber never produce
    [Error.Subscriber]; their error type is left polymorphic in ['e] so that
    it composes with the caller's without conversion. *)

(** What can go wrong, as a value the caller can act on. *)
module Error = struct
  type 'e t =
    | Connection of string
        (** A connection could not be established or acquired. Retry once
            the provider can connect again. *)
    | Request of string
        (** The database refused or failed a statement: permanent for a
            missing table or a violated constraint, transient for a deadlock
            or a connection lost mid-statement. Look before retrying. *)
    | Malformed of string
        (** A value could not be encoded for, or decoded from, the database.
            A defect, not a retry. *)
    | Subscriber of 'e
        (** The subscriber declined a message with its own error. The batch
            was rolled back and is redelivered by the next dispatch. *)

  let pp pp_subscriber ppf = function
    | Connection reason -> Format.fprintf ppf "connection: %s" reason
    | Request reason -> Format.fprintf ppf "request: %s" reason
    | Malformed reason -> Format.fprintf ppf "malformed: %s" reason
    | Subscriber e -> Format.fprintf ppf "subscriber: %a" pp_subscriber e

  let to_string subscriber_to_string error =
    Format.asprintf "%a"
      (pp (fun ppf e -> Format.pp_print_string ppf (subscriber_to_string e)))
      error
end

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

  module Error = Error
  (** The errors of every operation below. *)

  type 'e subscriber = Outbox_message.t -> (unit, 'e) result
  (** Callback invoked by the dispatcher for each message read from the
      outbox. Returning [Error e] aborts the current batch and rolls back
      the dispatcher transaction so the messages are redelivered next
      time; [e] reaches the caller as [Error.Subscriber e]. *)

  val publish : t -> uow -> Outbox_message.t -> (unit, 'e Error.t) result
  (** Insert a message inside the caller's unit of work. The message
      becomes visible to dispatchers only after the surrounding
      transaction commits. *)

  val dispatch :
    ?consumer_group:string ->
    ?uri:string ->
    ?worker_id:int ->
    ?num_workers:int ->
    t ->
    'e subscriber ->
    (bool, 'e Error.t) result
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
    'e subscriber ->
    (unit, 'e Error.t) result
  (** Continuously dispatch messages until [stop ()] returns [true], with
      [concurrency] loops in this process.

      Returns [Ok ()] once stopped. The first error of any loop, a
      database failure or a subscriber returning [Error], stops every
      loop, a loop in the middle of a batch finishing it first, and comes
      back as [Error]. Retrying is the caller's policy: a supervisor that
      matches on the error, waits and calls [run] again redelivers the
      rolled back batch. *)

  val setup : t -> uow -> (unit, 'e Error.t) result
  (** Create the outbox / offsets tables and indexes if they do not
      already exist. *)

  val cleanup : t -> uow -> (unit, 'e Error.t) result
  (** Release any resources held by the outbox. *)

  val get_position :
    ?consumer_group:string ->
    ?uri:string ->
    t ->
    uow ->
    (string * int64, 'e Error.t) result
  (** Read the current [(transaction_id, offset_acked)] for a consumer
      group. Returns [("0", 0L)] when no row exists yet. *)

  val set_position :
    t ->
    uow ->
    consumer_group:string ->
    uri:string ->
    transaction_id:string ->
    offset:int64 ->
    (unit, 'e Error.t) result
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

    val next : iter -> (Outbox_message.t option, 'e Error.t) result
    (** The next message; [Ok None] once the iterator has stopped or been
        closed; [Error] with the failure that ended it, never
        [Error.Subscriber]. After an error the iterator is closed and its
        open transaction rolled back: call {!start} again to resume from
        the last acknowledged position. *)

    val close : iter -> unit
    (** Detach the consumer: the open batch, acknowledgements included, is
        rolled back and redelivered by the next start. *)

    val iter :
      ?consumer_group:string ->
      ?uri:string ->
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      'e subscriber ->
      (unit, 'e Error.t) result
    (** Run the subscriber on every message until [stop ()] returns
        [true], giving [Ok ()]. A subscriber returning [Error e] rolls the
        open batch back and ends the iteration with [Error.Subscriber e];
        a database failure ends it with that error. *)
  end
end
