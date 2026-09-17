(** Port (abstract interface) for the Transactional Inbox.

    The application layer depends on this signature; the infrastructure
    layer (e.g. {!Inbox} for PostgreSQL via Caqti) provides a concrete
    implementation by satisfying [S with type uow = ...].

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
        (** The subscriber declined a message with its own error. Its writes
            and the mark were rolled back; the message is retried by the
            next dispatch. *)

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
  (** An inbox handle. Construction is implementation-specific (see
      {!Inbox.create}); from this point on it is abstract to the
      application layer. *)

  type uow
  (** The unit of work in which the subscriber callback runs. The
      implementation pins this to a concrete type (e.g.
      [Caqti_unit_of_work.t]); the application layer treats it as opaque. *)

  module Error = Error
  (** The errors of every operation below. *)

  type 'e subscriber = uow -> Inbox_message.t -> (unit, 'e) result
  (** Callback invoked by the dispatcher with the in-flight transaction
      and the message. Returning [Error e] aborts the transaction so that
      the message is not marked processed and gets retried; [e] reaches
      the caller as [Error.Subscriber e]. *)

  val publish : t -> Inbox_message.t -> (unit, 'e Error.t) result
  (** Receive and persist an incoming message.

      Idempotent on
      [(tenant_id, stream_type, stream_id, stream_position)] — duplicate
      submissions are silently ignored via [INSERT ... ON CONFLICT DO
      NOTHING]. *)

  val dispatch :
    ?worker_id:int ->
    ?num_workers:int ->
    t ->
    'e subscriber ->
    (bool, 'e Error.t) result
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
    'e subscriber ->
    (unit, 'e Error.t) result
  (** Continuously dispatch messages until [stop ()] returns [true], with
      [concurrency] loops in this process.

      Returns [Ok ()] once stopped. The first error of any loop, a
      database failure or a subscriber returning [Error], stops every
      loop, a loop in the middle of a message finishing it first, and
      comes back as [Error]. Retrying is the caller's policy: a supervisor
      that matches on the error, waits and calls [run] again retries the
      unprocessed message. *)

  val setup : t -> uow -> (unit, 'e Error.t) result
  (** Create the inbox sequence + table + indexes if they do not
      already exist. *)

  val cleanup : t -> uow -> (unit, 'e Error.t) result
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

    val next : iter -> ((uow * Inbox_message.t) option, 'e Error.t) result
    (** The next message with its transaction; [Ok None] once the iterator
        has stopped or been closed; [Error] with the failure that ended
        it, never [Error.Subscriber]. After an error the iterator is
        closed and its open transaction rolled back: call {!start} again
        to retry the message. *)

    val close : iter -> unit
    (** Detach the consumer: the open transaction, the consumer's writes
        and the mark included, is rolled back and the message retried by
        the next start. *)

    val iter :
      ?poll_interval:float ->
      ?stop:(unit -> bool) ->
      clock:_ Eio.Time.Mono.t ->
      t ->
      'e subscriber ->
      (unit, 'e Error.t) result
    (** Run the subscriber on every message until [stop ()] returns
        [true], giving [Ok ()]. A subscriber returning [Error e] rolls the
        transaction back and ends the iteration with [Error.Subscriber e];
        a database failure ends it with that error. *)
  end
end
