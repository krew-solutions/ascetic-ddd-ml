(** Connection provider — how a dispatcher acquires its own database
    connections, separately from the publisher's unit of work.

    The outbox and inbox dispatchers open transactions of their own to read
    pending messages and record positions; they cannot borrow the publisher's
    unit of work, which belongs to the business operation that produced the
    message.

    Acquiring a connection and using it are two layers of the result:
    [with_connection f] is [Error] only when no connection could be obtained,
    and [Ok] with whatever [f] returned otherwise, [f]'s own result included.
    The provider therefore knows nothing about the caller's error type, and
    the caller lifts the acquisition error into its own once. This is also
    the shape of [Caqti_eio.Pool.use], so a pool needs no adaptation: see
    {!of_pool}. *)

(** A first-class module, so that [with_connection] stays polymorphic in the
    result of [f]. *)
module type S = sig
  val with_connection :
    ((module Caqti_eio.CONNECTION) -> 'a) -> ('a, Caqti_error.t) result
  (** Run [f] with a connection, returning the connection to the provider
      afterwards, whether [f] returns or raises. *)
end

type t = (module S)

let with_connection ((module P) : t) f = P.with_connection f

(** A provider over one connection, handed to every call in turn. Fine for
    one fiber; Caqti rejects concurrent use of a connection from several
    fibers, so pair [concurrency > 1] with {!of_pool}. *)
let of_connection conn : t =
  (module struct
    let with_connection f = Ok (f conn)
  end)

(** A provider over a Caqti pool, such as the one [Caqti_eio_unix.connect_pool]
    returns. A connection that cannot be acquired is the [Error]. *)
let of_pool (pool : ((module Caqti_eio.CONNECTION), Caqti_error.t) Caqti_eio.Pool.t)
    : t =
  (module struct
    let with_connection f = Caqti_eio.Pool.use (fun conn -> Ok (f conn)) pool
  end)
