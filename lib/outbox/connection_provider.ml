(** Connection provider — abstracts how the outbox dispatcher acquires its
    own database connection (separate from the publisher's unit of work).

    The dispatcher needs to open new transactions to read pending messages
    and update consumer offsets. It cannot reuse the publisher's UoW because
    that UoW is owned by the business operation that produced the message.

    Typical implementations:
    - Single connection: see {!of_connection}.
    - Caqti pool: build a module wrapping [Caqti_eio.Pool.use]. *)

(** A connection provider is a first-class module so its [with_connection]
    field is genuinely polymorphic in the result type ['a]. *)
module type S = sig
  val with_connection :
    ((module Caqti_eio.CONNECTION) -> ('a, string) result) ->
    ('a, string) result
end

type t = (module S)

let with_connection ((module P) : t) f = P.with_connection f

let of_connection conn : t =
  (module struct
    let with_connection f = f conn
  end)
