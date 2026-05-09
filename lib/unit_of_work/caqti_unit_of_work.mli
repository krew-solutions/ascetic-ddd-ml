(** Caqti-backed Unit of Work.

    Wraps a Caqti connection within a transaction boundary.
    Implements {!Unit_of_work.S}. *)

type t = (module Caqti_eio.CONNECTION)
(** A Caqti connection participating in a transaction. *)

val of_connection : (module Caqti_eio.CONNECTION) -> t
(** Wrap a Caqti connection as a unit of work. The caller is responsible
    for issuing [BEGIN] beforehand (e.g. via [C.start ()]) so that
    {!commit} / {!rollback} land on a real transaction. *)

val commit : t -> (unit, string) result
(** Commit the underlying transaction. *)

val rollback : t -> unit
(** Rollback the underlying transaction. Errors are swallowed: rollback is
    a best-effort cleanup typically run in error paths. *)
