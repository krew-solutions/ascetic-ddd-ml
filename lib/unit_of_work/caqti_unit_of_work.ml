(** Caqti-backed Unit of Work.

    Wraps a Caqti connection within a transaction boundary.
    All repository operations within a single UoW share the same
    database transaction. *)

type t = (module Caqti_eio.CONNECTION)

let of_connection conn = conn

let commit (module C : Caqti_eio.CONNECTION) =
  match C.commit () with
  | Ok () -> Ok ()
  | Error err -> Error (Format.asprintf "%a" Caqti_error.pp err)

let rollback (module C : Caqti_eio.CONNECTION) =
  match C.rollback () with
  | Ok () -> ()
  | Error _ -> ()
