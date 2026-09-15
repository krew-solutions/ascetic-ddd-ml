module Backend = struct
  type conn = (module Caqti_eio.CONNECTION)

  let show error = Caqti_error.show error
  let begin_ (module C : Caqti_eio.CONNECTION) = Result.map_error show (C.start ())
  let commit (module C : Caqti_eio.CONNECTION) = Result.map_error show (C.commit ())
  let rollback (module C : Caqti_eio.CONNECTION) = Result.map_error show (C.rollback ())

  (* Savepoint statements carry a name, so each is a one-shot request rather
     than an entry in the driver's prepared-statement cache. *)
  let exec (module C : Caqti_eio.CONNECTION) sql =
    let open Caqti_request.Infix in
    let request = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql in
    Result.map_error show (C.exec request ())

  let savepoint conn name = exec conn ("SAVEPOINT " ^ name)
  let release conn name = exec conn ("RELEASE SAVEPOINT " ^ name)
  let rollback_to conn name = exec conn ("ROLLBACK TO SAVEPOINT " ^ name)

  (* A disconnected connection fails the pool's validation and is dropped;
     the server ends the abandoned transaction with the socket. *)
  let discard (module C : Caqti_eio.CONNECTION) = C.disconnect ()
end

include Ascetic_session.Scope.Make (Backend)

let create ?observer conn = of_conn ?observer conn
let connection = conn
