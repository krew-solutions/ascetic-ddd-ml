module Backend = struct
  type conn = (module Caqti_eio.CONNECTION)

  (* A driver call returns its error, or raises one of the client library's
     on a connection whose server is gone: either way the scope gets an
     error, never an exception, from a failure of the driver. *)
  let call f =
    Transient.protect ~raised:Fun.id (fun () ->
        Result.map_error Transient.driver_error (f ()))

  let begin_ (module C : Caqti_eio.CONNECTION) = call (fun () -> C.start ())
  let commit (module C : Caqti_eio.CONNECTION) = call (fun () -> C.commit ())
  let rollback (module C : Caqti_eio.CONNECTION) = call (fun () -> C.rollback ())

  (* Savepoint statements carry a name, so each is a one-shot request rather
     than an entry in the driver's prepared-statement cache. *)
  let exec (module C : Caqti_eio.CONNECTION) sql =
    let open Caqti_request.Infix in
    let request = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql in
    call (fun () -> C.exec request ())

  let savepoint conn name = exec conn ("SAVEPOINT " ^ name)
  let release conn name = exec conn ("RELEASE SAVEPOINT " ^ name)
  let rollback_to conn name = exec conn ("ROLLBACK TO SAVEPOINT " ^ name)
end

include Ascetic_session.Scope.Make (Backend)

let create ?observer conn = of_conn ?observer conn
let connection = conn
