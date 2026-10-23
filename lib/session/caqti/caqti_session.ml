module Scope = Ascetic_session.Scope

module Backend = struct
  (* The driver's connection, the one the session hands out, which remembers the failures
     of the statements run through it, and the memory itself. The session's own statements
     go to the driver's: the memory is of the scope's statements alone (ADR-0034). *)
  type conn = {
    driver : (module Caqti_eio.CONNECTION);
    handed_out : (module Caqti_eio.CONNECTION);
    memory : Remembering.t;
  }

  (* A driver call returns its error, or raises one of the client library's on a
     connection whose server is gone: either way the scope gets an error, never an
     exception, from a failure of the driver. The error is kept as the driver gave it,
     for the close to read its SQLSTATE. *)
  let attempt f =
    Transient.protect
      ~raised:(fun reason -> `Raised reason)
      (fun () -> Result.map_error (fun error -> `Driver error) (f ()))

  let reason = function
    | `Driver error -> Transient.driver_error error
    | `Raised reason -> reason

  let call f = Result.map_error reason (attempt f)

  (* Savepoint statements carry a name, so each is a one-shot request rather than an
     entry in the driver's prepared-statement cache. *)
  let exec (module C : Caqti_eio.CONNECTION) sql =
    let open Caqti_request.Infix in
    let request = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql in
    C.exec request ()

  (* A failure before the transaction began aborted nothing in it. *)
  let begin_ conn =
    let module C = (val conn.driver) in
    Result.map (fun () -> Remembering.forget conn.memory) (call (fun () -> C.start ()))

  let savepoint conn name = call (fun () -> exec conn.driver ("SAVEPOINT " ^ name))

  (* A rollback ends what the memory was of: the transaction, or the part of it since the
     savepoint, which the failure remembered aborted. *)
  let rollback conn =
    let module C = (val conn.driver) in
    Remembering.forget conn.memory;
    call (fun () -> C.rollback ())

  let rollback_to conn name =
    Remembering.forget conn.memory;
    call (fun () -> exec conn.driver ("ROLLBACK TO SAVEPOINT " ^ name))

  (* [in_failed_sql_transaction]: the server's refusal of any statement in a transaction
     a statement aborted, until the block ends. *)
  let in_failed_transaction : [< Caqti_error.t ] -> bool = function
    | `Request_failed ({ msg; _ } : Caqti_error.query_error)
    | `Response_failed ({ msg; _ } : Caqti_error.query_error) -> (
        match msg with
        | Caqti_driver_postgresql.Result_error_msg { sqlstate = "25P02"; _ } -> true
        | _ -> false)
    | _ -> false

  (* The close of a scope that succeeded, as the server answered it: refused with 25P02
     in an aborted transaction, which is [Aborted] with the failure the connection
     remembers; refused for a reason of its own; or done. Either way the memory of the
     transaction ends with it. *)
  let closing conn = function
    | Ok () ->
        Remembering.forget conn.memory;
        Ok ()
    | Error (`Driver error) when in_failed_transaction error ->
        Error (Scope.Aborted (Remembering.take conn.memory))
    | Error refusal ->
        Remembering.forget conn.memory;
        Error (Scope.Refused (reason refusal))

  (* A statement the server refuses with 25P02 in an aborted transaction and answers in a
     whole one: what tells the two apart, since a [COMMIT] in an aborted transaction is
     answered with a rollback and no error, and the driver keeps the answer's tag to
     itself. One round trip of its own: the driver sends every request through the
     extended protocol, which takes one statement at a time, so the probe cannot share
     the commit's message as the reference's does. *)
  let probe conn =
    let module C = (val conn.driver) in
    let open Caqti_request.Infix in
    let request = (Caqti_type.unit ->! Caqti_type.int) ~oneshot:true "SELECT 1" in
    attempt (fun () -> Result.map ignore (C.find request ()))

  (* The server alone knows whether the transaction is still whole, and is asked where
     there is reason to doubt it: a statement run through the connection failed since
     the transaction began. Where none did the transaction is whole by the server's own
     rule - it is aborted by a failed statement and by nothing else - and the commit goes
     without the probe. *)
  let commit conn =
    let module C = (val conn.driver) in
    let whole =
      match Remembering.remembered conn.memory with
      | None -> Ok ()
      | Some _ -> probe conn
    in
    closing conn
      (match whole with
      | Ok () -> attempt (fun () -> C.commit ())
      | Error _ as refused -> refused)

  (* A [RELEASE SAVEPOINT] in an aborted transaction is refused with 25P02 by itself. *)
  let release conn name =
    closing conn (attempt (fun () -> exec conn.driver ("RELEASE SAVEPOINT " ^ name)))
end

include Scope.Make (Backend)

let create ?observer driver =
  let memory = Remembering.create () in
  of_conn ?observer
    { Backend.driver; handed_out = Remembering.wrap memory driver; memory }

let connection t = (conn t).Backend.handed_out
