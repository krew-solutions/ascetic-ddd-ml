module Journal = struct
  type t = { mutable entries : string list (* newest first *) }

  let create () = { entries = [] }
  let entries t = List.rev t.entries
  let clear t = t.entries <- []
  let record t statement = t.entries <- statement :: t.entries
end

module Backend = struct
  (* The double keeps what the server keeps of a transaction: whether one is open, and
     the first statement that failed inside it, which aborted it - every statement after
     it is refused until the transaction ends or is rolled back to a savepoint, and so is
     its close (ADR-0034). *)
  type conn = {
    journal : Journal.t;
    fail : string -> string option;
    mutable transaction : bool;
    mutable aborted : string option;
  }

  let refused reason = Error (Ascetic_session.Driver_error.defect reason)

  let in_aborted =
    "current transaction is aborted, commands ignored until end of transaction block"

  (* A failed statement is not recorded: the journal is what ran. *)
  let statement conn sql =
    match conn.fail sql with
    | Some reason -> refused reason
    | None ->
        Journal.record conn.journal sql;
        Ok ()

  let begin_ conn =
    Result.map
      (fun () ->
        conn.transaction <- true;
        conn.aborted <- None)
      (statement conn "BEGIN")

  let savepoint conn name =
    if Option.is_some conn.aborted then refused in_aborted
    else statement conn ("SAVEPOINT " ^ name)

  let rollback conn =
    Result.map
      (fun () ->
        conn.transaction <- false;
        conn.aborted <- None)
      (statement conn "ROLLBACK")

  let rollback_to conn name =
    Result.map
      (fun () -> conn.aborted <- None)
      (statement conn ("ROLLBACK TO SAVEPOINT " ^ name))

  (* The close of a scope: refused where the transaction was aborted, as the server
     refuses it, naming the statement that aborted it. *)
  let close conn sql =
    match conn.aborted with
    | Some reason ->
        Error (Ascetic_session.Scope.Aborted (Some { sqlstate = None; message = reason }))
    | None ->
        Result.map_error
          (fun reason -> Ascetic_session.Scope.Refused reason)
          (statement conn sql)

  let commit conn =
    let outcome = close conn "COMMIT" in
    (* A refused COMMIT ends the block, as the server has it; an aborted one stays open
       until it is rolled back. *)
    (match outcome with
    | Error (Ascetic_session.Scope.Aborted _) -> ()
    | Ok () | Error (Refused _) -> conn.transaction <- false);
    outcome

  let release conn name = close conn ("RELEASE SAVEPOINT " ^ name)

  (* A statement as a repository double runs it: refused as [fail] says, and the first
     refusal inside a transaction aborts it, as the server does. *)
  let run conn sql =
    match conn.aborted with
    | Some _ -> refused in_aborted
    | None -> (
        match conn.fail sql with
        | Some reason ->
            if conn.transaction then conn.aborted <- Some reason;
            refused reason
        | None ->
            Journal.record conn.journal sql;
            Ok ())
end

include Ascetic_session.Scope.Make (Backend)

let create ?observer ?(fail = fun _ -> None) journal =
  of_conn ?observer { Backend.journal; fail; transaction = false; aborted = None }

let journal t = (conn t).Backend.journal
let record t statement = Journal.record (journal t) statement
let run t statement = Backend.run (conn t) statement
