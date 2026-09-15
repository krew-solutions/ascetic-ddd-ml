module Journal = struct
  type t = { mutable entries : string list (* newest first *) }

  let create () = { entries = [] }
  let entries t = List.rev t.entries
  let clear t = t.entries <- []
  let record t statement = t.entries <- statement :: t.entries
end

module Backend = struct
  type conn = { journal : Journal.t; fail : string -> string option }

  (* A failed statement is not recorded: the journal is what ran. *)
  let statement conn sql =
    match conn.fail sql with
    | Some reason -> Error reason
    | None ->
        Journal.record conn.journal sql;
        Ok ()

  let begin_ conn = statement conn "BEGIN"
  let commit conn = statement conn "COMMIT"
  let rollback conn = statement conn "ROLLBACK"
  let savepoint conn name = statement conn ("SAVEPOINT " ^ name)
  let release conn name = statement conn ("RELEASE SAVEPOINT " ^ name)
  let rollback_to conn name = statement conn ("ROLLBACK TO SAVEPOINT " ^ name)
  let discard _ = ()
end

include Ascetic_session.Scope.Make (Backend)

let create ?observer ?(fail = fun _ -> None) journal =
  of_conn ?observer { Backend.journal; fail }

let journal t = (conn t).Backend.journal
let record t statement = Journal.record (journal t) statement
