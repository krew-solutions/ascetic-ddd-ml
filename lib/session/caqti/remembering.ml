type t = { mutable failure : Ascetic_session.Session_error.cause option }

let create () = { failure = None }
let remembered t = t.failure
let forget t = t.failure <- None

let take t =
  let failure = t.failure in
  t.failure <- None;
  failure

(* The failure as the server reported it: the PostgreSQL driver keeps the SQLSTATE and
   the server's message, which ends in a newline; another driver's message is rendered as
   it is. *)
let cause_of_msg : Caqti_error.msg -> Ascetic_session.Session_error.cause = function
  | Caqti_driver_postgresql.Result_error_msg { sqlstate; error_message } ->
      { sqlstate = Some sqlstate; message = String.trim error_message }
  | msg -> { sqlstate = None; message = Format.asprintf "%a" Caqti_error.pp_msg msg }

(* The first failure is the one that aborted the transaction: every later statement in
   it fails for that reason, 25P02, which the first failure explains. The driver runs one
   statement at a time on a connection, so the first failure reported is the first that
   happened. *)
let remember t ({ msg; _ } : Caqti_error.query_error) =
  if Option.is_none t.failure then t.failure <- Some (cause_of_msg msg)

(* The outcome of a request, its failure remembered. A request's own failures are
   [Request_failed]; a retrieval's, [Response_failed] as well. Each matches the tags of
   its class and no other, so the row of the error stays the driver's. *)
let on_call t outcome =
  (match outcome with
  | Error (`Request_failed failure) -> remember t failure
  | _ -> ());
  outcome

let on_retrieval t outcome =
  (match outcome with
  | Error (`Request_failed failure) | Error (`Response_failed failure) ->
      remember t failure
  | _ -> ());
  outcome

let wrap t (module C : Caqti_eio.CONNECTION) : (module Caqti_eio.CONNECTION) =
  (module struct
    include C

    let call ~f request param = on_call t (C.call ~f request param)
    let set_statement_timeout timeout = on_call t (C.set_statement_timeout timeout)
    let start () = on_retrieval t (C.start ())
    let commit () = on_retrieval t (C.commit ())
    let rollback () = on_retrieval t (C.rollback ())
    let deallocate request = on_call t (C.deallocate request)
    let exec request param = on_retrieval t (C.exec request param)

    let exec_with_affected_count request param =
      on_retrieval t (C.exec_with_affected_count request param)

    let find request param = on_retrieval t (C.find request param)
    let find_opt request param = on_retrieval t (C.find_opt request param)
    let fold request f param acc = on_retrieval t (C.fold request f param acc)
    let fold_s request f param acc = on_retrieval t (C.fold_s request f param acc)
    let iter_s request f param = on_retrieval t (C.iter_s request f param)
    let collect_list request param = on_retrieval t (C.collect_list request param)
    let rev_collect_list request param = on_retrieval t (C.rev_collect_list request param)
    let with_transaction f = on_retrieval t (C.with_transaction f)

    let populate ~table ~columns row_type data =
      on_retrieval t (C.populate ~table ~columns row_type data)
  end)
