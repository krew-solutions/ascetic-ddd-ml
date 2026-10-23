(** A failure of the session machinery itself, as opposed to a failure of the work done
    inside a scope.

    A scope returns its own error type; a session only asks how to carry one of these in
    it, through the [lift] argument of [atomic]. The reasons are {!Driver_error.t}: the
    text the driver rendered, and whether the failure is of the moment. The port stays
    free of driver types.

    Whether another attempt may succeed is {!is_transient}, answered without the driver:
    the backend that met the failure classifies it where it can see the driver's error,
    and the verdict travels with it, so that an application layer which never names a
    driver can tell a moment from a defect. *)

type cause = { sqlstate : string option; message : string }
(** The statement failure that aborted a transaction, as the server reported it: what an
    absorbed failure was, so that its defect can be found. The SQLSTATE, [23505] for a
    unique violation, where the driver gives it; and the server's message. *)

type t =
  | Acquire of Driver_error.t  (** No connection could be taken from the pool. *)
  | Begin of Driver_error.t  (** [BEGIN] or [SAVEPOINT] failed. *)
  | Commit of Driver_error.t
      (** [COMMIT] or [RELEASE SAVEPOINT] failed for a reason of its own: the server
          refused the commit, a deferred constraint, a serialization failure, or the
          connection was lost on the way. The scope was rolled back; nothing it did is
          durable, where the caller believed it was. *)
  | Aborted of cause option
      (** The scope returned success, but a statement inside it had failed and the
          failure was absorbed: the server aborts a transaction on the first failed
          statement, so there was nothing left to commit. The scope was rolled back - the
          whole transaction, or a nested scope back to its savepoint, the enclosing
          transaction going on - and nothing it did is durable (ADR-0034).

          Always a defect of the code that absorbed the failure, whatever the failure was:
          a refusal meant to be absorbed is absorbed in a nested scope of its own, whose
          rollback undoes that statement alone. The cause is the failure as the backend
          remembers it, where it saw it. *)
  | Scope_already_open
      (** A second scope was opened on a session that already has one open. Nesting is
          expressed by opening a scope on the session the previous scope handed out, not
          on the one that opened it: two scopes side by side on one connection would share
          one savepoint stack, and releasing the older one silently destroys the newer. *)
  | Abandoned of Driver_error.t
      (** A rollback failed or was cut short, so the state of the transaction on the
          connection is unknown. Nothing may be committed on it again: every further scope
          of the session is refused, and an enclosing scope is refused at commit and rolls
          back on its way out. A connection whose server is gone fails the pool's check
          and is dropped. The reason is the one the rollback gave. *)

(** Whether the failure is of the moment, so that a loop meeting it waits and goes on with
    a fresh session rather than stopping: the pool could not hand out a connection for a
    reason of the moment, or a scope boundary failed for one. An absorbed failure never
    is, whatever it was: retrying runs the same code, which absorbs it again. A scope
    opened twice is a defect of the caller. *)
let is_transient = function
  | Acquire reason | Begin reason | Commit reason | Abandoned reason -> reason.transient
  | Aborted _ | Scope_already_open -> false

let pp_cause ppf { sqlstate; message } =
  match sqlstate with
  | Some code -> Format.fprintf ppf "%s (%s)" message code
  | None -> Format.pp_print_string ppf message

let pp ppf = function
  | Acquire reason ->
      Format.fprintf ppf "cannot acquire a connection: %a" Driver_error.pp reason
  | Begin reason -> Format.fprintf ppf "cannot open a scope: %a" Driver_error.pp reason
  | Commit reason -> Format.fprintf ppf "cannot commit a scope: %a" Driver_error.pp reason
  | Aborted cause -> (
      Format.pp_print_string ppf
        "a statement failed inside the scope and the scope still reported success: the \
         transaction was aborted and rolled back, nothing is durable";
      match cause with
      | Some cause -> Format.fprintf ppf "; the failure absorbed: %a" pp_cause cause
      | None -> ())
  | Scope_already_open ->
      Format.pp_print_string ppf
        "a scope is already open on this session: open a nested scope on the session the \
         outer scope handed out, or use another session"
  | Abandoned reason ->
      Format.fprintf ppf
        "a scope could not be rolled back and its transaction is abandoned: %a"
        Driver_error.pp reason

let to_string error = Format.asprintf "%a" pp error
