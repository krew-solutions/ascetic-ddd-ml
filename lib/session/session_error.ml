(** A failure of the session machinery itself, as opposed to a failure of the work done
    inside a scope.

    A scope returns its own error type; a session only asks how to carry one of these in
    it, through the [lift] argument of [atomic]. The reasons are {!Driver_error.t}: the
    text the driver rendered, and whether the failure is of the moment. The port stays
    free of driver types. *)

type t =
  | Acquire of Driver_error.t  (** No connection could be taken from the pool. *)
  | Begin of Driver_error.t  (** [BEGIN] or [SAVEPOINT] failed. *)
  | Commit of Driver_error.t
      (** [COMMIT] or [RELEASE SAVEPOINT] failed. A failure of the whole scope: the caller
          believes the work is durable, and it is not. *)
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
    reason of the moment, or a scope boundary failed for one. A scope opened twice is a
    defect of the caller. *)
let is_transient = function
  | Acquire reason | Begin reason | Commit reason | Abandoned reason -> reason.transient
  | Scope_already_open -> false

let pp ppf = function
  | Acquire reason ->
      Format.fprintf ppf "cannot acquire a connection: %a" Driver_error.pp reason
  | Begin reason -> Format.fprintf ppf "cannot open a scope: %a" Driver_error.pp reason
  | Commit reason -> Format.fprintf ppf "cannot commit a scope: %a" Driver_error.pp reason
  | Scope_already_open ->
      Format.pp_print_string ppf
        "a scope is already open on this session: open a nested scope on the session the \
         outer scope handed out, or use another session"
  | Abandoned reason ->
      Format.fprintf ppf
        "a scope could not be rolled back and its transaction is abandoned: %a"
        Driver_error.pp reason

let to_string error = Format.asprintf "%a" pp error
