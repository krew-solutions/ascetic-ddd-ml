(** A failure of the session machinery itself, as opposed to a failure of the work done
    inside a scope.

    A scope returns its own error type; a session only asks how to carry one of these in
    it, through the [lift] argument of [atomic]. The reasons are text rendered by the
    driver: the port stays free of driver types. *)

type t =
  | Acquire of string  (** No connection could be taken from the pool. *)
  | Begin of string  (** [BEGIN] or [SAVEPOINT] failed. *)
  | Commit of string
      (** [COMMIT] or [RELEASE SAVEPOINT] failed. A failure of the whole scope: the caller
          believes the work is durable, and it is not. *)
  | Scope_already_open
      (** A second scope was opened on a session that already has one open. Nesting is
          expressed by opening a scope on the session the previous scope handed out, not
          on the one that opened it: two scopes side by side on one connection would share
          one savepoint stack, and releasing the older one silently destroys the newer. *)
  | Abandoned of string
      (** A rollback failed or was cut short, so the state of the transaction on the
          connection is unknown. Nothing may be committed on it again: every further scope
          is refused, an enclosing scope is refused at commit, and the connection is
          discarded rather than reused. The text is the reason the rollback gave. *)

let pp ppf = function
  | Acquire reason -> Format.fprintf ppf "cannot acquire a connection: %s" reason
  | Begin reason -> Format.fprintf ppf "cannot open a scope: %s" reason
  | Commit reason -> Format.fprintf ppf "cannot commit a scope: %s" reason
  | Scope_already_open ->
      Format.pp_print_string ppf
        "a scope is already open on this session: open a nested scope on the session the \
         outer scope handed out, or use another session"
  | Abandoned reason ->
      Format.fprintf ppf
        "a scope could not be rolled back and its transaction is abandoned: %s" reason

let to_string error = Format.asprintf "%a" pp error
