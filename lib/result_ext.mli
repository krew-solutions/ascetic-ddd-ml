(** Railway-Oriented Programming extensions for [Result].

    Provides monadic binding operators and utility functions for composing
    operations that may fail, forming a clean "railway" of success/error paths. *)

(** {1 Binding Operators} *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** Monadic bind for [Result]. Chains computations that may fail.
    If the left side is [Error], short-circuits immediately. *)

val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** Map over the success value. *)

val ( and* ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
(** Combine two results. Returns the first [Error] encountered. *)

val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
(** Alias for [and*]. *)

(** {1 Combinators} *)

val map_error : ('e1 -> 'e2) -> ('a, 'e1) result -> ('a, 'e2) result
(** Transform the error side of a result. *)

val flat_map : ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result
(** [flat_map f r] is [Result.bind r f] with arguments flipped for piping. *)

val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
(** [traverse f xs] applies [f] to each element of [xs], collecting results.
    Returns [Error] on the first failure, short-circuiting. *)

val sequence : ('a, 'e) result list -> ('a list, 'e) result
(** [sequence rs] collects a list of results into a result of list.
    Returns [Error] on the first failure. *)

val or_else : ('a, 'e) result -> (unit -> ('a, 'e) result) -> ('a, 'e) result
(** [or_else r f] returns [r] if [Ok], otherwise evaluates [f ()]. *)

val tap : ('a -> unit) -> ('a, 'e) result -> ('a, 'e) result
(** [tap f r] applies [f] to the success value for side effects,
    then returns [r] unchanged. *)

val tap_error : ('e -> unit) -> ('a, 'e) result -> ('a, 'e) result
(** [tap_error f r] applies [f] to the error value for side effects,
    then returns [r] unchanged. *)

val to_option : ('a, 'e) result -> 'a option
(** Discard the error, returning [Some] on success, [None] on failure. *)

val of_option : error:'e -> 'a option -> ('a, 'e) result
(** Convert an [option] to a [result], using [error] for [None]. *)

val guard : bool -> error:'e -> (unit, 'e) result
(** [guard condition ~error] returns [Ok ()] if [condition] is true,
    [Error error] otherwise. Useful for precondition checks. *)
