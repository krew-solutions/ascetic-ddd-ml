(** The reader that decides, in memory, whether a candidate satisfies a specification: the
    sources' [EvaluateVisitor].

    The logic is PostgreSQL's, so that this reader and the database reading the compiled
    SQL agree on every candidate, nulls included:

    - a comparison or a computation with a null operand is null;
    - [AND], [OR], [NOT] are three-valued: [NULL AND FALSE] is false, [NULL OR TRUE] is
      true, [NOT NULL] is null;
    - [IS], [IS NULL], [IS NOT NULL] and [any] are true or false, never null;
    - a candidate satisfies a specification whose value is true; a null does not, as a row
      with a null [WHERE] is not selected.

    [AND] and [OR] do not evaluate their right side once the left has decided the result,
    and [any] stops at its first witness, as the same operators do in OCaml, Python and
    Go. The sources evaluate everything; the difference shows only where the part not
    evaluated would have failed. *)

(** A specification could not be evaluated. *)
type error =
  | Context of Context.error  (** The candidate has no such member. *)
  | No_current_item  (** A path from the item under test, outside any collection. *)
  | Not_boolean of string
      (** A truth value was needed, by [NOT], [AND], [OR], as the predicate of [any], as
          the result, and a value of this kind was found. *)
  | Operand of Operand.error  (** An operator could not be applied to its operands. *)
[@@deriving show, eq]

val error_to_string : error -> string

(** The evaluator over values of one type. *)
module Make (O : Operand.S) : sig
  val is_satisfied_by : O.t Ast.t -> O.t Context.t -> (bool, error) result
  (** Whether [candidate] satisfies [specification]: whether its value is true. False and
      null both say no. *)

  val evaluate : O.t Ast.t -> O.t Context.t -> (O.t, error) result
  (** The value of [expr] for [candidate]. *)
end
