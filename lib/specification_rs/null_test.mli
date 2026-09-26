(** Equality with the null, for the notations in which null is a value.

    In the tree a comparison with null is null, as in SQL: [a = NULL] is true of nothing,
    and that is what keeps the evaluator and the database agreed. But a JSONPath template
    finds a null by [@.a == null], and a host-language predicate by [a = None]: written
    into the tree word for word they would find none, in either reader, and say nothing of
    it. What those notations mean by it is [IS NULL], so that is what their frontends
    build: the rule of SQLAlchemy's [column == None].

    The rule is of constants, not of data: [a = b] with both members null stays null. The
    equality in which null is a value is {!Ast.is}.

    The tree built by hand, {!Ast.equal}, says what it says. *)

(** What the rule needs of a value: whether it is the null. *)
module type NULLABLE = sig
  type t

  val is_null : t -> bool
end

module Make (N : NULLABLE) : sig
  val equal : N.t Ast.t -> N.t Ast.t -> N.t Ast.t
  (** [left = right]; or, if either is the null constant, [IS NULL] of the other. Of two
      nulls the left is tested, and is null: true, as [null == null] is in the notations
      this is for. *)

  val not_equal : N.t Ast.t -> N.t Ast.t -> N.t Ast.t
  (** [left != right]; or, if either is the null constant, [IS NOT NULL] of the other. *)

  val throughout : N.t Ast.t -> N.t Ast.t
  (** [expr] with every equality in it read by {!equal} and {!not_equal}: for a tree whose
      constants were not known when it was built, as those of a template are not until it
      is bound. *)
end
