(** What the evaluator needs of a value.

    The Go source keeps a registry of operator implementations keyed by the dynamic types
    of the operands, with a fallback to interfaces a Value Object may implement
    ([EqualOperand], [LessThanOperand], ...); the Python source leans on duck typing. Both
    are how an open set of value types is served when a value is [any]. Here the value
    type is a parameter of the tree, so the set is open the static way: a type is usable
    in an evaluated specification if it is an {!S}. {!Value} is; a domain that wants its
    Value Objects as constants wraps [Value.t] and its own types in a variant and
    implements the signature for that, delegating the scalars.

    The signature holds only what depends on the value type. Null propagation,
    three-valued [AND]/[OR]/[NOT], [IS], [IS NULL] are the same for every value type, and
    the evaluator does them once; a function here is never called with a null. *)

(** An operator could not be applied. *)
type error =
  | Unsupported of { operator : string; left : string; right : string option }
      (** The operator, as it is spelled, is not defined for operands of these kinds: the
          kind of the left, or only, operand, and of the right one if there is one. *)
  | Division_by_zero  (** A division, or a remainder, by zero. *)
  | Out_of_range  (** The result does not fit the type. *)
[@@deriving show, eq]

val unsupported : string -> string -> string -> error
(** [unsupported operator left right]: [operator] is not defined between a [left] and a
    [right]. *)

val unsupported_unary : string -> string -> error
(** [unsupported_unary operator operand]: [operator] is not defined for an [operand]. *)

val error_to_string : error -> string

(** A value a specification can be evaluated over. *)
module type S = sig
  type t

  val null : t
  (** The null. *)

  val is_null : t -> bool
  (** Whether this is the null. *)

  val of_bool : bool -> t
  (** The truth value [value]. *)

  val to_bool : t -> bool option
  (** The truth value this is, if it is one. *)

  val kind : t -> string
  (** What kind of value this is, for an error to name: ["integer"]. *)

  val equals : t -> t -> (bool, error) result
  (** Whether the two are equal. Go's [EqualOperand]. *)

  val compare : t -> t -> (int, error) result
  (** How the two are ordered: negative, zero or positive, as [compare] has it. Go's
      [LessThanOperand] and its kin. *)

  val negate : t -> (t, error) result
  (** [-value]. *)

  val compute : Operator.arithmetic -> t -> t -> (t, error) result
  (** [compute op left right] is [left op right]. *)
end
