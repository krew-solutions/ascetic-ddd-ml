(** The operators of a specification.

    An operator is grouped by what it needs of its operands, because that is how every
    reader of the tree tells them apart: a comparison needs values that can be compared,
    arithmetic needs values that can be computed with, a logical connective needs truth
    values and nothing of the values themselves. The grouping is in the type, so a
    reader's [match] is total without a catch-all case.

    Precedence and associativity are not here. A tree needs neither: its shape already
    says what applies to what. They belong to a notation: the JSONPath parser has its own
    to read with, the PostgreSQL compiler its own to write with. *)

(** An operator written before its operand. *)
type prefix =
  | Not  (** Logical negation, [NOT x]. *)
  | Neg  (** Arithmetic negation, [-x]. *)
[@@deriving show, eq, ord]

(** An operator written after its operand. *)
type postfix =
  | Is_null  (** [x IS NULL]: true or false, never unknown. *)
  | Is_not_null  (** [x IS NOT NULL]: true or false, never unknown. *)
[@@deriving show, eq, ord]

(** A comparison of two values: unknown if either is null. *)
type comparison = Eq | Ne | Gt | Lt | Ge | Le [@@deriving show, eq, ord]

(** A logical connective, in three-valued logic. *)
type logical =
  | And  (** [AND]: false if either side is false, even beside an unknown. *)
  | Or  (** [OR]: true if either side is true, even beside an unknown. *)
[@@deriving show, eq, ord]

(** A computation on two values: null if either is null. *)
type arithmetic =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Shl  (** [<<], a shift of the bits of an integer. *)
  | Shr  (** [>>], a shift of the bits of an integer. *)
[@@deriving show, eq, ord]

(** An operator written between its operands. *)
type infix =
  | Comparison of comparison
  | Is
      (** Equality that treats null as a value: two nulls are equal, a null and a value
          are not. True or false, never unknown. *)
  | Logical of logical
  | Arithmetic of arithmetic
[@@deriving show, eq, ord]

(** {1 The infix operators by name} *)

val eq : infix
val ne : infix
val gt : infix
val lt : infix
val ge : infix
val le : infix
val is : infix
val and_ : infix
val or_ : infix
val add : infix
val sub : infix
val mul : infix
val div : infix
val modulo : infix
val shl : infix
val shr : infix

(** {1 How an operator is spelled} *)

val prefix_to_string : prefix -> string
(** [NOT], [-]. *)

val postfix_to_string : postfix -> string
(** [IS NULL], [IS NOT NULL]. *)

val comparison_to_string : comparison -> string
val logical_to_string : logical -> string
val arithmetic_to_string : arithmetic -> string

val infix_to_string : infix -> string
(** [=], [IS], [AND], [+], ... *)
