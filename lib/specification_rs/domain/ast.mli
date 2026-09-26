(** The tree of a specification, and the functions that build it.

    The Python and Go sources describe the tree as classes with [accept] and read it
    through a [Visitor]. A visitor is how a language without sum types spells a [match];
    here the tree is a sum type, {!t}, and a reader is a function that matches on it. New
    readers need no change here, which is what the visitor was for.

    The tree is generic in its values. ['v t] says nothing of ['v]; the readers ask for
    what they need: the evaluator an {!Operand.S}, the PostgreSQL compiler a
    {!Pg.PARAM_TYPE}. So one tree type serves every stage a specification goes through,
    and the stage is in the type: a template with unbound placeholders is a
    [Jsonpath.Slot.t t], a bound one a [Value.t t]; a specification over domain values is
    a ['d t], the same over column values an ['s t]. {!try_map_values} goes from one to
    the next. *)

(** A specification: a tree over values of type ['v]. *)
type 'v t =
  | Value of 'v  (** A constant. *)
  | Field of Path.t  (** The value of a member. *)
  | Prefix of Operator.prefix * 'v t  (** An operator before its operand. *)
  | Infix of 'v t * Operator.infix * 'v t  (** An operator between its operands. *)
  | Postfix of 'v t * Operator.postfix  (** An operator after its operand. *)
  | Any of Path.t * 'v t
      (** At least one item of the collection at the path satisfies the predicate, in
          which [Path.Item] is that item. True or false, never unknown: an item whose
          predicate is unknown is not a witness. The sources call this node [Wildcard],
          after JSONPath's [[*]]. *)
[@@deriving show, eq]

val try_map_values : ('v -> ('w, 'e) result) -> 'v t -> ('w t, 'e) result
(** The same tree over other values: each constant goes through [f], in order, left to
    right; the first failure is the result. *)

(** {1 Building a tree} *)

val value : 'v -> 'v t
(** The constant [value]. *)

val field_at : Path.t -> 'v t
(** The value of the member at [path]. *)

val field : string -> 'v t
(** The value of the member at the dotted path from the candidate: [field "a.b.c"]. *)

val item : string -> 'v t
(** The value of the member [name] of the item under test. *)

val outer : int -> string -> 'v t
(** The value of the member [name] of the item [up] collections out. *)

val not_ : 'v t -> 'v t
(** [NOT operand]. *)

val neg : 'v t -> 'v t
(** [-operand]. *)

val infix : 'v t -> Operator.infix -> 'v t -> 'v t
(** [left op right]. *)

val eq : 'v t -> 'v t -> 'v t
(** [left = right]. *)

val ne : 'v t -> 'v t -> 'v t
(** [left != right]. *)

val gt : 'v t -> 'v t -> 'v t
(** [left > right]. *)

val lt : 'v t -> 'v t -> 'v t
(** [left < right]. *)

val ge : 'v t -> 'v t -> 'v t
(** [left >= right]. *)

val le : 'v t -> 'v t -> 'v t
(** [left <= right]. *)

val is : 'v t -> 'v t -> 'v t
(** [left IS right]: equality in which null is a value. *)

val and_ : 'v t -> 'v t -> 'v t
(** [left AND right]. *)

val or_ : 'v t -> 'v t -> 'v t
(** [left OR right]. *)

val and_all : 'v t -> 'v t list -> 'v t
(** [first AND second AND ...], nested to the left, as the sources' variadic [And] nests
    it; [first] alone if there is no other. *)

val or_all : 'v t -> 'v t list -> 'v t
(** [first OR second OR ...], nested to the left; [first] alone if there is no other. *)

val add : 'v t -> 'v t -> 'v t
(** [left + right]. *)

val sub : 'v t -> 'v t -> 'v t
(** [left - right]. *)

val mul : 'v t -> 'v t -> 'v t
(** [left * right]. *)

val div : 'v t -> 'v t -> 'v t
(** [left / right]. *)

val modulo : 'v t -> 'v t -> 'v t
(** [left % right]. *)

val left_shift : 'v t -> 'v t -> 'v t
(** [left << right]. *)

val right_shift : 'v t -> 'v t -> 'v t
(** [left >> right]. *)

val is_null : 'v t -> 'v t
(** [operand IS NULL]. *)

val is_not_null : 'v t -> 'v t
(** [operand IS NOT NULL]. *)

val any_at : Path.t -> 'v t -> 'v t
(** Some item of the collection at [source] satisfies [predicate]. *)

val any : string -> 'v t -> 'v t
(** {!any_at} of the dotted path from the candidate. *)

val all_at : Path.t -> 'v t -> 'v t
(** No item of the collection at [source] fails [predicate]: written as
    [NOT any(source, NOT predicate)], which is what it means, so that the tree needs no
    second quantifier and no reader a second case. An item whose predicate is unknown does
    not fail it: the same reading SQL gives [NOT EXISTS (... WHERE NOT predicate)]. *)

val all : string -> 'v t -> 'v t
(** {!all_at} of the dotted path from the candidate. *)
