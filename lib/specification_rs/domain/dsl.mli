(** A specification built from typed terms: the sources' [public] package.

    The tree takes any operand anywhere: [and_ (value (Int 1L)) (value (Text "x"))] is a
    tree, and only evaluating it shows that it means nothing. A term knows what sort of
    value it stands for, so such a tree is not built: only booleans are combined with
    [&&:], [||:], [not_]; only numbers are multiplied; a number is compared with a number;
    [is_null] is offered only by a term declared nullable. What a sort does not have does
    not type-check.

    {[
    open Ascetic_specification.Dsl

    let price = Number.field "price"
    let discount = Number.field "discount"
    let cheap = lt (sub price discount) (Number.of_int 100)

    let listed =
      and_ (Boolean.field "active") (is_null (Null_datetime.field "deleted_at"))

    let specification =
      expr (or_ (and_ cheap listed) (eq (Text.field "tag") (Text.value "sale")))

    (* Or, with the operators of {!Infix} opened where a specification is written: *)
    let specification =
      expr
        Infix.(
          (price - discount < Number.of_int 100 && listed)
          || Text.field "tag" = Text.value "sale")
    ]}

    Python overloads the comparison operators; Go, which cannot, names them. Here every
    operator is a named function, and {!Infix} redefines OCaml's own operators over terms
    for a scope that opens it: OCaml gives an operator its precedence by its first
    character, so [&&] and [||] are the only spellings that bind as [AND] and [OR] do. *)

(** {1 The sorts} *)

type boolean = [ `Boolean ]
type number = [ `Number ]
type text = [ `Text ]
type datetime = [ `Datetime ]
type timespan = [ `Timespan ]
type date = [ `Date ]
type uuid = [ `Uuid ]

type comparable = [ number | text | datetime | date | timespan | uuid ]
(** A sort whose values can be compared with each other. *)

type negatable = [ number | timespan ]
(** A sort that has a negation. *)

type not_null
(** Declared never null: the term has no [is_null]. *)

type nullable
(** Declared nullable: the term has [is_null] and [is_not_null]. *)

type ('s, 'n) t
(** A part of a specification that stands for a value of sort ['s], declared nullable or
    not by ['n]: the sources' [Delegating]. *)

type ('s, 'n) term = ('s, 'n) t
(** The same, under a name the sort modules below can use beside their own [t]. *)

val field_at : Path.t -> ('s, 'n) t
(** The member at [path], declared to be of whatever sort and nullability its use asks
    for: the sources' [make_field]. The sort modules below fix them. *)

val field : string -> ('s, 'n) t
(** {!field_at} of the dotted path from the candidate. *)

val of_expr : Value.t Ast.t -> ('s, 'n) t
(** [expr], declared to be of this sort. Nothing checks the declaration: this is the way
    in for a tree built by other means - an {!Ast.any}, say, which is a boolean. *)

val expr : ('s, 'n) t -> Value.t Ast.t
(** The tree built so far: the sources' [delegate]. *)

(** {1 Nullability} *)

val is_null : ('s, nullable) t -> (boolean, not_null) t
(** [term IS NULL]. *)

val is_not_null : ('s, nullable) t -> (boolean, not_null) t
(** [term IS NOT NULL]. *)

(** {1 Booleans} *)

val not_ : (boolean, 'n) t -> (boolean, not_null) t
val and_ : (boolean, 'n) t -> (boolean, 'n2) t -> (boolean, not_null) t
val or_ : (boolean, 'n) t -> (boolean, 'n2) t -> (boolean, not_null) t

val is : (boolean, 'n) t -> (boolean, 'n2) t -> (boolean, not_null) t
(** [left IS right]: equality in which null is a value. *)

(** {1 Comparisons} *)

val eq : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
(** [left = right]; [left IS NULL] if [right] is the null constant - a nullable value of
    [None] - as {!Null_test} has it. *)

val ne : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
(** [left != right]; [left IS NOT NULL] if [right] is the null constant. *)

val gt : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
val lt : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
val ge : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
val le : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t

(** {1 Numbers} *)

val add : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val sub : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val mul : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val div : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val modulo : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val shl : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
val shr : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t

val neg : (([< negatable ] as 's), 'n) t -> ('s, not_null) t
(** [-term], of a number or of a span of time. *)

(** {1 The operators}

    OCaml's operators over terms, for the scope of an [Infix.( ... )]: they bind as OCaml
    binds them, which for these is as SQL does - [*] over [+] over a comparison over [&&]
    over [||]. The shifts have no operator: OCaml's [lsl] and [asr] bind tighter than [*],
    where SQL's [<<] binds looser than [+]. *)
module Infix : sig
  val ( && ) : (boolean, 'n) t -> (boolean, 'n2) t -> (boolean, not_null) t
  val ( || ) : (boolean, 'n) t -> (boolean, 'n2) t -> (boolean, not_null) t
  val ( = ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( <> ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( > ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( < ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( >= ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( <= ) : (([< comparable ] as 's), 'n) t -> ('s, 'n2) t -> (boolean, not_null) t
  val ( + ) : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
  val ( - ) : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
  val ( * ) : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
  val ( / ) : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
  val ( mod ) : (number, 'n) t -> (number, 'n2) t -> (number, not_null) t
  val ( ~- ) : (([< negatable ] as 's), 'n) t -> ('s, not_null) t
end

(** {1 The sorts, as modules} *)

module Boolean : sig
  type nonrec t = (boolean, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : bool -> t
end

module Null_boolean : sig
  type nonrec t = (boolean, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : bool option -> t
end

module Number : sig
  type nonrec t = (number, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val of_int : int -> t
  val of_int64 : int64 -> t
  val of_float : float -> t
end

module Null_number : sig
  type nonrec t = (number, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val of_int : int option -> t
  val of_int64 : int64 option -> t
  val of_float : float option -> t
end

module Text : sig
  type nonrec t = (text, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : string -> t
end

module Null_text : sig
  type nonrec t = (text, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : string option -> t
end

(** A point in time. A point less a point is a span; a point and a span make a point. *)
module Datetime : sig
  type nonrec t = (datetime, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Timestamp.t -> t

  val add : (datetime, 'n) term -> (timespan, 'n2) term -> t
  (** [point + span]. *)

  val sub : (datetime, 'n) term -> (timespan, 'n2) term -> t
  (** [point - span]. *)

  val diff : (datetime, 'n) term -> (datetime, 'n2) term -> (timespan, not_null) term
  (** [point - point]: the span between them. *)
end

module Null_datetime : sig
  type nonrec t = (datetime, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Timestamp.t option -> t
end

(** A span of time. Spans add up. *)
module Timespan : sig
  type nonrec t = (timespan, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Interval.t -> t
  val add : (timespan, 'n) term -> (timespan, 'n2) term -> t
  val sub : (timespan, 'n) term -> (timespan, 'n2) term -> t
end

module Null_timespan : sig
  type nonrec t = (timespan, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Interval.t option -> t
end

(** A calendar date. *)
module Date : sig
  type nonrec t = (date, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Date.t -> t
end

module Null_date : sig
  type nonrec t = (date, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Value.Date.t option -> t
end

(** A UUID. *)
module Uuid : sig
  type nonrec t = (uuid, not_null) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Uuidm.t -> t
end

module Null_uuid : sig
  type nonrec t = (uuid, nullable) t

  val field : string -> t
  val field_at : Path.t -> t
  val value : Uuidm.t option -> t
end
