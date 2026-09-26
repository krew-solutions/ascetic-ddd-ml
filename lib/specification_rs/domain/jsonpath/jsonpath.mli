(** A specification written as a JSONPath filter with placeholders: parsed once, bound to
    parameters for each use.

    {[
    open Ascetic_specification

    let adults = Jsonpath.Template.parse_exn "$[?@.age >= %d && @.active == true]"

    let user =
      Record.(
        object_ [ ("age", value (Value.Int 30L)); ("active", value (Value.Bool true)) ])

    let () =
      assert (
        Jsonpath.Template.matches adults (Record.to_context user)
          (Jsonpath.Params.positional [ Value.Int 18L ])
        = Ok true)
    ]}

    {2 Grammar}

    {v
    template    = "$" ( filter | path "[*]" filter )
    path        = ( "." name )+
    filter      = "[" "?" or "]"
    or          = and ( "||" and )*
    and         = unary ( "&&" unary )*
    unary       = "!" unary | comparison
    comparison  = operand ( ( "==" | "!=" | "<" | "<=" | ">" | ">=" ) operand )?
    operand     = "(" or ")" | literal | placeholder | query
    query       = ( "@" | "$" ) path ( "[*]" filter )?
    literal     = integer | float | string | "true" | "false" | "null"
    placeholder = "%s" | "%d" | "%f" | "%(" name ")" ( "s" | "d" | "f" )
    name        = ( letter | "_" ) ( letter | digit | "_" )*
    v}

    [$[?p]] is [p] of the candidate, and [@] in it is the candidate. [$.a.items[*][?p]] is
    "some item of [a.items] satisfies [p]", and [@] in [p] is the item; [$] is the
    candidate everywhere. A query with [[*]] inside a filter nests the same. This is the
    language of the Python and Go ports as well - a template of one is a template of the
    others - and it is not RFC 9535's reading of the same text, where a filter selects
    among the children of what precedes it: a specification is a predicate, not a
    selection.

    [@.a == null] is how a template finds a null, and it does: bound, an equality with
    null - spelled out, or a placeholder given a null - is the null test, [IS NULL], and
    [!=] is [IS NOT NULL] ({!Null_test}). In the tree, as in SQL, [a = NULL] would be true
    of nothing. The rule is of the template's constants, not of the candidate's data:
    [@.a == @.b] with both null is null, where RFC 9535 has true.

    Operators, literals, escapes and numbers are RFC 9535's. Either side of a comparison
    may be any operand, and [$] may be used inside a filter.

    What is refused, each because the sources this was ported from read it as something
    other than what it says, until the same was carried back to them: a bracket or
    parenthesis left open or closed twice; a filter on a path without [[*]], whose path
    was dropped and the filter applied to the candidate; a name without [@]; positional
    and named placeholders in one template, which bound the wrong parameters.

    {2 Limits}

    A template's tree has at most 128 levels, and a template nests at most 32 deep -
    groups, [!], the filters of collections: a text that is not trusted must not be a
    stack overflow. A chain of [&&] or [||] nests to the left, so it has at most 128
    operands. A template is at most {!max_length} bytes of UTF-8, looked at before
    anything is read. *)

(** {1 Placeholders} *)

(** How a placeholder names its parameter. *)
module Param_key : sig
  type t =
    | Position of int
        (** By its place among the template's placeholders, from zero: [%d]. *)
    | Name of string  (** By name: [%(age)d]. *)
  [@@deriving show, eq, ord]

  val to_string : t -> string
end

(** What a placeholder's letter asks of its parameter. A null fits any. *)
module Param_kind : sig
  type t =
    | Any  (** [%s]: a value of any kind, as Python's [%s] takes any object. *)
    | Integer  (** [%d]: an integer. *)
    | Number  (** [%f]: a number, integer or float. *)
  [@@deriving show, eq, ord]

  val to_string : t -> string
end

(** A placeholder. *)
module Param : sig
  type t = {
    key : Param_key.t;  (** Which parameter it takes. *)
    kind : Param_kind.t;  (** What it asks of it. *)
  }
  [@@deriving show, eq, ord]
end

(** What stands where a value will: the value, if the template spells it out, or a
    placeholder for it. *)
module Slot : sig
  type t = Literal of Value.t | Param of Param.t [@@deriving show, eq]
end

(** The parameters of one match. *)
module Params : sig
  type t =
    | Positional of Value.t list
        (** For [%s], [%d], [%f], in the order of the placeholders. *)
    | Named of (string * Value.t) list
        (** For [%(name)s] and its kin. A name no placeholder uses is ignored, as Python's
            [%] ignores a key of its mapping. *)
  [@@deriving show, eq]

  val none : t
  (** No parameters: for a template without placeholders. *)

  val positional : Value.t list -> t
  val named : (string * Value.t) list -> t
end

(** {1 Errors} *)

(** A template is not in the grammar. *)
module Syntax_error : sig
  type t = {
    message : string;  (** What was found. *)
    position : int;  (** Where: the index of the character, from zero. *)
    expected : string;  (** What would have been accepted there. *)
    expression : string;  (** The template. *)
  }
  [@@deriving show, eq]

  val to_string : t -> string
  (** The message, where, what was expected, and the template with a caret under the
      place:
      {v
      Unexpected character '#' at position 7 (expected valid token)
        $[?@.a # 1]
               ^
      v} *)
end

(** The parameters do not fit the template's placeholders. The sources leave an unbound
    placeholder in the tree, where it compares unequal to everything; here a template is
    bound wholly or not at all. *)
module Bind_error : sig
  type t =
    | Missing of Param_key.t  (** No parameter for this placeholder. *)
    | Unused of { placeholders : int; parameters : int }
        (** More positional parameters than placeholders. *)
    | Wrong_style  (** Positional parameters for named placeholders, or the reverse. *)
    | Mismatch of { key : Param_key.t; expected : Param_kind.t; found : string }
        (** The parameter is not of the kind the placeholder's letter asks for. *)
  [@@deriving show, eq]

  val to_string : t -> string
end

(** A bound template could not be matched against a candidate. *)
module Match_error : sig
  type t = Bind of Bind_error.t | Eval of Evaluate.error [@@deriving show, eq]

  val to_string : t -> string
end

(** {1 Templates} *)

val max_length : int
(** How long a template may be, in bytes of UTF-8: 262 144. The bounds on a tree's height
    and the parser's nesting bound the shape of a tree and not the size of a text: a
    string literal is as long as it is written, and a text of megabytes was read to its
    end. The length is checked before anything is read. A template of realistic operands
    within the bounds is a few kilobytes; a long value belongs in a parameter. *)

(** A template: read once, bound to parameters as often as needed.

    The sources keep the parsed tree and, for each match, copy it with every placeholder
    marker replaced - a marker being a value of a shape no real value is supposed to have.
    Here a template's tree is over {!Slot.t}s, a bound one over {!Value.t}s, and binding
    is the map from one to the other: that a tree still has placeholders in it is a fact
    of its type. *)
module Template : sig
  type t [@@deriving show, eq]

  val parse : string -> (t, Syntax_error.t) result
  (** Reads [source]. See the module for the grammar. *)

  val parse_exn : string -> t
  (** {!parse}, raising [Invalid_argument] with the error's text: for a template written
      in the code, which is right or is a defect. *)

  val source : t -> string
  (** The text this was read from. *)

  val expr : t -> Slot.t Ast.t
  (** The tree, placeholders in place. *)

  val bind : t -> Params.t -> (Value.t Ast.t, Bind_error.t) result
  (** The specification this is with [params] for its placeholders. *)

  val matches : t -> Value.t Context.t -> Params.t -> (bool, Match_error.t) result
  (** Whether [candidate] satisfies this with [params] for its placeholders: the sources'
      [match]. *)
end
