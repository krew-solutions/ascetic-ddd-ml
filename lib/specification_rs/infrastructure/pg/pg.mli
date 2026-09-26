(** The reader that writes a specification as the condition of a PostgreSQL query: the
    sources' [PostgresqlVisitor].

    {[
    open Ascetic_specification

    let specification : Value.t Ast.t =
      Ast.(
        and_ (field "active") (any "items" (gt (item "price") (value (Value.Int 500L)))))

    let () =
      match Pg.compile specification with
      | Ok { sql; params } ->
          assert (
            sql
            = {|"active" AND EXISTS (SELECT 1 FROM unnest("items") AS "item_1" WHERE "item_1"."price" > $1)|});
          assert (params = [ Value.Int 500L ])
      | Error _ -> assert false
    ]}

    Every constant becomes a numbered parameter; the text holds names and operators only.
    A name is written between double quotes, as it is: it is the column's name, to the
    letter, and nothing else PostgreSQL knows by that word. A name that is not of ASCII
    letters, digits and [_] is refused, and a quote inside one would be doubled, so that
    no tree, whatever it was built from, can put SQL of its own into the query.

    A path of more than one name goes through objects. From the candidate an object is a
    qualifier of the name, ["s"."price"]. From the item of a collection it is a Value
    Object, a composite kept in the item's row: [("item_1"."maker")."name"]. Either is
    what it is unless the {!Schema} says the object is kept in a table of its own, as it
    says of a collection: then its member is read through the key, by a subquery in the
    column's place. The sources write dots in every case, which PostgreSQL reads as a
    table and a column; and from an item they drop its alias.

    The sources number parameters and aliases with counters that every visitor shares and
    changes. Here the count so far goes into each step and the count after comes out of
    it, so compiling changes nothing anywhere.

    {2 Where the text differs from the sources'}

    - A constant with nothing but constants beside it has its type said,
      [$1::bigint + $2::bigint]: the server has nothing to find it by, and the sources'
      [$1 + $2] is "operator is not unique: unknown + unknown" to a driver that asks the
      server for the types. {!PARAM_TYPE} is how a value says its kind.
    - Parentheses follow associativity as well as precedence. The sources compare
      precedences alone and write [a - (b - c)] as [a - b - c], and [(a = b) = c] as
      [a = b = c], which PostgreSQL does not parse.
    - [IS] is written [IS NOT DISTINCT FROM]. PostgreSQL's [IS] takes a keyword - [TRUE],
      [NULL] - and not a parameter: the sources' [x IS $1] is a syntax error.
    - The predicate of a relational collection is parenthesised where it must be. The
      sources write [fk AND p OR q], which selects rows of other parents through [q].
    - A collection named from the candidate inside another collection's predicate joins to
      the root row, not to the enclosing item. *)

module Schema = Pg_schema
module Foreign_key = Pg_schema.Foreign_key

(** A specification could not be compiled. *)
type error = Pg_error.t =
  | No_current_item
  | No_table
  | Ambiguous_key of string
  | Wrong_key of string
  | Invalid_identifier of string
  | Nul_in_text
[@@deriving show, eq]

val error_to_string : error -> string

type 'v query = {
  sql : string;  (** The condition: what goes after [WHERE]. *)
  params : 'v list;  (** The parameters, in the order of their numbers. *)
}
[@@deriving show, eq]
(** A condition and its parameters: [$1] is the first of [params], unless the compiler was
    given an [offset]. *)

(** What PostgreSQL calls the type of a value, by its kind. *)
module type PARAM_TYPE = sig
  type t

  val param_type : t -> string option
  (** The name of the type, or none for a value of no kind: the null. *)

  val nul_in_text : t -> bool
  (** Whether the value is a text with a NUL in it: no text PostgreSQL has, [text] holds
      none. The compiler refuses such a value where it meets the server, rather than let
      the driver or the server fail the query. *)
end

(** The compiler over values of one type. *)
module Make (P : PARAM_TYPE) : sig
  val compile : ?schema:Schema.t -> ?offset:int -> P.t Ast.t -> (P.t query, error) result
  (** [specification] as a condition: the sources' [compile_to_sql]. Its collections are
      embedded unless [schema] says how they are stored; its parameters are numbered from
      [$1], or from [$offset+1] for a condition that is a part of a larger query whose
      first [offset] parameters are taken. *)
end

val compile :
  ?schema:Schema.t -> ?offset:int -> Value.t Ast.t -> (Value.t query, error) result
(** {!Make} over {!Value}. *)
