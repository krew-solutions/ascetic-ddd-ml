(** How the rows of a storage point at one another: the sources' [SchemaRegistry].

    A schema is the foreign keys of a storage, as [\d] shows them, and nothing of any
    aggregate or query: a key is on a table, of columns, and references a table's columns.
    What the compiler calls a row in a query - an alias - is the compiler's own, made as
    it goes. The one thing of the query in a schema is the table the query is of,
    [FROM stores s]: the row the compiler starts from, and what it qualifies that row's
    columns with.

    A tree names a collection by the table its rows are in, [store_items], or, where two
    keys of that table reference the same row, by the key's name; and an object kept in a
    table of its own by the key's column, [owner_id]. A key has a name as it has in
    PostgreSQL: the one it is given, or [<table>_<columns>_fkey]. A Value Object kept in
    the query's row as a column of a composite type is declared as one,
    [composite "stores" "address"], and a path through it from the candidate is a member
    of it, [("s"."address")."city"]: from the candidate an undeclared name is a table's
    alias, ["s"."price"]. *)

(** A foreign key: [table (columns) REFERENCES referenced_table (referenced_columns)]. *)
module Foreign_key : sig
  type t [@@deriving show, eq]

  val make : string -> string -> string -> string -> t
  (** [make table column referenced_table referenced_column]: the key on [table] of
      [column], referencing [referenced_column] of [referenced_table]. A key has at least
      this one column: without any, every row of the table would belong to every row it
      references. [table] is a table; or, for a key on a row of an array in a composite,
      which has no table, the array's column by its table, [stores.items]. *)

  val and_ : string -> string -> t -> t
  (** [and_ column referenced_column key]: one more column of a composite key, and the
      column it references. *)

  val named : string -> t -> t
  (** The key's name, where the storage gives it one: [CONSTRAINT name]. *)

  val name : t -> string
  (** The key's name: the one it was given, or the one PostgreSQL gives a key that was
      not, [<table>_<columns>_fkey]. *)

  val table : t -> string
  val columns : t -> string list
  val referenced_table : t -> string
  val referenced_columns : t -> string list
end

type t [@@deriving show, eq]
(** The foreign keys of a storage, for the queries of one table. *)

val make : string -> t
(** For the queries of [table]: the row the compiler starts from. *)

val alias : string -> t -> t
(** The alias the query gives its table: [s] of [FROM stores s]. *)

val foreign_key : string -> string -> string -> string -> t -> t
(** A key of one column: {!Foreign_key.make}. *)

val key : Foreign_key.t -> t -> t
(** A key as built: composite, or named. *)

val composite : string -> string -> t -> t
(** [composite table column]: the column [column] of [table] is of a composite type: a
    Value Object kept in the row. From the candidate a path through it is a member of the
    composite, [("s"."address")."city"], where a name not declared is a table's alias,
    ["s"."price"]. *)

(** {1 What the compiler asks} *)

val is_composite : t -> string -> string -> bool
(** Whether [column] of [table] is declared a composite. *)

val key_named : t -> string -> Foreign_key.t option
(** The key called [name], if there is one. *)

val keys_referencing : t -> string -> string -> Foreign_key.t list
(** [keys_referencing schema table referenced_table]: the keys on [table] that reference
    [referenced_table]. *)

val keys_on : t -> string -> string -> Foreign_key.t list
(** [keys_on schema table column]: the keys on [table] that [column] is a column of. *)

val table : t -> string
(** The query's table, as given: what its row is to a key. *)

val row : t -> string
(** What the query calls its table's row: the alias, or the table. *)
