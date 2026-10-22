(** What a [jsonb] document of the storage holds: the members of each object in it, and
    the kind of each value.

    A column says its type to the server, and the compiler leaves the rest to it: a
    parameter beside a column takes the column's type. A member of a [jsonb] document says
    nothing of the kind. JSON has numbers, strings and booleans, so [3.0] equals [3]
    there, and a point in time, a date and a UUID are strings. The server reads such a
    member as text, [->>], and compares it as text. A string ordered as text is not
    ordered as the point in time it spells: ["2026-01-10T00:00:00.000000Z"] is greater
    than ["2026-01-10"], which means the same instant. So the compiler is told the kind,
    and says it in the query: [("s"."document" ->> 'priority')::bigint], which is a value
    of that type as a column is, and the rest is the server's as it is for a column.

    A layout is of the storage, as a {!Pg_schema.t} is: it says what the encoding of a
    document writes, and nothing of any aggregate. The {!Mapping.t} says which members of
    the aggregate are read from the document and which from the columns beside it. A
    member the layout does not declare is refused, as a member the mapping does not know
    is.

    {[
    let policy =
      Pg.Layout.(
        empty
        |> value "status" Kind.Text
        |> value "priority" Kind.Int
        |> object_ "targeting" (empty |> value "mode" Kind.Text)
        |> collection "versions"
             (empty |> value "number" Kind.Int |> value "published_at" Kind.Timestamp)
        |> values "device_groups" Kind.Text)
    ]} *)

(** The kind of a value in a document, and the type it is read as. *)
module Kind : sig
  type t =
    | Bool  (** [true] or [false]: [boolean]. *)
    | Int
        (** An integer: [bigint]. A number with a fraction there is an error of the
            server. *)
    | Float  (** A number: [double precision]. *)
    | Text
        (** A string, ordered by code point as the evaluator orders it: [COLLATE "C"]. The
            database's collation orders ['B'] after ['a']. *)
    | Timestamp
        (** A point in time, written in ISO 8601 with its offset: [timestamptz]. Written
            at microseconds: the server rounds what is finer, and the evaluator's
            {!Value.Timestamp} has none of it. *)
    | Date  (** A date, [YYYY-MM-DD]: [date]. *)
    | Uuid  (** A UUID, in either case: [uuid]. *)
  [@@deriving show, eq]

  val read : t -> string -> string
  (** [read kind text]: [text], a member read as text, read as this kind: cast, or
      ordered by code point. A cast binds tighter than any operator, so the cast text is
      an operand as it stands; [COLLATE] binds looser than unary minus, so the collated
      text is parenthesised. *)
end

type t [@@deriving show, eq]
(** The members of an object in a document: the document itself, an object in it, an
    item of an array in it. *)

val empty : t
(** An object of no members yet. *)

val value : string -> Kind.t -> t -> t
(** [value name kind layout]: the member [name] is a value of the kind [kind], or null. *)

val object_ : string -> t -> t -> t
(** [object_ name inner layout]: the member [name] is an object of the members [inner],
    or null. *)

val collection : string -> t -> t -> t
(** [collection name inner layout]: the member [name] is an array of objects of the
    members [inner]. *)

val values : string -> Kind.t -> t -> t
(** [values name kind layout]: the member [name] is an array of values of the kind
    [kind], each named in a specification as the item itself, {!Ast.itself}. *)

(** {1 What the compiler asks} *)

(** A member of an object in a document. *)
type member =
  | Value of Kind.t  (** A value of this kind, or null. *)
  | Object of t  (** An object of these members, or null. *)
  | Collection of t  (** An array of objects of these members. *)
  | Values of Kind.t  (** An array of values of this kind: the groups a policy targets. *)

(** The items of an array in a document: objects of these members, or values of this
    kind. *)
module Items : sig
  type layout = t
  type t = Objects of layout | Values of Kind.t
end

val member : t -> string -> member option
(** What the member [name] is: the last declaration of that name. *)

val items : member -> Items.t option
(** What the items are, if the member is an array. *)
