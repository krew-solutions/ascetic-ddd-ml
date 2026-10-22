(** From a specification in the domain's terms to the same in the storage's: the sources'
    [TransformVisitor] and [CompositeExpression].

    A member of the domain model need not be a column, nor a domain value a column's
    value. The case the sources are built around is a composite identity:
    [something.id = MemberSomethingId(...)] in the domain is three columns compared with
    three scalars in the table. A mapping says what each member and each value becomes -
    one expression, or a composite of them - and {!transform} rewrites the tree, turning
    an equality of two composites into the conjunction of the equalities of their parts.

    The sources make the composite a node of the tree, of a class the visitor does not
    know: its [accept] raises, and the transformer must catch every composite before
    anything else visits it. Here a composite is not a tree. It is the other case of what
    a mapping returns, {!mapped}, and the result of {!transform} is an {!Ast.t}, which has
    no such case: a composite left over - under [<], under [+], as the whole specification
    \- is an error of [transform], and nothing downstream can meet one.

    The value type changes on the way, ['d Ast.t] to ['s Ast.t], so a tree that was not
    transformed does not pass for one that was. *)

(** What a member or a value of the domain is in the storage. *)
type 's mapped =
  | Scalar of 's Ast.t  (** One expression: a column, a scalar. *)
  | Composite of 's mapped list
      (** Several, compared part by part; a part may be composite itself. *)
  | Null of 's
      (** The storage's null, for a value of the domain that is one there: a special case
          that answers for itself in the domain - "no discount", equal to itself - and is
          a column's null in the storage. Compared for equality it is tested for,
          [IS NULL]: [= $1] with a null is true of nothing. Anywhere else it is the null
          it carries.

          A null that was one in the domain already is a [Scalar] like any constant, and
          stays compared: it is the mapping that knows which of the two a null is, so it
          is the mapping that says. *)
[@@deriving show, eq]

type ('d, 's, 'e) t = {
  field : Path.t -> ('s mapped, 'e) result;
      (** What the member at [path] is: a path from the candidate. *)
  value : 'd -> ('s mapped, 'e) result;  (** What [value] is. *)
  element : Path.t -> ('s mapped, 'e) result;
      (** What an element of the collection at [collection] - a path from the candidate,
          as [field] is asked - is in the storage, said from where the collection's
          predicate stands, as the tree says it there: {!Ast.itself} is the element,
          [Ast.outer_itself n] and a path from it the item [n] collections further out, a
          path from the candidate is the candidate's. {!transform} puts the answer where
          the element was.

          The element itself, unless the storage keeps a part of it elsewhere. A
          tenant-scoped identity in the domain is often its local key in the storage's
          array and its tenant once, in the candidate's row: the element is then
          [Composite [ Scalar (field "tenant_id"); Scalar itself ]], and its equality with
          an identity, mapped to the same two parts, compares the tenant with the row's and
          the key with the element - where an identity mapped to its key alone would find
          the groups of another tenant that have the same key. *)
}
(** What the storage has for the domain's members and values: the sources'
    [ITransformContext].

    A mapping is of the aggregate's members, and knows nothing of any query: it is asked
    about a member by its whole path from the candidate, [categories.products.price] for
    the price of a product of a category, and answers what that is in the storage,
    [categories.products.price_cents], as a path from the candidate's row. A collection is
    a member like any other, [categories.products]. Where a query stands when it asks -
    inside which collection's predicate, how far out an item is - is the tree's, and
    {!transform} puts the answer there: the member of an item is the answer less the
    collection's, from the item.

    A mapping is a guard as well. A specification may arrive as data - a template bound
    from a request, a tree from another service - and the mapping names every member such
    a specification may filter by, and refuses any other: a member the mapping does not
    know is its error, not a column that happens to exist. So what reaches the database is
    a query over the columns the repository chose to expose, through the relations it
    declared in the {!Pg.Schema}, and not whatever a caller composed to read another table
    or to scan a column without an index. {!transform} is the one place for that: the
    compiler writes the names it is given.

    An element of a collection of values, {!Ast.itself}, is what the collection holds,
    where the mapping put the collection - unless the mapping says otherwise, by [element],
    of an element the storage keeps in part elsewhere. A constant compared with it is the
    mapping's [value]. *)

val make :
  ?element:(Path.t -> ('s mapped, 'e) result) ->
  field:(Path.t -> ('s mapped, 'e) result) ->
  value:('d -> ('s mapped, 'e) result) ->
  unit ->
  ('d, 's, 'e) t
(** A mapping of [field] and [value]; [element] answers the element itself,
    [Scalar Ast.itself], unless given: a mapping that says nothing of elements is what it
    was before there were any. *)

(** A specification could not be transformed. *)
type 'e error =
  | Mapping of 'e  (** The mapping failed. *)
  | Shape_mismatch
      (** Two composites compared are not of one shape: the sources'
          [CompositeExpressionsDifferentLengthError]. *)
  | No_current_item
      (** A path from an item outside any collection's predicate, or from an item further
          out than there are collections. *)
  | Outside_its_collection
      (** The mapping put a member of an item, or a collection of one, somewhere else than
          in its collection: [items.price] answered with a path that does not start with
          what [items] was answered with. *)
  | Collection_not_a_place
      (** The mapping answered for a collection with something that is not a place: a
          value, a composite, a null. *)
  | Not_composite  (** A composite compared with one expression. *)
  | Empty_composite  (** A composite without parts. *)
  | Unsupported_operator of Operator.infix
      (** A composite under this operator. Only [=] and [!=] take composites. *)
  | Unexpected_composite
      (** A composite where one expression is needed: under a prefix or postfix operator,
          as a predicate, as the whole specification. *)
[@@deriving show, eq]

val error_to_string : ('e -> string) -> 'e error -> string

val transform : ('d, 's, 'e) t -> 'd Ast.t -> ('s Ast.t, 'e error) result
(** [expr] in the terms of [mapping]. *)
