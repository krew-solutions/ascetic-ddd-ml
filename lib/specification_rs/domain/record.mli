(** A candidate made of plain data: what the sources' [DictContext], [NestedDictContext]
    and [CollectionContext] are between them. For tests, for documents, and for a
    candidate that arrives as data rather than as a domain object; a domain object is a
    {!Context.t} of its own.

    An object that may be absent, an option of a Value Object, is a null value where it is
    absent, [value Value.Null]: null to a null test, and no object to go into. A member
    left out is missing, which is another thing. *)

(** A value, an object of named members, or a collection of items. *)
type 'v t =
  | Value of 'v  (** A value. *)
  | Object of (string * 'v t) list  (** Named members. *)
  | Collection of 'v t list  (** Items, in order. *)
[@@deriving show, eq]

val value : 'v -> 'v t
val object_ : (string * 'v t) list -> 'v t
val collection : 'v t list -> 'v t

val to_context : 'v t -> 'v Context.t
(** The record as a candidate: a member that is not there is {!Context.Missing}, one that
    is there as something else is not a value, not an object, not a collection. *)
