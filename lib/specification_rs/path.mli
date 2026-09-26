(** A member reached from a root through nested objects: [user.profile.age]. Never empty:
    it names at least the member.

    The sources describe such a path as a linked chain of [Object] nodes ending in a
    [Field]; here it is a root and names. *)

(** Where a path starts. *)
type root =
  | Global  (** The candidate itself: JSONPath's [$]. *)
  | Item of int
      (** The item under test in an enclosing [Any], by how far out that one is: [Item 0]
          is the item of the nearest, JSONPath's [@]; [Item 1] the item of the collection
          enclosing that one, which the text of JSONPath cannot name and a predicate
          written in the host language can: the category, from the predicate of its
          products. *)
[@@deriving show, eq, ord]

type t [@@deriving show, eq, ord]

val make : root -> string -> t
(** The member [name] of [root]. *)

val global : string -> t
(** The member [name] of the candidate. *)

val item : string -> t
(** The member [name] of the item under test. *)

val outer : int -> string -> t
(** The member [name] of the item [up] collections out: [outer 1 name] is the item of the
    collection enclosing the nearest, [outer 0 name] is {!item}. *)

val child : t -> string -> t
(** One step down: what this path named is an object, and the path now names its member
    [name]. *)

val sibling : t -> string -> t
(** The member [name] of the same object: [user.profile.age] to [user.profile.name]. What
    a mapping needs when one member of the domain is several columns of one table. *)

val dotted : root -> string -> t
(** The dotted [names] from [root]: ["user.profile.age"]. The text is split at the dots
    and nothing else is done to it: whether a name is one the storage accepts is the
    storage's to say. *)

val of_string : string -> t
(** A dotted path from the candidate: [dotted Global]. *)

val root : t -> root

val objects : t -> string list
(** The objects walked through, outermost first. *)

val name : t -> string
(** The member named. *)

val names : t -> string list
(** Every name of the path in order: the objects, then the member. *)
