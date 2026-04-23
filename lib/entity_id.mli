(** Typed entity ID wrapper functor.

    Prevents mixing up IDs of different entity types at compile time.
    For example, [Member_id.t] and [Message_id.t] are distinct types
    even though both wrap [int64]. *)

(** Input signature: the name of the entity (for display). *)
module type NAME = sig
  val name : string
end

(** Output signature of a typed ID. *)
module type S = sig
  type t [@@deriving show, eq, ord]

  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
end

(** Functor to create a typed entity ID. *)
module Make (N : NAME) : S
