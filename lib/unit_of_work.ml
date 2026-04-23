(** Unit of Work pattern. *)

module type S = sig
  type t

  val commit : t -> (unit, string) result
  val rollback : t -> unit
end
