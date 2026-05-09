(** Unit of Work pattern.

    Abstracts the database transaction boundary at the application layer.
    The application layer operates on [UnitOfWork.t] without knowing
    about database connections or transaction mechanics.

    Infrastructure layer provides a concrete implementation
    (e.g., backed by Caqti transaction). *)

(** Module type for Unit of Work. *)
module type S = sig
  type t
  (** The unit of work handle (abstract transaction context). *)

  val commit : t -> (unit, string) result
  (** Commit all changes made within this unit of work.
      Returns [Error] if the commit fails (e.g., constraint violation). *)

  val rollback : t -> unit
  (** Rollback all changes, discarding uncommitted work. *)
end
