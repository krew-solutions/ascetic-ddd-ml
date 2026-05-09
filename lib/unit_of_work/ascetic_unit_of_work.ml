(** Unit of Work pattern.

    {!Unit_of_work} declares the abstract module type used by application-layer
    code. {!Caqti_unit_of_work} is a concrete implementation backed by a Caqti
    connection wrapped in a transaction boundary. *)

module Unit_of_work = Unit_of_work
module Caqti_unit_of_work = Caqti_unit_of_work
