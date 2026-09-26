(* A specification could not be compiled. *)
type t =
  | No_current_item  (** A path from the item under test, outside any collection. *)
  | No_table
      (** A member of the candidate inside a collection's predicate, with no schema to say
          what the candidate's row is called there. *)
  | Ambiguous_key of string
      (** A name that fits more than one key: a table with two keys to the row it is named
          from, or a column of two keys. The message names them; the tree names the key it
          means. *)
  | Wrong_key of string
      (** A key named in the tree that does not go where the tree stands: a collection's
          key not referencing the row, an object's key not on it. *)
  | Invalid_identifier of string
      (** A name that is not one: anything but ASCII letters, digits and [_], not starting
          with a digit. *)
  | Nul_in_text  (** A text with a NUL in it: no text PostgreSQL has, so no query. *)
[@@deriving show { with_path = false }, eq]

let to_string = function
  | No_current_item -> "no current item in context"
  | No_table ->
      "a member of the candidate inside a collection's predicate needs the candidate's \
       table: compile with a schema"
  | Ambiguous_key message | Wrong_key message -> message
  | Nul_in_text -> "a text with a NUL (U+0000) in it is no text PostgreSQL has"
  | Invalid_identifier name -> Printf.sprintf "'%s' is not a valid identifier" name
