(** A name that may be spliced into SQL.

    Table and sequence names go into statements as text, not as parameters: PostgreSQL
    takes no parameter where an identifier goes. So a name must be known safe before it is
    used, and it is known safe by being an {!t}: lower-case letters, digits and
    underscores, not starting with a digit, at most forty characters, room for the
    suffixes the adapters append ([_meta], [_slots], [__waiting_since_idx]) within the
    sixty-three PostgreSQL keeps. Unquoted, so the name is what the catalogue shows;
    unqualified, since the indexes named after a table cannot carry a schema: the
    [search_path] decides. *)

type t

val max_length : int
(** The longest name accepted, forty characters. *)

val of_string : string -> (t, string) result
(** Accepts [[a-z_][a-z0-9_]*] of at most {!max_length} characters; the error says what is
    wrong with the name. *)

val of_string_exn : string -> t
(** {!of_string}, raising [Invalid_argument] on a name refused: for literals. *)

val to_string : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
