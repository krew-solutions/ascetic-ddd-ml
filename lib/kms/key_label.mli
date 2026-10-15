(** What a MAC key is for, as part of the key's name.

    A tenant's keys of every kind sit on one mount, told apart by name:
    [{tenant}.sign.{algorithm}], [{tenant}.mac.{purpose}]. The dot is the separator, so
    neither part may contain one; a label is letters, digits, [_] and [-], one to
    sixty-four of them, parsed once. *)

type t

val max_length : int
(** Sixty-four characters. *)

val is_part : string -> bool
(** Whether the text can be one part of a dotted key name: non-empty, of letters, digits,
    [_] and [-] only, so the parts stay apart. What a tenant's id must be for the dotted
    names too. *)

val make : string -> (t, Kms_error.t) result
(** The label, checked: [Malformed] when the text is empty, too long, or carries a
    character a key name cannot. *)

val to_string : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
