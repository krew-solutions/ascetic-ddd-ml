(** One text for one value: compact JSON with object keys in order. What the associated
    data of every cipher of this library is made of, so it must never change for data that
    exists.

    The text is the one the reference port writes, byte for byte, so that what one port
    sealed another opens: no whitespace; the fields of an object by the byte order of
    their names; in a string, ["\""] and ["\\"] escaped, the control characters below
    [U+0020] as [\b], [\f], [\n], [\r], [\t] or [\u00xx] in lower-case hex, and every
    other byte as it is. *)

type json =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `String of string
  | `List of json list
  | `Assoc of (string * json) list ]
(** What has a canonical text: a subtype of [Yojson.Safe.t], so a value built for that
    library is accepted as it is. There is no float, by the type: a number that has more
    than one spelling, [1] and [1.0] and [1e0], has no one text, and equality of floats is
    not identity. *)

val to_string : json -> string
(** The canonical text. A field given twice counts once, the last one, as a map would keep
    it. *)
