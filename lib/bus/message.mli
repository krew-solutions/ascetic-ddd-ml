(** What crosses the wire: an opaque payload, optionally a key, and headers.

    The bus never looks inside the payload. Producers encode their values into it and
    consumers decode it back, each with its own function, so two consumers of one topic
    may read the same bytes as different types: the wire format is the contract, the types
    are local to each side.

    The key is for transports that partition: messages with equal keys stay in order
    relative to each other. An integration event is keyed by the id of the aggregate it is
    about. The in-memory transport keeps every message in order and ignores the key.

    Headers are flat, name to bytes, as a broker's are; anything nested travels as a JSON
    string. *)

type t

val make : string -> t
(** A message carrying the payload, no key and no headers. *)

val with_key : t -> string -> t
(** The same message, keyed. *)

val with_header : t -> string -> string -> t
(** The same message with one more header, after those it has. *)

val with_payload : t -> string -> t
(** The same message with another payload: what a stage makes. *)

val without_header : t -> string -> t
(** The same message without the headers of that name. *)

val header : t -> string -> string option
(** The first header of that name. *)

val headers : t -> (string * string) list
(** Every header, in order. *)

val key : t -> string option
val payload : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
