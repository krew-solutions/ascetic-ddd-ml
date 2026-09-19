(** What a data-encryption key protects.

    One thing of one tenant, named by its kind and its id. An event store names its
    aggregate's stream this way, a document store its document; the id is JSON, so a
    composite id fits. *)

type id = Canonical.json
(** An id: text, an integer, or an array or object of these. *)

type t

val make : tenant_id:string -> kind:string -> id -> t
(** The resource [id] of [kind], of the tenant. *)

val tenant_id : t -> string
(** The tenant. *)

val kind : t -> string
(** The kind: the aggregate type, the table, the collection. *)

val id : t -> id
(** The id within the kind. *)

val canonical : t -> string
(** The canonical text: the three as a JSON array, the id's object keys in order wherever
    they occur, so that one resource has one text however its id was built. This is the
    associated data of the resource's ciphers, what binds a ciphertext to its resource,
    and so must never change for a resource that has data. *)

val id_text : t -> string
(** The canonical text of the id alone: what a store keeps the id as. *)

val equal : t -> t -> bool
(** Whether two resources are one: whether their canonical texts are. *)

val pp : Format.formatter -> t -> unit
(** The canonical text. *)
