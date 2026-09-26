(** What a specification is evaluated against: the candidate, an object nested in it, an
    item of one of its collections.

    The sources have one method, [get(key)], and look at what came back: a value, a
    context, a list of contexts. The tree already knows which of the three it is after, so
    here it asks for that, and the answer is typed. A domain object is made a context by a
    function that reads its members; plain data is one through {!Record.to_context}. *)

(** A context has no such member, or has it as something else. *)
type error =
  | Missing of string  (** No member of this name. *)
  | Not_a_value of string  (** The member is not a value. *)
  | Not_an_object of string  (** The member is not an object. *)
  | Not_a_collection of string  (** The member is not a collection. *)
[@@deriving show, eq]

val error_to_string : error -> string

type 'v t = {
  field : string -> ('v, error) result;  (** The value of the member [name]. *)
  object_ : string -> ('v t, error) result;  (** The object that is the member [name]. *)
  collection : string -> ('v t list, error) result;
      (** The items of the collection that is the member [name]. *)
}
