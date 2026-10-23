(** What a tenant's signing or MAC key is made with, by whoever has custody of it
    (ADR-0036). *)

type t = {
  auto_rotate_period : float option;
      (** The KMS rotates the key itself, this often, in seconds; an adapter without a
          scheduler refuses it rather than ignore it. [None] asks nothing of a key that
          exists: it keeps the period it has. *)
}

let default = { auto_rotate_period = None }
let equal (a : t) (b : t) = a = b
