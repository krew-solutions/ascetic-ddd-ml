(** What a tenant's KEK is made with, by whoever has custody of it (ADR-0024). *)

type t = {
  derived : bool;
      (** Every operation on the key takes a derivation context, and what is wrapped under
          one context does not unwrap under another; the context is a channel's name on
          the bus. Fixed at creation: a key that exists otherwise is a conflict. *)
  auto_rotate_period : float option;
      (** The KMS rotates the key itself, this often, in seconds; an adapter without a
          scheduler refuses it rather than ignore it. *)
}

let default = { derived = false; auto_rotate_period = None }
let equal (a : t) (b : t) = a = b
