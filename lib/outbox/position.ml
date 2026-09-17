(** Where a consumer group is in one slot: the last transaction it acknowledged, and the
    position within it. *)

type t = {
  transaction_id : int;  (** The last acknowledged transaction; [0] before any. *)
  offset : int;  (** The last acknowledged position within that transaction. *)
}

(** Before anything was acknowledged. *)
let zero = { transaction_id = 0; offset = 0 }

let equal (a : t) (b : t) = a = b

let pp ppf { transaction_id; offset } =
  Format.fprintf ppf "(%d, %d)" transaction_id offset
