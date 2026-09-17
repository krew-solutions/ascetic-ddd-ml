(** What a dispatcher takes: a consumer group, and optionally one URI with everything
    under it ([kafka://orders] covers [kafka://orders/order-1]). *)

type t = {
  consumer_group : string;  (** The consumer group; each keeps its own positions. *)
  uri : string;  (** The URI prefix; empty means every URI. *)
}

(** Everything, for [consumer_group]. *)
let group consumer_group = { consumer_group; uri = "" }

(** The same group, narrowed to one URI and everything under it. *)
let uri t uri = { t with uri }
