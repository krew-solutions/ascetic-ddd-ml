(** Domain event envelope. *)

type 'a t = {
  event_id : Uuidm.t;
  occurred_at : Ptime.t;
  aggregate_id : string;
  aggregate_version : int;
  payload : 'a;
}

let create ~aggregate_id ~aggregate_version ~occurred_at payload =
  let event_id = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  { event_id; occurred_at; aggregate_id; aggregate_version; payload }

let map_payload f e = { e with payload = f e.payload }
