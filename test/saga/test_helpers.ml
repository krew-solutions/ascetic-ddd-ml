(** Shared helpers for the saga test suite.

    Built as a normal module (not an executable) — alcotest stanzas pick
    up sibling .ml files automatically. The helpers mirror the small
    boilerplate that Python tests get for free via class-level mutable
    counters and Activity ABC. *)

module S = Ascetic_saga
module RS = S.Routing_slip
module WI = S.Work_item
module WL = S.Work_log
module Args = S.Work_item_arguments
module Res = S.Work_result
module Resolver = S.Activity_resolver
module Ser = S.Serializable

(** Mutable counters scoped to a single test through [setup]. *)
type counters = {
  mutable call_count : int;
  mutable compensate_count : int;
}

let make_counters () : counters = { call_count = 0; compensate_count = 0 }

(** Build a stub activity that:
    - returns a [Work_log] with [{"ok": true}] when [should_succeed = true],
      otherwise returns [None];
    - increments [counters.call_count] on every [do_work];
    - increments [counters.compensate_count] on every [compensate];
    - exposes [sb://./<name>] / [sb://./<name>Compensation] queue addresses.

    The activity reuses itself as the factory target so that
    [Work_log.factory] round-trips correctly through the resolver. *)
let make_stub_activity
      ?(should_succeed = true)
      ?(extra_result : (string * Yojson.Safe.t) list = [])
      ~(counters : counters)
      ~(name : string)
      ()
  : S.Activity.t =
  let activity_ref : S.Activity.t option ref = ref None in
  let factory : S.Saga_types.factory =
    fun () ->
      match !activity_ref with
      | Some a -> a
      | None -> assert false
  in
  let do_work _wi =
    counters.call_count <- counters.call_count + 1;
    if should_succeed then
      let base = [ "ok", `Bool true ] in
      let result = Res.of_list (base @ extra_result) in
      Some (WL.create_with_factory ~factory ~result)
    else None
  in
  let compensate _wl _rs =
    counters.compensate_count <- counters.compensate_count + 1;
    true
  in
  let activity =
    S.Activity.create
      ~name
      ~do_work
      ~compensate
      ~work_item_queue_address:("sb://./" ^ name)
      ~compensation_queue_address:("sb://./" ^ name ^ "Compensation")
  in
  activity_ref := Some activity;
  activity

(** Factory that produces a fresh stub activity sharing the same counters
    instance every time it is invoked. Used wherever a [Saga_types.factory]
    is required (in [Work_item], in [Resolver.Map_based.register], etc.). *)
let stub_factory
      ?should_succeed
      ?extra_result
      ~(counters : counters)
      ~(name : string)
      ()
  : S.Saga_types.factory =
  fun () ->
    make_stub_activity ?should_succeed ?extra_result ~counters ~name ()
