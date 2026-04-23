(** Clock abstraction. *)

module type S = sig
  val now : unit -> Ptime.t
end

module SystemClock : S = struct
  let now () =
    match Ptime.of_float_s (Unix.gettimeofday ()) with
    | Some t -> t
    | None -> Ptime.epoch
end

module FixedClock = struct
  let current = ref Ptime.epoch

  let now () = !current

  let set t = current := t

  let advance span =
    match Ptime.add_span !current span with
    | Some t -> current := t
    | None -> ()

  let create (t : Ptime.t) : (module S) =
    let r = ref t in
    (module struct
      let now () = !r
    end)
end
