type t = { mutable detach : (unit -> unit) option; quiesce : unit -> unit }

let make ?(quiesce = ignore) detach = { detach = Some detach; quiesce }

let cancel t =
  (match t.detach with
  | None -> ()
  | Some detach ->
      t.detach <- None;
      detach ());
  t.quiesce ()
