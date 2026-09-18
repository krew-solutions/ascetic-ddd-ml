type t = { mutable detach : (unit -> unit) option }

let make detach = { detach = Some detach }

let cancel t =
  match t.detach with
  | None -> ()
  | Some detach ->
      t.detach <- None;
      detach ()
