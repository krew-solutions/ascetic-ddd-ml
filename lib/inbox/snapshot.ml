(** What a statement could see.

    A PostgreSQL snapshot, [pg_current_snapshot()], as the statement that read a row ran
    under it: which transactions it could see. A transaction is visible when its id is
    below [xmin], or below [xmax] and not among those in progress. The observer reports
    the snapshot of every statement that walks the table, so that a recorded run says
    which stored rows each walk could see, whatever order the events were logged in. *)

type t = {
  xmin : int;  (** Every transaction below this id had ended. *)
  xmax : int;  (** No transaction at or above this id had started. *)
  in_progress : int list;  (** Transactions between the two that were still in progress. *)
}

(** Whether the statement could see what transaction [xid] committed. *)
let sees t xid = xid < t.xmin || (xid < t.xmax && not (List.mem xid t.in_progress))

(** [xmin:xmax:xip1,xip2], the text of [pg_snapshot]. *)
let of_string text =
  let malformed = Error (Printf.sprintf "not a snapshot: `%s`" text) in
  let number part = int_of_string_opt part in
  match String.split_on_char ':' text with
  | [ xmin; xmax; in_progress ] -> (
      let in_progress =
        List.filter (fun p -> p <> "") (String.split_on_char ',' in_progress)
      in
      match (number xmin, number xmax, List.map number in_progress) with
      | Some xmin, Some xmax, in_progress when List.for_all Option.is_some in_progress ->
          Ok { xmin; xmax; in_progress = List.filter_map Fun.id in_progress }
      | _ -> malformed)
  | _ -> malformed

let to_string t =
  Printf.sprintf "%d:%d:%s" t.xmin t.xmax
    (String.concat "," (List.map string_of_int t.in_progress))

let equal (a : t) (b : t) = a = b
let pp ppf t = Format.pp_print_string ppf (to_string t)
