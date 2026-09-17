(** What one {!Pg_inbox.dispatch} call did. *)

type t =
  | Nothing
      (** Nothing was done: no slot had a due head; or the slot taken had none once its
          head was read after the lock; or the head was about to wait for a dependency
          that was marked meanwhile, and the next call takes it again. *)
  | Set_aside
      (** The head of the slot taken was set aside to wait for a dependency (ADR-0005),
          and that wait is committed (ADR-0008); the next call takes the slot's next head.
      *)
  | Processed  (** A message was processed and marked. *)
  | Failed of { attempts : int; parked : bool }
      (** The subscriber failed; its writes were rolled back and the attempt recorded.
          [attempts] counts this one; [parked] says whether this attempt was the last: its
          attempts ran out, or the subscriber's verdict was permanent. *)

let equal (a : t) (b : t) = a = b

let pp ppf = function
  | Nothing -> Format.pp_print_string ppf "Nothing"
  | Set_aside -> Format.pp_print_string ppf "Set_aside"
  | Processed -> Format.pp_print_string ppf "Processed"
  | Failed { attempts; parked } ->
      Format.fprintf ppf "Failed { attempts = %d; parked = %b }" attempts parked
