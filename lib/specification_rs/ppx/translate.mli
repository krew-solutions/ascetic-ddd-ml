(** From the expression a predicate returns to the code that builds its tree.

    The first parameter of the function is the candidate; the others are the constants of
    the specification, each annotated with its type - [int], [int64], [float], [string],
    [bool], [Value.t], or an [option] of one - and [<name>_ast] takes the same, less the
    candidate. The body is one expression of: members of the candidate and of the item,
    [s.owner.age]; literals and the parameters, which become values; [= <> < <= > >=],
    [&& || not], [+ - * / mod lsl asr] and their [Int64], [Int], [Float] spellings,
    [Int64.add s.a 1L]; [String.equal], [Int64.equal] and their kin, which is how a Value
    Object compares; [Option.is_none], [Option.is_some], and [= None], which is the same;
    [Some x], which is [x]; [Option.fold ~none:false ~some:(fun held -> ...) x] and
    [match x with None -> false | Some held -> ...] of a member or of a parameter that is
    an option, where the name stands for it, and [~none:true], [None -> true] the same
    with [||]: this is how what an option holds is ordered - [<] with [Some x], [None] or
    an option parameter is refused; [List.exists (fun item -> ...)] and [List.for_all],
    nested as deep as the collections are. Anything else is a compile error at the place
    it stands, and the function stays as it was. *)

open Ppxlib

val structure_items : loc:Location.t -> pattern -> expression -> structure_item list
(** The [let] as it was written, and beside it [<name>_ast] - or, where the body cannot be
    a tree, an error at the place that cannot. *)
