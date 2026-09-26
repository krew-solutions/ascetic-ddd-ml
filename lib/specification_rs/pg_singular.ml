(* The singular of a collection's name, for the alias of its items: [items] are each an
   [item_1].

   The sources take it from an inflection library. An alias has to be distinct, which its
   number sees to, and readable, which a few rules of English do; nothing depends on the
   singular being right. *)

let unchanged =
  [ "series"; "species"; "news"; "data"; "equipment"; "information"; "money"; "sheep" ]

let irregular =
  [
    ("people", "person");
    ("children", "child");
    ("men", "man");
    ("women", "woman");
    ("mice", "mouse");
    ("feet", "foot");
  ]

let strip_suffix word suffix =
  let length = String.length word and suffix_length = String.length suffix in
  if length > suffix_length && String.ends_with ~suffix word then
    Some (String.sub word 0 (length - suffix_length))
  else None

(* [word] is expected in lower case. *)
let singular word =
  if List.mem word unchanged then word
  else
    match List.assoc_opt word irregular with
    | Some one -> one
    | None -> (
        match strip_suffix word "ies" with
        | Some stem -> stem ^ "y"
        | None -> (
            if
              List.exists
                (fun suffix -> Option.is_some (strip_suffix word suffix))
                [ "sses"; "shes"; "ches"; "xes"; "zes"; "uses" ]
            then Option.value (strip_suffix word "es") ~default:word
            else
              match strip_suffix word "s" with
              | Some stem
                when not
                       (List.exists
                          (fun e -> String.ends_with ~suffix:e stem)
                          [ "s"; "u"; "i" ]) ->
                  stem
              | _ -> word))
