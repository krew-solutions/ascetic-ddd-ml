(** [let%specification]: a predicate function and its tree from one source.

    Put on a [let], the extension keeps the function as it is and writes beside it
    [<name>_ast], which returns the same predicate as a tree - to compile to SQL, or to
    evaluate against something that is not the OCaml type. The port of the reference's
    [#[specification]] attribute, itself the port of the Python [lambda_filter], which
    reads a lambda's source at run time, and of the Go [cmd/specgen], which generates a
    file before compilation. See {!Translate} for what the predicate may consist of. *)

module Translate = Translate

let () =
  let open Ppxlib in
  let pattern =
    Ast_pattern.(
      pstr
        (pstr_value nonrecursive (value_binding ~pat:__ ~expr:__ ~constraint_:drop ^:: nil)
        ^:: nil))
  in
  let extension =
    Extension.declare_inline "specification" Extension.Context.structure_item pattern
      (fun ~loc ~path:_ pat expr -> Translate.structure_items ~loc pat expr)
  in
  Driver.register_transformation
    ~rules:[ Context_free.Rule.extension extension ]
    "specification"
