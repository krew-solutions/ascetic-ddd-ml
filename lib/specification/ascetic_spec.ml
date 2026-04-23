(** Ascetic Specification — dual-interpretation specification pattern.

    Parse specifications from strings, evaluate in-memory or compile to SQL. *)

module Ast = Spec_ast
module Eval = Spec_eval
module Sql = Spec_sql
module Parse = Spec_parse
