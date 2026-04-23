(** Ascetic Gherkin — .feature file parser and runner for OCaml.

    Pure OCaml implementation using ocamllex/menhir.
    No C bindings, no external dependencies beyond Re (regex). *)

module Ast = Gherkin_ast
module Runner = Gherkin_runner
