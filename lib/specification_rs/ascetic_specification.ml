(** The Specification pattern: a predicate on a domain object, kept as a tree, so that one
    statement of a business rule answers two questions - does this object in memory
    satisfy it, and which rows of the table do.

    The tree and its readers are in the modules below; see the README for the four ways to
    write a specification and the two ways to read one. *)

module Operator = Operator
module Path = Path
module Ast = Ast
module Operand = Operand
module Value = Value
module Context = Context
module Record = Record
module Evaluate = Evaluate
module Null_test = Null_test
module Dsl = Dsl
module Jsonpath = Jsonpath
module Mapping = Mapping
module Pg = Pg
