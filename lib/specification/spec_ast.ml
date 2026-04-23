(** Core AST for specification pattern.

    Designed to be interpreted by multiple backends:
    - In-memory evaluator (for domain policy checks)
    - SQL compiler (for persistent queries)

    Supports JSONPath-like navigation with wildcards,
    quantifiers (exists/forall), and parameterized placeholders. *)

(* === Values === *)

type value =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Str of string
  | List of value list
  | Record of (string * value) list
[@@deriving show, eq]

(* === Path navigation === *)

type path_step =
  | Field of string          (* .name *)
  | Wildcard                 (* [*] *)
  | Index of int             (* [0] *)
  | Filter of expr           (* [?(@.active == true)] *)
[@@deriving show, eq]

and path = path_step list
[@@deriving show, eq]

(* === Expressions === *)

and expr =
  | Lit of value
  | Path of path
  | Placeholder of string            (* $param — for parameterized queries *)
  | BinOp of binop * expr * expr
  | UnaryOp of unaryop * expr
  | Call of string * expr list        (* size(), startsWith(), contains() *)
  | Exists of path * string * expr    (* path.exists(alias, predicate) *)
  | ForAll of path * string * expr    (* path.all(alias, predicate) *)
  | AliasRef of string                (* reference to exists/forall bound var *)
[@@deriving show, eq]

and binop =
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or
  | Add | Sub | Mul | Div
  | In
[@@deriving show, eq]

and unaryop =
  | Not
  | Neg
[@@deriving show, eq]

(* === Specification — top-level wrapper === *)

type spec = {
  name : string option;
  expr : expr;
}
[@@deriving show, eq]

(* === Schema metadata for SQL compilation === *)

type column_type = ColInt | ColStr | ColBool | ColFloat | ColTimestamp
[@@deriving show, eq]

type composite_key = {
  columns : (string * column_type) list;
}
[@@deriving show, eq]

type relation =
  | OneToMany of {
      child_table : string;
      fk : (string * string) list;
    }
  | ManyToMany of {
      junction_table : string;
      left_fk : (string * string) list;
      right_fk : (string * string) list;
      target_table : string;
    }
[@@deriving show, eq]

type table_schema = {
  table_name : string;
  primary_key : composite_key;
  columns : (string * column_type) list;
  relations : (string * relation) list;
}
[@@deriving show, eq]

type schema = {
  root_table : string;
  tables : (string * table_schema) list;
}
[@@deriving show, eq]
