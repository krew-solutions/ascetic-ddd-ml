(** Gherkin AST — parsed representation of .feature files. *)

type step_keyword = Given | When | Then | And | But
[@@deriving show, eq]

type step = {
  keyword : step_keyword;
  text : string;
}
[@@deriving show, eq]

type scenario = {
  name : string;
  steps : step list;
}
[@@deriving show, eq]

type feature = {
  name : string;
  description : string option;
  scenarios : scenario list;
}
[@@deriving show, eq]
