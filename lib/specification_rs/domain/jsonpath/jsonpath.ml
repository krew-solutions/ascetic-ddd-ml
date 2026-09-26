module Param_key = Jsonpath_slot.Param_key
module Param_kind = Jsonpath_slot.Param_kind
module Param = Jsonpath_slot.Param

module Slot = struct
  type t = Jsonpath_slot.t = Literal of Value.t | Param of Param.t
  [@@deriving show { with_path = false }, eq]
end

module Params = struct
  type t = Positional of Value.t list | Named of (string * Value.t) list
  [@@deriving show { with_path = false }, eq]

  let none = Positional []
  let positional values = Positional values
  let named values = Named values
end

module Syntax_error = struct
  type t = Jsonpath_error.t = {
    message : string;
    position : int;
    expected : string;
    expression : string;
  }
  [@@deriving show { with_path = false }, eq]

  let to_string = Jsonpath_error.to_string
end

module Bind_error = struct
  type t =
    | Missing of Param_key.t
    | Unused of { placeholders : int; parameters : int }
    | Wrong_style
    | Mismatch of { key : Param_key.t; expected : Param_kind.t; found : string }
  [@@deriving show { with_path = false }, eq]

  let to_string = function
    | Missing key ->
        Printf.sprintf "no parameter for placeholder %s" (Param_key.to_string key)
    | Unused { placeholders; parameters } ->
        Printf.sprintf "%d parameters for %d placeholders" parameters placeholders
    | Wrong_style ->
        "positional parameters for named placeholders, or named for positional"
    | Mismatch { key; expected; found } ->
        Printf.sprintf "placeholder %s expects %s, got %s" (Param_key.to_string key)
          (Param_kind.to_string expected)
          found
end

module Match_error = struct
  type t = Bind of Bind_error.t | Eval of Evaluate.error
  [@@deriving show { with_path = false }, eq]

  let to_string = function
    | Bind error -> Bind_error.to_string error
    | Eval error -> Evaluate.error_to_string error
end

let max_length = 262_144
let ( let* ) = Result.bind

module Template = struct
  type t = { source : string; expr : Slot.t Ast.t; positional : int }
  [@@deriving show { with_path = false }, eq]

  module Evaluate = Evaluate.Make (Value)
  module Null_test = Null_test.Make (Value)

  let parse source =
    if String.length source > max_length then
      Error
        (Jsonpath_error.make "Template too long" max_length
           (Printf.sprintf "at most %d bytes of UTF-8" max_length))
    else
      let chars = Jsonpath_error.code_points source in
      let within error = Jsonpath_error.within error source in
      let* tokens, positional = Result.map_error within (Jsonpath_lexer.tokenize chars) in
      let* expr =
        Result.map_error within (Jsonpath_parser.template tokens (Array.length chars))
      in
      Ok { source; expr; positional }

  let parse_exn source =
    match parse source with
    | Ok template -> template
    | Error error -> invalid_arg (Syntax_error.to_string error)

  let source template = template.source
  let expr template = template.expr

  let get params key =
    let found =
      match (params, key) with
      | Params.Positional values, Param_key.Position position ->
          Ok (List.nth_opt values position)
      | Params.Named values, Param_key.Name name -> Ok (List.assoc_opt name values)
      | _ -> Error Bind_error.Wrong_style
    in
    let* found = found in
    Option.to_result ~none:(Bind_error.Missing key) found

  let bind template params =
    let slot = function
      | Slot.Literal value -> Ok value
      | Slot.Param { key; kind } ->
          let* value = get params key in
          if Param_kind.admits kind value then Ok value
          else
            Error (Bind_error.Mismatch { key; expected = kind; found = Value.kind value })
    in
    let* bound = Ast.try_map_values slot template.expr in
    (* Every placeholder has its parameter; is there a parameter without a placeholder?
       Python's [%] refuses a tuple too long as well. *)
    match params with
    | Params.Positional values when List.length values > template.positional ->
        Error
          (Bind_error.Unused
             { placeholders = template.positional; parameters = List.length values })
    (* [@.a == null] is how a template finds a null, spelled out or bound: now that the
       values are known, it is the null test. *)
    | Params.Positional _ | Params.Named _ -> Ok (Null_test.throughout bound)

  let matches template candidate params =
    let* bound =
      Result.map_error (fun error -> Match_error.Bind error) (bind template params)
    in
    Result.map_error
      (fun error -> Match_error.Eval error)
      (Evaluate.is_satisfied_by bound candidate)
end
