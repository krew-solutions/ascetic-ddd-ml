open Ppxlib
module B = Ast_builder.Default

exception Refused of Location.t * string

let refuse loc fmt = Printf.ksprintf (fun message -> raise (Refused (loc, message))) fmt

let inexpressible (expr : expression) =
  refuse expr.pexp_loc "not expressible in a specification"

(* ------------------------------------------------------------------------ *)
(* The code the tree is built of                                             *)

let lid ~loc names = { txt = Longident.parse (String.concat "." names); loc }
let library = "Ascetic_specification"
let ast ~loc name = B.pexp_ident ~loc (lid ~loc [ library; "Ast"; name ])
let path ~loc name = B.pexp_ident ~loc (lid ~loc [ library; "Path"; name ])
let value_of ~loc name = B.pexp_ident ~loc (lid ~loc [ library; "Value"; name ])

let construct ~loc name arg =
  B.pexp_construct ~loc (lid ~loc [ library; "Value"; name ]) arg

let call ~loc f args = B.pexp_apply ~loc f (List.map (fun arg -> (Nolabel, arg)) args)
let constant ~loc value = call ~loc (ast ~loc "value") [ value ]
let null ~loc = constant ~loc (construct ~loc "Null" None)

(* Equality is built by [Null_test]: [x = None] is what OCaml writes for "x is none", and
   a parameter of an option type may be none when the tree is asked for. In the tree a
   comparison with null is true of nothing. The module is bound at the top of the tree
   function. *)
let null_test ~loc name = B.pexp_ident ~loc (lid ~loc [ "Null_test"; name ])

(* ------------------------------------------------------------------------ *)
(* What the names in the expression stand for                                *)

type root = Candidate | Item of int

(* A member of the candidate or of the item: where its path starts, and the names along
   it. *)
type place = { root : root; names : string list }

(* What stands behind the name given to what an option holds: the option, which is its
   value or the null. *)
type held =
  | Member of place  (** A member of the candidate or of the item. *)
  | Outside of expression
      (** A value from outside - a parameter - as the tree function writes it. *)

(* How the tree function turns a parameter into a value: the conversion its declared type
   asks for, and whether that type is an option. *)
type param = { convert : expression -> expression; optional : bool }

type scope = {
  candidate : string;
  items : string list;  (** The items of the enclosing quantifiers, the nearest first. *)
  held : (string * held) list;
      (** What an option holds, under its name; the nearest first. *)
  params : (string * param) list;
}

type base =
  | Base_candidate
  | Base_item of int
  | Base_held of held
  | Base_param of param
  | Base_other

(* The nearest of that name, as OCaml reads it: what is held is named inside everything
   else, the items inside the candidate, the nearest item inside the ones further out. *)
let base scope name =
  match List.assoc_opt name scope.held with
  | Some held -> Base_held held
  | None -> (
      let rec index at = function
        | [] -> None
        | item :: items ->
            if String.equal item name then Some at else index (at + 1) items
      in
      match index 0 scope.items with
      | Some up -> Base_item up
      | None -> (
          if String.equal name scope.candidate then Base_candidate
          else
            match List.assoc_opt name scope.params with
            | Some param -> Base_param param
            | None -> Base_other))

(* The same place, seen from one collection further in. *)
let further_in place =
  {
    place with
    root = (match place.root with Candidate -> Candidate | Item up -> Item (up + 1));
  }

(* The scope of the predicate of a collection whose items are [item]. What is held so far
   is one collection further out from here. *)
let inside scope item =
  {
    scope with
    items = item :: scope.items;
    held =
      List.filter_map
        (fun (name, held) ->
          if String.equal name item then None
          else
            Some
              ( name,
                match held with
                | Member place -> Member (further_in place)
                | outside -> outside ))
        scope.held;
  }

(* The scope of a predicate of what an option holds, which it calls [name]. *)
let holding scope name held = { scope with held = (name, held) :: scope.held }

(* The code of the [Path.t] of a place; none of the candidate or the item itself. *)
let path_of ~loc place =
  match place.names with
  | [] -> None
  | first :: rest ->
      let root =
        match place.root with
        | Candidate -> call ~loc (path ~loc "global") [ B.estring ~loc first ]
        | Item 0 -> call ~loc (path ~loc "item") [ B.estring ~loc first ]
        | Item up ->
            call ~loc (path ~loc "outer") [ B.eint ~loc up; B.estring ~loc first ]
      in
      Some
        (List.fold_left
           (fun acc name -> call ~loc (path ~loc "child") [ acc; B.estring ~loc name ])
           root rest)

let field ~loc place =
  Option.map (fun path -> call ~loc (ast ~loc "field_at") [ path ]) (path_of ~loc place)

(* [a.b.c] as its first name and the names after it; none for anything else. *)
let rec chain (expr : expression) =
  match expr.pexp_desc with
  | Pexp_ident { txt = Lident name; _ } -> Some (name, [])
  | Pexp_field (base, { txt = Lident name; _ }) ->
      Option.map (fun (first, names) -> (first, names @ [ name ])) (chain base)
  | Pexp_field (_, { txt = _; loc }) ->
      refuse loc "a specification reaches a member by its plain name"
  | Pexp_constraint (inner, _) -> chain inner
  | _ -> None

(* The member that [expr] is, if it is one of the candidate or of the item; none if it is
   a value from elsewhere. *)
let place scope (expr : expression) =
  match chain expr with
  | None -> None
  | Some (first, names) -> (
      match base scope first with
      | Base_candidate -> Some { root = Candidate; names }
      | Base_item up -> Some { root = Item up; names }
      | Base_held (Member held) -> Some { root = held.root; names = held.names @ names }
      | Base_held (Outside _) ->
          if names = [] then None
          else
            refuse expr.pexp_loc
              "a member of what a parameter holds is not a value the tree can name"
      | Base_param _ ->
          if names = [] then None
          else
            refuse expr.pexp_loc
              "a member of a parameter is not a value the tree can name: a constant is a \
               parameter of a scalar type"
      | Base_other -> None)

(* Whether [expr] is seen to be an option: [None], [Some _], a parameter declared as one.
   A member that is an option is not seen: the translation has no types. *)
let rec is_option scope (expr : expression) =
  match expr.pexp_desc with
  | Pexp_constraint (inner, _) -> is_option scope inner
  | Pexp_construct ({ txt = Lident ("None" | "Some"); _ }, _) -> true
  | Pexp_ident { txt = Lident name; _ } -> (
      match base scope name with Base_param { optional; _ } -> optional | _ -> false)
  | _ -> false

(* An option is not ordered in a specification. OCaml has a none below every [Some], the
   storage has a null that is neither below nor above: of a none [closed_at < Some 5L] is
   true to the function and null to the tree, and a guard on one side does not make the
   other side a value. What is ordered is what an option holds, where no none is left to
   compare. *)
let ordered scope ~loc operands =
  if List.exists (is_option scope) operands then
    refuse loc
      "an option has no order in a specification: order what it holds, `Option.fold \
       ~none:false ~some:(fun x -> x < 5L) opt` or `match opt with None -> false | Some \
       x -> x < 5L`"

(* ------------------------------------------------------------------------ *)
(* The expression                                                            *)

let name_of (pattern : pattern) =
  let rec name (pattern : pattern) =
    match pattern.ppat_desc with
    | Ppat_var { txt; _ } -> txt
    | Ppat_constraint (inner, _) -> name inner
    | _ -> refuse pattern.ppat_loc "the name is a plain one"
  in
  name pattern

(* [fun name -> body], and nothing else. *)
let closure (expr : expression) what =
  match expr.pexp_desc with
  | Pexp_function
      ( [ { pparam_desc = Pparam_val (Nolabel, None, pattern); _ } ],
        _,
        Pfunction_body body ) ->
      (name_of pattern, body)
  | _ -> refuse expr.pexp_loc "%s" what

let rec expr scope (e : expression) : expression =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constraint (inner, _) -> expr scope inner
  | Pexp_constant (Pconst_integer (_, None)) ->
      constant ~loc (call ~loc (value_of ~loc "of_int") [ e ])
  | Pexp_constant (Pconst_integer (_, Some 'L')) ->
      constant ~loc (construct ~loc "Int" (Some e))
  | Pexp_constant (Pconst_float _) -> constant ~loc (construct ~loc "Float" (Some e))
  | Pexp_constant (Pconst_string _) -> constant ~loc (construct ~loc "Text" (Some e))
  | Pexp_constant _ -> inexpressible e
  | Pexp_construct ({ txt = Lident (("true" | "false") as word); _ }, None) ->
      constant ~loc
        (construct ~loc "Bool" (Some (B.ebool ~loc (String.equal word "true"))))
  (* An option is its value or the null: [None] is the null constant, and [Some x] is
     [x]. *)
  | Pexp_construct ({ txt = Lident "None"; _ }, None) -> null ~loc
  | Pexp_construct ({ txt = Lident "Some"; _ }, Some inner) -> expr scope inner
  | Pexp_construct _ -> inexpressible e
  | Pexp_ident _ | Pexp_field _ -> member scope e
  | Pexp_apply (f, args) -> apply scope e f args
  | Pexp_match (scrutinee, cases) -> matched scope e scrutinee cases
  | _ -> inexpressible e

(* A member of the candidate or of the item, or a parameter. *)
and member scope (e : expression) =
  let loc = e.pexp_loc in
  match place scope e with
  | Some place -> (
      match field ~loc place with
      | Some field -> field
      | None -> refuse loc "the candidate itself is not a value: name one of its members")
  | None -> (
      match chain e with
      | Some (name, []) -> (
          match base scope name with
          | Base_held (Outside outside) -> outside
          | Base_param param -> constant ~loc (param.convert (B.evar ~loc name))
          | _ ->
              refuse loc
                "`%s` is not a member of the candidate, an item, or a parameter: a \
                 constant of a specification is a parameter with a type, or a literal"
                name)
      | _ -> refuse loc "not a member of the candidate, an item, or a parameter")

and apply scope (e : expression) (f : expression) args =
  let loc = e.pexp_loc in
  let operands = List.map snd args in
  let binary make =
    match operands with
    | [ left; right ] when List.for_all (fun (label, _) -> label = Nolabel) args ->
        call ~loc make [ expr scope left; expr scope right ]
    | _ -> inexpressible e
  in
  let order make =
    ordered scope ~loc operands;
    binary make
  in
  let unary make =
    match args with
    | [ (Nolabel, operand) ] -> call ~loc make [ expr scope operand ]
    | _ -> inexpressible e
  in
  match f.pexp_desc with
  | Pexp_ident { txt = Lident op; _ } -> (
      match op with
      | "=" -> binary (null_test ~loc "equal")
      | "<>" -> binary (null_test ~loc "not_equal")
      | "<" -> order (ast ~loc "lt")
      | "<=" -> order (ast ~loc "le")
      | ">" -> order (ast ~loc "gt")
      | ">=" -> order (ast ~loc "ge")
      | "&&" -> binary (ast ~loc "and_")
      | "||" -> binary (ast ~loc "or_")
      | "not" -> unary (ast ~loc "not_")
      | "+" | "+." -> binary (ast ~loc "add")
      | "-" | "-." -> binary (ast ~loc "sub")
      | "*" | "*." -> binary (ast ~loc "mul")
      | "/" | "/." -> binary (ast ~loc "div")
      | "mod" -> binary (ast ~loc "modulo")
      | "lsl" -> binary (ast ~loc "left_shift")
      | "asr" -> binary (ast ~loc "right_shift")
      | "lsr" ->
          refuse loc
            "a logical shift has no counterpart in PostgreSQL: `asr` is the arithmetic \
             one"
      | "~-" | "~-." -> unary (ast ~loc "neg")
      | "==" | "!=" ->
          refuse loc "physical equality has no meaning in a specification: `=` and `<>`"
      | _ -> inexpressible e)
  | Pexp_ident { txt = Ldot (Lident (("Int" | "Int64" | "Float") as m), fn); _ } -> (
      match fn with
      | "add" -> binary (ast ~loc "add")
      | "sub" -> binary (ast ~loc "sub")
      | "mul" -> binary (ast ~loc "mul")
      | "div" -> binary (ast ~loc "div")
      | "rem" when m <> "Float" -> binary (ast ~loc "modulo")
      | "neg" -> unary (ast ~loc "neg")
      | "shift_left" when m <> "Float" -> binary (ast ~loc "left_shift")
      | "shift_right" when m <> "Float" -> binary (ast ~loc "right_shift")
      | "shift_right_logical" ->
          refuse loc
            "a logical shift has no counterpart in PostgreSQL: `shift_right` is the \
             arithmetic one"
      | "equal" -> binary (null_test ~loc "equal")
      | _ -> refuse f.pexp_loc "`%s.%s` has no meaning in a specification" m fn)
  | Pexp_ident { txt = Ldot (Lident (("String" | "Bool") as m), fn); _ } -> (
      match fn with
      | "equal" -> binary (null_test ~loc "equal")
      | _ -> refuse f.pexp_loc "`%s.%s` has no meaning in a specification" m fn)
  | Pexp_ident { txt = Ldot (Lident "Option", fn); _ } -> (
      match (fn, args) with
      | "is_none", [ (Nolabel, option) ] ->
          call ~loc (ast ~loc "is_null") [ expr scope option ]
      | "is_some", [ (Nolabel, option) ] ->
          call ~loc (ast ~loc "is_not_null") [ expr scope option ]
      | "fold", _ -> fold scope e args
      | _ -> refuse f.pexp_loc "`Option.%s` has no meaning in a specification" fn)
  | Pexp_ident { txt = Ldot (Lident "List", fn); _ } -> (
      match (fn, args) with
      | "exists", [ (Nolabel, predicate); (Nolabel, collection) ] ->
          quantifier scope "any_at" collection predicate
      | "for_all", [ (Nolabel, predicate); (Nolabel, collection) ] ->
          quantifier scope "all_at" collection predicate
      | _ -> refuse f.pexp_loc "`List.%s` has no meaning in a specification" fn)
  | _ -> inexpressible e

(* [Option.fold ~none:false ~some:(fun held -> predicate) option]: the option is not null
   and the predicate is true of it; [~none:true]: it is null, or the predicate is. *)
and fold scope (e : expression) args =
  let loc = e.pexp_loc in
  let labelled label =
    List.find_map
      (fun (l, arg) ->
        match l with Labelled l when String.equal l label -> Some arg | _ -> None)
      args
  in
  let positional =
    List.filter_map (fun (l, arg) -> match l with Nolabel -> Some arg | _ -> None) args
  in
  match (labelled "none", labelled "some", positional) with
  | Some none, Some some, [ option ] ->
      let join, test =
        match none.pexp_desc with
        | Pexp_construct ({ txt = Lident "false"; _ }, None) -> ("and_", "is_not_null")
        | Pexp_construct ({ txt = Lident "true"; _ }, None) -> ("or_", "is_null")
        | _ -> refuse none.pexp_loc "what a none folds to is `false` or `true`"
      in
      let name, predicate =
        closure some "the predicate of an option takes what it holds: `fun held -> ...`"
      in
      held scope ~loc ~join ~test option name predicate
  | _ ->
      refuse loc "`Option.fold` in a specification takes `~none`, `~some` and the option"

(* [match option with None -> false | Some held -> predicate], and [None -> true]. *)
and matched scope (e : expression) scrutinee cases =
  let loc = e.pexp_loc in
  let refused () =
    refuse loc
      "a match in a specification is on an option: `None -> false | Some held -> ...`, \
       or `None -> true`"
  in
  let none =
    List.find_map
      (fun case ->
        match (case.pc_lhs.ppat_desc, case.pc_guard, case.pc_rhs.pexp_desc) with
        | ( Ppat_construct ({ txt = Lident "None"; _ }, None),
            None,
            Pexp_construct ({ txt = Lident "false"; _ }, None) ) ->
            Some ("and_", "is_not_null")
        | ( Ppat_construct ({ txt = Lident "None"; _ }, None),
            None,
            Pexp_construct ({ txt = Lident "true"; _ }, None) ) ->
            Some ("or_", "is_null")
        | _ -> None)
      cases
  in
  let some =
    List.find_map
      (fun case ->
        match (case.pc_lhs.ppat_desc, case.pc_guard) with
        | Ppat_construct ({ txt = Lident "Some"; _ }, Some (_, pattern)), None ->
            Some (name_of pattern, case.pc_rhs)
        | _ -> None)
      cases
  in
  match (none, some, List.length cases) with
  | Some (join, test), Some (name, predicate), 2 ->
      held scope ~loc ~join ~test scrutinee name predicate
  | _ -> refused ()

(* The name the closure gives to what is held stands for the option - a member, or a
   parameter - which is its value or the null. The null test beside the predicate is what
   the function means, and it makes the whole of two values as the function has it: of a
   none the predicate is null, and [false AND null] is false, [true OR null] true. So the
   function and its tree agree under a [not] too, which [member < Some 5L] does not. *)
and held scope ~loc ~join ~test option name predicate =
  let nameless () =
    refuse option.pexp_loc
      "an option asked for what it holds is a member of the candidate or of the item, or \
       a parameter"
  in
  let held, option_expr =
    match place scope option with
    | Some place -> (
        match field ~loc place with
        | Some field -> (Member place, field)
        | None -> nameless ())
    | None -> (
        match chain option with
        | Some (name, []) -> (
            match base scope name with
            | Base_param param ->
                let outside = constant ~loc (param.convert (B.evar ~loc name)) in
                (Outside outside, outside)
            | Base_held (Outside outside) -> (Outside outside, outside)
            | _ -> nameless ())
        | _ -> nameless ())
  in
  let predicate = expr (holding scope name held) predicate in
  call ~loc (ast ~loc join) [ call ~loc (ast ~loc test) [ option_expr ]; predicate ]

(* [List.exists (fun item -> predicate) collection], and the same with [for_all]. *)
and quantifier scope make collection predicate =
  let loc = collection.pexp_loc in
  let source =
    match place scope collection with
    | Some place -> (
        match path_of ~loc place with
        | Some path -> path
        | None ->
            refuse loc
              "a collection of a specification is a member of the candidate or of the \
               item")
    | None ->
        refuse loc
          "a collection of a specification is a member of the candidate or of the item"
  in
  let item, predicate =
    closure predicate "the predicate of a collection takes the item: `fun item -> ...`"
  in
  let predicate = expr (inside scope item) predicate in
  call ~loc (ast ~loc make) [ source; predicate ]

(* ------------------------------------------------------------------------ *)
(* The signature                                                             *)

(* How a parameter of the declared type becomes a value. *)
let rec conversion (typ : core_type) =
  let loc = typ.ptyp_loc in
  match typ.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
      { convert = (fun e -> call ~loc (value_of ~loc "of_int") [ e ]); optional = false }
  | Ptyp_constr ({ txt = Lident "int64"; _ }, []) ->
      { convert = (fun e -> construct ~loc "Int" (Some e)); optional = false }
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
      { convert = (fun e -> construct ~loc "Float" (Some e)); optional = false }
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
      { convert = (fun e -> construct ~loc "Text" (Some e)); optional = false }
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
      { convert = (fun e -> construct ~loc "Bool" (Some e)); optional = false }
  | Ptyp_constr
      ( {
          txt =
            ( Ldot (Lident "Value", "t")
            | Ldot (Ldot (Lident "Ascetic_specification", "Value"), "t") );
          _;
        },
        [] ) ->
      { convert = Fun.id; optional = false }
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner ]) ->
      let { convert; _ } = conversion inner in
      let some =
        B.pexp_function ~loc
          [ B.pparam_val ~loc Nolabel None (B.pvar ~loc "held") ]
          None
          (Pfunction_body (convert (B.evar ~loc "held")))
      in
      {
        convert = (fun e -> call ~loc (value_of ~loc "of_option") [ some; e ]);
        optional = true;
      }
  | _ ->
      refuse loc
        "a constant of a specification is a parameter of type int, int64, float, string, \
         bool, Value.t, or an option of one"

let parameter (param : function_param) =
  match param.pparam_desc with
  | Pparam_val (Nolabel, None, pattern) -> pattern
  | Pparam_val _ -> refuse param.pparam_loc "a parameter of a specification is unlabelled"
  | Pparam_newtype _ -> refuse param.pparam_loc "a specification is not generic"

let structure_items ~loc (pat : pattern) (body : expression) =
  let original =
    B.pstr_value ~loc Nonrecursive [ B.value_binding ~loc ~pat ~expr:body ]
  in
  let tree =
    try
      let name =
        match pat.ppat_desc with
        | Ppat_var { txt; _ } -> txt
        | _ -> refuse pat.ppat_loc "a specification is a named function"
      in
      let params, body =
        match body.pexp_desc with
        | Pexp_function (params, constraint_, Pfunction_body body) ->
            (match constraint_ with
            | Some
                (Pconstraint
                   { ptyp_desc = Ptyp_constr ({ txt = Lident "bool"; _ }, []); _ })
            | None ->
                ()
            | Some _ -> refuse body.pexp_loc "a specification returns a bool");
            (List.map parameter params, body)
        | Pexp_function (_, _, Pfunction_cases _) ->
            refuse body.pexp_loc "the body of a specification is one expression"
        | _ ->
            refuse pat.ppat_loc
              "a specification takes its candidate as the first parameter"
      in
      let candidate, constants =
        match params with
        | candidate :: constants -> (name_of candidate, constants)
        | [] ->
            refuse pat.ppat_loc
              "a specification takes its candidate as the first parameter"
      in
      let params =
        List.map
          (fun (pattern : pattern) ->
            match pattern.ppat_desc with
            | Ppat_constraint (inner, typ) -> (name_of inner, conversion typ)
            | _ ->
                refuse pattern.ppat_loc
                  "annotate the parameter with its type: int, int64, float, string, \
                   bool, Value.t, or an option of one")
          constants
      in
      let scope = { candidate; items = []; held = []; params } in
      let tree = expr scope body in
      let tree =
        B.pexp_letmodule ~loc
          { txt = Some "Null_test"; loc }
          (B.pmod_apply ~loc
             (B.pmod_ident ~loc (lid ~loc [ library; "Null_test"; "Make" ]))
             (B.pmod_ident ~loc (lid ~loc [ library; "Value" ])))
          tree
      in
      let tree =
        match constants with
        | [] -> tree
        | constants ->
            B.pexp_function ~loc
              (List.map (fun pattern -> B.pparam_val ~loc Nolabel None pattern) constants)
              None (Pfunction_body tree)
      in
      B.pstr_value ~loc Nonrecursive
        [ B.value_binding ~loc ~pat:(B.pvar ~loc (name ^ "_ast")) ~expr:tree ]
    with Refused (loc, message) ->
      B.pstr_extension ~loc (Location.error_extensionf ~loc "%s" message) []
  in
  [ original; tree ]
