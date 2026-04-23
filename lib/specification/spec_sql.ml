(** SQL compiler for specification AST.

    Translates a spec AST into a parameterized SQL query.
    Handles composite primary keys, JOINs for nested entities,
    EXISTS/NOT EXISTS for collection predicates,
    and $placeholder compilation to positional parameters. *)

open Spec_ast

type sql_fragment = { sql : string; params : value list }

type compiled_query = {
  select : string;
  from : string;
  joins : string list;
  where : sql_fragment;
  params : value list;
}

(* === Context for compilation === *)

type compile_ctx = {
  schema : schema;
  root_alias : string;
  alias_counter : int ref;
  join_acc : string list ref;
  param_acc : value list ref;
  alias_table : (string * string * table_schema) list ref;
  placeholder_env : (string * value) list;
}

let fresh_alias ctx prefix =
  let n = !(ctx.alias_counter) in
  ctx.alias_counter := n + 1;
  Printf.sprintf "%s%d" prefix n

let find_table ctx table_name =
  match List.assoc_opt table_name ctx.schema.tables with
  | Some t -> t
  | None -> failwith ("unknown table: " ^ table_name)

let find_root_table ctx = find_table ctx ctx.schema.root_table

let add_param ctx v =
  ctx.param_acc := !(ctx.param_acc) @ [ v ];
  let idx = List.length !(ctx.param_acc) in
  Printf.sprintf "$%d" idx

(* === Resolve a field path to SQL column reference === *)

let resolve_field_path ctx (path : path) : string =
  let rec walk alias (tbl : table_schema) remaining =
    match remaining with
    | [] -> failwith "empty path"
    | [ Field col ] -> Printf.sprintf "%s.%s" alias col
    | Field rel_name :: rest -> (
        match List.assoc_opt rel_name tbl.relations with
        | Some (OneToMany { child_table; fk }) ->
            let child_tbl = find_table ctx child_table in
            let child_alias = fresh_alias ctx "j" in
            let join_cond =
              fk
              |> List.map (fun (pcol, ccol) ->
                     Printf.sprintf "%s.%s = %s.%s" alias pcol child_alias
                       ccol)
              |> String.concat " AND "
            in
            ctx.join_acc :=
              !(ctx.join_acc)
              @ [
                  Printf.sprintf "LEFT JOIN %s %s ON %s" child_table
                    child_alias join_cond;
                ];
            walk child_alias child_tbl rest
        | Some (ManyToMany { junction_table; left_fk; right_fk; target_table })
          ->
            let junc_alias = fresh_alias ctx "jn" in
            let target_tbl = find_table ctx target_table in
            let target_alias = fresh_alias ctx "j" in
            let left_cond =
              left_fk
              |> List.map (fun (pcol, jcol) ->
                     Printf.sprintf "%s.%s = %s.%s" alias pcol junc_alias
                       jcol)
              |> String.concat " AND "
            in
            let right_cond =
              right_fk
              |> List.map (fun (jcol, tcol) ->
                     Printf.sprintf "%s.%s = %s.%s" junc_alias jcol
                       target_alias tcol)
              |> String.concat " AND "
            in
            ctx.join_acc :=
              !(ctx.join_acc)
              @ [
                  Printf.sprintf "LEFT JOIN %s %s ON %s" junction_table
                    junc_alias left_cond;
                  Printf.sprintf "LEFT JOIN %s %s ON %s" target_table
                    target_alias right_cond;
                ];
            walk target_alias target_tbl rest
        | None ->
            let col_ref = Printf.sprintf "%s.%s" alias rel_name in
            let json_path =
              rest
              |> List.filter_map (function Field f -> Some f | _ -> None)
              |> String.concat "."
            in
            Printf.sprintf "%s->>'%s'" col_ref json_path)
    | Wildcard :: _ ->
        failwith
          "wildcard in field path not directly supported — use exists/forall"
    | Index i :: rest ->
        let prev = Printf.sprintf "%s[%d]" alias i in
        walk prev tbl rest
    | Filter _ :: _ ->
        failwith
          "filter in SQL path not directly supported — use exists/forall"
  in
  match path with
  | Field alias_name :: rest
    when List.exists (fun (n, _, _) -> n = alias_name) !(ctx.alias_table) ->
      let _, sql_alias, tbl =
        List.find (fun (n, _, _) -> n = alias_name) !(ctx.alias_table)
      in
      walk sql_alias tbl rest
  | _ ->
      let root_tbl = find_root_table ctx in
      walk ctx.root_alias root_tbl path

(* === Compile expression to SQL WHERE fragment === *)

let binop_to_sql = function
  | Eq -> "="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "AND"
  | Or -> "OR"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | In -> "IN"

let rec compile_expr ctx (expr : expr) : string =
  match expr with
  | Lit v -> add_param ctx v
  | Path path -> resolve_field_path ctx path
  | Placeholder name -> (
      match List.assoc_opt name ctx.placeholder_env with
      | Some v -> add_param ctx v
      | None -> Printf.sprintf "$%s" name (* unresolved placeholder *))
  | AliasRef name -> (
      match
        List.find_opt (fun (n, _, _) -> n = name) !(ctx.alias_table)
      with
      | Some (_, sql_alias, _) -> sql_alias
      | None -> failwith ("unbound alias: " ^ name))
  | BinOp (In, lhs, Lit (List items)) ->
      let l = compile_expr ctx lhs in
      let placeholders =
        items |> List.map (add_param ctx) |> String.concat ", "
      in
      Printf.sprintf "%s IN (%s)" l placeholders
  | BinOp (And, lhs, rhs) ->
      let l = compile_expr ctx lhs in
      let r = compile_expr ctx rhs in
      Printf.sprintf "(%s AND %s)" l r
  | BinOp (Or, lhs, rhs) ->
      let l = compile_expr ctx lhs in
      let r = compile_expr ctx rhs in
      Printf.sprintf "(%s OR %s)" l r
  | BinOp (op, lhs, rhs) ->
      let l = compile_expr ctx lhs in
      let r = compile_expr ctx rhs in
      Printf.sprintf "%s %s %s" l (binop_to_sql op) r
  | UnaryOp (Not, e) -> Printf.sprintf "NOT (%s)" (compile_expr ctx e)
  | UnaryOp (Neg, e) -> Printf.sprintf "-(%s)" (compile_expr ctx e)
  | Call (fname, args) -> compile_call ctx fname args
  | Exists (path, alias, pred) ->
      compile_quantifier ctx "EXISTS" path alias pred
  | ForAll (path, alias, pred) ->
      compile_quantifier ctx "NOT EXISTS" path alias (UnaryOp (Not, pred))

and compile_call ctx fname args =
  let compiled = List.map (compile_expr ctx) args in
  match fname, compiled with
  | "size", [ a ] -> Printf.sprintf "COALESCE(array_length(%s, 1), 0)" a
  | "lower", [ a ] -> Printf.sprintf "LOWER(%s)" a
  | "upper", [ a ] -> Printf.sprintf "UPPER(%s)" a
  | "startsWith", [ a; b ] -> Printf.sprintf "%s LIKE %s || '%%'" a b
  | "endsWith", [ a; b ] -> Printf.sprintf "%s LIKE '%%' || %s" a b
  | "contains", [ a; b ] ->
      Printf.sprintf "%s LIKE '%%' || %s || '%%'" a b
  | f, _ -> failwith ("unsupported SQL function: " ^ f)

and compile_quantifier ctx kind (path : path) alias pred =
  let rec find_relation current_tbl current_alias = function
    | [] -> failwith "quantifier path must reference a collection"
    | [ Field rel_name ] -> (
        match List.assoc_opt rel_name current_tbl.relations with
        | Some (OneToMany { child_table; fk }) ->
            let child_tbl = find_table ctx child_table in
            let child_alias = fresh_alias ctx "sq" in
            let join_cond =
              fk
              |> List.map (fun (pcol, ccol) ->
                     Printf.sprintf "%s.%s = %s.%s" current_alias pcol
                       child_alias ccol)
              |> String.concat " AND "
            in
            let prev_aliases = !(ctx.alias_table) in
            ctx.alias_table :=
              (alias, child_alias, child_tbl) :: prev_aliases;
            let pred_sql = compile_expr ctx pred in
            ctx.alias_table := prev_aliases;
            Printf.sprintf "%s (SELECT 1 FROM %s %s WHERE %s AND %s)" kind
              child_table child_alias join_cond pred_sql
        | Some
            (ManyToMany
              { junction_table; left_fk; right_fk; target_table }) ->
            let junc_alias = fresh_alias ctx "sq" in
            let target_tbl = find_table ctx target_table in
            let target_alias = fresh_alias ctx "sq" in
            let left_cond =
              left_fk
              |> List.map (fun (pcol, jcol) ->
                     Printf.sprintf "%s.%s = %s.%s" current_alias pcol
                       junc_alias jcol)
              |> String.concat " AND "
            in
            let right_cond =
              right_fk
              |> List.map (fun (jcol, tcol) ->
                     Printf.sprintf "%s.%s = %s.%s" junc_alias jcol
                       target_alias tcol)
              |> String.concat " AND "
            in
            let prev_aliases = !(ctx.alias_table) in
            ctx.alias_table :=
              (alias, target_alias, target_tbl) :: prev_aliases;
            let pred_sql = compile_expr ctx pred in
            ctx.alias_table := prev_aliases;
            Printf.sprintf
              "%s (SELECT 1 FROM %s %s JOIN %s %s ON %s WHERE %s AND %s)"
              kind junction_table junc_alias target_table target_alias
              right_cond left_cond pred_sql
        | None -> failwith ("not a relation: " ^ rel_name))
    | Field f :: rest -> (
        match List.assoc_opt f current_tbl.relations with
        | Some (OneToMany { child_table; fk }) ->
            let child_tbl = find_table ctx child_table in
            let child_alias = fresh_alias ctx "nav" in
            let join_cond =
              fk
              |> List.map (fun (pcol, ccol) ->
                     Printf.sprintf "%s.%s = %s.%s" current_alias pcol
                       child_alias ccol)
              |> String.concat " AND "
            in
            ctx.join_acc :=
              !(ctx.join_acc)
              @ [
                  Printf.sprintf "JOIN %s %s ON %s" child_table child_alias
                    join_cond;
                ];
            find_relation child_tbl child_alias rest
        | _ -> failwith ("cannot navigate through: " ^ f))
    | _ -> failwith "unsupported path step in quantifier"
  in
  let root_tbl = find_root_table ctx in
  find_relation root_tbl ctx.root_alias path

(* === Public API === *)

let compile ?(placeholders = []) schema (spec : spec) : compiled_query =
  let root_tbl =
    match List.assoc_opt schema.root_table schema.tables with
    | Some t -> t
    | None -> failwith "root table not found"
  in
  let root_alias = "r0" in
  let ctx =
    {
      schema;
      root_alias;
      alias_counter = ref 0;
      join_acc = ref [];
      param_acc = ref [];
      alias_table = ref [ ("root", root_alias, root_tbl) ];
      placeholder_env = placeholders;
    }
  in
  let where_sql = compile_expr ctx spec.expr in
  {
    select =
      Printf.sprintf "SELECT %s.* FROM %s %s" root_alias schema.root_table
        root_alias;
    from = schema.root_table;
    joins = !(ctx.join_acc);
    where = { sql = where_sql; params = !(ctx.param_acc) };
    params = !(ctx.param_acc);
  }

let to_sql query =
  let joins =
    match query.joins with [] -> "" | js -> " " ^ String.concat " " js
  in
  Printf.sprintf "%s%s WHERE %s" query.select joins query.where.sql

let param_to_string = function
  | Str s -> Printf.sprintf "'%s'" s
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool true -> "TRUE"
  | Bool false -> "FALSE"
  | Null -> "NULL"
  | List _ | Record _ -> "<complex>"

let to_sql_debug query =
  let raw = to_sql query in
  let params_str =
    query.params
    |> List.mapi (fun i p ->
           Printf.sprintf "  $%d = %s" (i + 1) (param_to_string p))
    |> String.concat "\n"
  in
  Printf.sprintf "%s\n\nParams:\n%s" raw params_str
