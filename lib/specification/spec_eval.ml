(** In-memory evaluator for specification AST.

    Resolves paths against a [value] tree and evaluates expressions
    to produce a boolean result (for specs) or a value (general use).
    Supports placeholder resolution from an environment map. *)

open Spec_ast

type env = (string * value) list

let value_to_bool = function
  | Bool b -> b
  | Null -> false
  | Int 0 -> false
  | Str "" -> false
  | List [] -> false
  | _ -> true

(* === Path resolution === *)

let rec resolve_path (env : env) (path : path) (root : value) : value list =
  (* Check if the first path segment is a bound alias *)
  match path with
  | Field alias_name :: rest -> (
      match List.assoc_opt alias_name env with
      | Some alias_val -> resolve_steps env rest alias_val
      | None -> resolve_steps env path root)
  | _ -> resolve_steps env path root

and resolve_steps env steps v =
  match steps, v with
  | [], _ -> [ v ]
  | Field f :: rest, Record fields -> (
      match List.assoc_opt f fields with
      | Some child -> resolve_steps env rest child
      | None -> [])
  | Wildcard :: rest, List items ->
      List.concat_map (resolve_steps env rest) items
  | Index i :: rest, List items -> (
      match List.nth_opt items i with
      | Some item -> resolve_steps env rest item
      | None -> [])
  | Filter pred :: rest, List items ->
      let matching =
        List.filter
          (fun item ->
            match eval env item pred with Bool true -> true | _ -> false)
          items
      in
      List.concat_map (resolve_steps env rest) matching
  | _ -> []

(* === Expression evaluation === *)

and eval (env : env) (root : value) (expr : expr) : value =
  match expr with
  | Lit v -> v
  | Path path -> (
      match resolve_path env path root with [ v ] -> v | vs -> List vs)
  | Placeholder name -> (
      match List.assoc_opt name env with Some v -> v | None -> Null)
  | AliasRef name -> (
      match List.assoc_opt name env with Some v -> v | None -> Null)
  | BinOp (op, lhs, rhs) -> eval_binop env root op lhs rhs
  | UnaryOp (Not, e) -> Bool (not (value_to_bool (eval env root e)))
  | UnaryOp (Neg, e) -> (
      match eval env root e with
      | Int i -> Int (-i)
      | Float f -> Float (-.f)
      | _ -> Null)
  | Call (fname, args) -> eval_call env root fname args
  | Exists (path, alias, pred) ->
      let resolved = resolve_path env path root in
      (* Unwrap: if path resolved to a single List, iterate over its items *)
      let items =
        match resolved with [ List items ] -> items | other -> other
      in
      Bool
        (List.exists
           (fun item ->
             value_to_bool (eval ((alias, item) :: env) item pred))
           items)
  | ForAll (path, alias, pred) ->
      let resolved = resolve_path env path root in
      let items =
        match resolved with [ List items ] -> items | other -> other
      in
      Bool
        (List.for_all
           (fun item ->
             value_to_bool (eval ((alias, item) :: env) item pred))
           items)

and eval_binop env root op lhs rhs =
  let l = eval env root lhs in
  let r = eval env root rhs in
  match op with
  | And -> Bool (value_to_bool l && value_to_bool r)
  | Or -> Bool (value_to_bool l || value_to_bool r)
  | Eq -> Bool (l = r)
  | Neq -> Bool (l <> r)
  | Lt -> Bool (compare l r < 0)
  | Gt -> Bool (compare l r > 0)
  | Lte -> Bool (compare l r <= 0)
  | Gte -> Bool (compare l r >= 0)
  | In -> (
      match r with List items -> Bool (List.mem l items) | _ -> Bool false)
  | Add -> (
      match l, r with
      | Int a, Int b -> Int (a + b)
      | Float a, Float b -> Float (a +. b)
      | Str a, Str b -> Str (a ^ b)
      | Int a, Float b -> Float (float_of_int a +. b)
      | Float a, Int b -> Float (a +. float_of_int b)
      | _ -> Null)
  | Sub -> (
      match l, r with
      | Int a, Int b -> Int (a - b)
      | Float a, Float b -> Float (a -. b)
      | _ -> Null)
  | Mul -> (
      match l, r with
      | Int a, Int b -> Int (a * b)
      | Float a, Float b -> Float (a *. b)
      | _ -> Null)
  | Div -> (
      match l, r with
      | Int a, Int b when b <> 0 -> Int (a / b)
      | Float a, Float b when b <> 0.0 -> Float (a /. b)
      | _ -> Null)

and eval_call env root fname args =
  let evaled = List.map (eval env root) args in
  match fname, evaled with
  | "size", [ List l ] -> Int (List.length l)
  | "size", [ Str s ] -> Int (String.length s)
  | "startsWith", [ Str s; Str prefix ] ->
      Bool
        (String.length s >= String.length prefix
        && String.sub s 0 (String.length prefix) = prefix)
  | "endsWith", [ Str s; Str suffix ] ->
      let sl = String.length s and xl = String.length suffix in
      Bool (sl >= xl && String.sub s (sl - xl) xl = suffix)
  | "contains", [ Str s; Str sub ] ->
      let rec check i =
        if i + String.length sub > String.length s then false
        else if String.sub s i (String.length sub) = sub then true
        else check (i + 1)
      in
      Bool (check 0)
  | "contains", [ List items; v ] -> Bool (List.mem v items)
  | "lower", [ Str s ] -> Str (String.lowercase_ascii s)
  | "upper", [ Str s ] -> Str (String.uppercase_ascii s)
  | _ -> Null

(* === Public API === *)

let evaluate ?(env = []) root expr = eval env root expr

let satisfies ?(env = []) root spec = value_to_bool (eval env root spec.expr)
