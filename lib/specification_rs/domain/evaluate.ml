type error =
  | Context of Context.error
  | No_current_item
  | Not_boolean of string
  | Operand of Operand.error
[@@deriving show { with_path = false }, eq]

let error_to_string = function
  | Context error -> Context.error_to_string error
  | No_current_item -> "no current item in context"
  | Not_boolean kind -> Printf.sprintf "a boolean was expected, got %s" kind
  | Operand error -> Operand.error_to_string error

let ( let* ) = Result.bind

module Make (O : Operand.S) = struct
  (* The item of one collection's predicate, inside the item of the enclosing
     collection's. *)
  type frame = { context : O.t Context.t; outer : frame option }

  (* What the roots of a path stand for at this point of the tree. Entering a collection
     makes a new scope rather than changing this one: the sources' [_with_item]. *)
  type scope = { global : O.t Context.t; item : frame option }

  (* True, false, or - [None] - unknown. *)
  let truth value =
    if O.is_null value then Ok None
    else
      match O.to_bool value with
      | Some value -> Ok (Some value)
      | None -> Error (Not_boolean (O.kind value))

  let of_truth = function None -> O.null | Some value -> O.of_bool value

  (* Three-valued [AND] and [OR]. [deciding] is the value that settles the result
     whatever the other side is: false for [AND], true for [OR]. *)
  let connective (op : Operator.logical) left right =
    let deciding = match op with And -> false | Or -> true in
    if left = Some deciding then Ok left
    else
      let* right = right () in
      Ok
        (match (left, right) with
        | _, Some right when right = deciding -> Some deciding
        | Some _, Some _ -> Some (not deciding)
        | _ -> None)

  (* A null operand makes a null result; [f] sees only values. *)
  let strict left right f =
    if O.is_null left || O.is_null right then Ok O.null else f left right

  let compare left (op : Operator.comparison) right =
    match op with
    | Eq -> O.equals left right
    | Ne -> Result.map not (O.equals left right)
    | Gt -> Result.map (fun order -> order > 0) (O.compare left right)
    | Lt -> Result.map (fun order -> order < 0) (O.compare left right)
    | Ge -> Result.map (fun order -> order >= 0) (O.compare left right)
    | Le -> Result.map (fun order -> order <= 0) (O.compare left right)

  (* An operand type reports the operator it implements the asked one with - [<] for
     every ordering; the error names the one the tree has. *)
  let named operator = function
    | Operand.Unsupported { left; right; _ } ->
        Operand (Operand.Unsupported { operator; left; right })
    | other -> Operand other

  (* The item [up] collections out from the item under test. *)
  let rec item_out frame up =
    match (frame, up) with
    | None, _ -> Error No_current_item
    | Some frame, 0 -> Ok frame.context
    | Some frame, up -> item_out frame.outer (up - 1)

  (* The context that has the member the path names. *)
  let owner path scope =
    let* root =
      match Path.root path with
      | Global -> Ok scope.global
      | Item up -> item_out scope.item up
    in
    List.fold_left
      (fun context name ->
        let* (context : O.t Context.t) = context in
        Result.map_error (fun error -> Context error) (context.object_ name))
      (Ok root) (Path.objects path)

  let rec eval expr scope =
    match expr with
    | Ast.Value value -> Ok value
    | Field path ->
        let* owner = owner path scope in
        Result.map_error (fun error -> Context error) (owner.field (Path.name path))
    | Prefix (Not, operand) ->
        let* operand = eval operand scope in
        let* truth = truth operand in
        Ok (of_truth (Option.map not truth))
    | Prefix (Neg, operand) ->
        let* operand = eval operand scope in
        if O.is_null operand then Ok O.null
        else Result.map_error (fun error -> Operand error) (O.negate operand)
    | Postfix (operand, Is_null) -> Result.map O.of_bool (is_null operand scope)
    | Postfix (operand, Is_not_null) ->
        Result.map (fun null -> O.of_bool (not null)) (is_null operand scope)
    | Infix (left, Logical op, right) ->
        let* left = eval left scope in
        let* left = truth left in
        let right () =
          let* right = eval right scope in
          truth right
        in
        Result.map of_truth (connective op left right)
    | Infix (left, Is, right) ->
        let* left = eval left scope in
        let* right = eval right scope in
        if O.is_null left || O.is_null right then
          Ok (O.of_bool (O.is_null left && O.is_null right))
        else
          O.equals left right |> Result.map O.of_bool
          |> Result.map_error (named (Operator.infix_to_string Is))
    | Infix (left, Comparison op, right) ->
        let* left = eval left scope in
        let* right = eval right scope in
        strict left right (fun left right -> Result.map O.of_bool (compare left op right))
        |> Result.map_error (named (Operator.comparison_to_string op))
    | Infix (left, Arithmetic op, right) ->
        let* left = eval left scope in
        let* right = eval right scope in
        strict left right (fun left right -> O.compute op left right)
        |> Result.map_error (named (Operator.arithmetic_to_string op))
    | Any (source, predicate) ->
        let* owner = owner source scope in
        let* items =
          Result.map_error
            (fun error -> Context error)
            (owner.collection (Path.name source))
        in
        let witness item =
          let scope = { scope with item = Some { context = item; outer = scope.item } } in
          let* value = eval predicate scope in
          let* truth = truth value in
          Ok (truth = Some true)
        in
        (* The first witness decides; so does the first failure. *)
        let rec any = function
          | [] -> Ok false
          | item :: items -> (
              match witness item with Ok false -> any items | decided -> decided)
        in
        Result.map O.of_bool (any items)

  (* Whether [operand] is null. A member is asked about whatever it is: an object that is
     there is not null, though it is no value - the guard a host-language frontend writes
     for "is some and" over a Value Object, [discount IS NOT NULL], asks that of an
     object. An object that is not there is a null value in the context, and null.
     Anything else is evaluated, and is null or is not. *)
  and is_null operand scope =
    match operand with
    | Ast.Field path -> (
        let* owner = owner path scope in
        match owner.field (Path.name path) with
        | Ok value -> Ok (O.is_null value)
        | Error (Context.Not_a_value _) ->
            Result.map_error
              (fun error -> Context error)
              (Result.map (fun _ -> false) (owner.object_ (Path.name path)))
        | Error error -> Error (Context error))
    | _ ->
        let* value = eval operand scope in
        Ok (O.is_null value)

  let evaluate expr candidate = eval expr { global = candidate; item = None }

  let is_satisfied_by specification candidate =
    let* value = evaluate specification candidate in
    let* truth = truth value in
    Ok (truth = Some true)
end
