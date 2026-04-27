(** Serializable mirror of [Routing_slip].

    The wire model carries activity *names*, not factories — the factory is
    a closure value that cannot survive transmission. On the receiving side
    an [Activity_resolver] turns each name back into a factory. *)

type value = Yojson.Safe.t

type work_item = {
  activity_type_name : string;
  arguments : (string * value) list;
}

type work_log = {
  activity_type_name : string;
  result : (string * value) list;
}

type routing_slip = {
  completed_work_logs : work_log list;
  next_work_items : work_item list;
}

let work_item_to_json (wi : work_item) : Yojson.Safe.t =
  `Assoc [
    "activityTypeName", `String wi.activity_type_name;
    "arguments", `Assoc wi.arguments;
  ]

let work_log_to_json (wl : work_log) : Yojson.Safe.t =
  `Assoc [
    "activityTypeName", `String wl.activity_type_name;
    "result", `Assoc wl.result;
  ]

let to_json (rs : routing_slip) : Yojson.Safe.t =
  `Assoc [
    "completedWorkLogs",
    `List (List.map work_log_to_json rs.completed_work_logs);
    "nextWorkItems",
    `List (List.map work_item_to_json rs.next_work_items);
  ]

let to_string (rs : routing_slip) : string =
  Yojson.Safe.to_string (to_json rs)

let assoc_of_json (j : Yojson.Safe.t) : ((string * value) list, string) result =
  match j with
  | `Assoc kvs -> Ok kvs
  | `Null -> Ok []
  | _ -> Error "expected JSON object"

let string_field (kvs : (string * Yojson.Safe.t) list) (key : string)
  : (string, string) result =
  match List.assoc_opt key kvs with
  | Some (`String s) -> Ok s
  | Some _ -> Error (Printf.sprintf "field %S is not a string" key)
  | None -> Error (Printf.sprintf "missing field %S" key)

let assoc_field (kvs : (string * Yojson.Safe.t) list) (key : string)
  : ((string * value) list, string) result =
  match List.assoc_opt key kvs with
  | Some j -> assoc_of_json j
  | None -> Error (Printf.sprintf "missing field %S" key)

let list_field (kvs : (string * Yojson.Safe.t) list) (key : string)
  : (Yojson.Safe.t list, string) result =
  match List.assoc_opt key kvs with
  | Some (`List xs) -> Ok xs
  | Some `Null -> Ok []
  | Some _ -> Error (Printf.sprintf "field %S is not an array" key)
  | None -> Ok []

let work_item_of_json (j : Yojson.Safe.t) : (work_item, string) result =
  match j with
  | `Assoc kvs ->
    Result.bind (string_field kvs "activityTypeName") (fun name ->
      Result.bind (assoc_field kvs "arguments") (fun args ->
        Ok { activity_type_name = name; arguments = args }))
  | _ -> Error "expected JSON object for work item"

let work_log_of_json (j : Yojson.Safe.t) : (work_log, string) result =
  match j with
  | `Assoc kvs ->
    Result.bind (string_field kvs "activityTypeName") (fun name ->
      Result.bind (assoc_field kvs "result") (fun r ->
        Ok { activity_type_name = name; result = r }))
  | _ -> Error "expected JSON object for work log"

let collect (xs : ('a, string) result list) : ('a list, string) result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: rest -> loop (x :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] xs

let of_json (j : Yojson.Safe.t) : (routing_slip, string) result =
  match j with
  | `Assoc kvs ->
    Result.bind (list_field kvs "completedWorkLogs") (fun logs_json ->
      Result.bind (list_field kvs "nextWorkItems") (fun items_json ->
        Result.bind
          (collect (List.map work_log_of_json logs_json))
          (fun logs ->
            Result.bind
              (collect (List.map work_item_of_json items_json))
              (fun items ->
                Ok { completed_work_logs = logs; next_work_items = items }))))
  | _ -> Error "expected JSON object for routing slip"

let of_string (s : string) : (routing_slip, string) result =
  match Yojson.Safe.from_string s with
  | exception (Yojson.Json_error msg) -> Error msg
  | j -> of_json j

let to_serializable
      (rs : Routing_slip.t)
      (resolver : Activity_resolver.t)
  : (routing_slip, string) result =
  let logs_r =
    Routing_slip.completed_work_logs rs
    |> List.mapi (fun i log ->
      match Activity_resolver.get_name resolver (Work_log.factory log) with
      | Ok name ->
        Ok { activity_type_name = name; result = Work_log.result log }
      | Error e ->
        Error (Printf.sprintf "cannot serialize work log %d: %s" i e))
  in
  Result.bind (collect logs_r) (fun logs ->
    let items_r =
      Routing_slip.pending_work_items rs
      |> List.mapi (fun i item ->
        match Activity_resolver.get_name resolver (Work_item.factory item) with
        | Ok name ->
          Ok { activity_type_name = name; arguments = Work_item.arguments item }
        | Error e ->
          Error (Printf.sprintf "cannot serialize work item %d: %s" i e))
    in
    Result.bind (collect items_r) (fun items ->
      Ok { completed_work_logs = logs; next_work_items = items }))

let from_serializable
      (srs : routing_slip)
      (resolver : Activity_resolver.t)
  : (Routing_slip.t, string) result =
  let logs_r =
    srs.completed_work_logs
    |> List.mapi (fun i (slog : work_log) ->
      match Activity_resolver.resolve resolver slog.activity_type_name with
      | Ok factory ->
        Ok (Work_log.create_with_factory ~factory ~result:slog.result)
      | Error e ->
        Error (Printf.sprintf "cannot deserialize work log %d: %s" i e))
  in
  Result.bind (collect logs_r) (fun logs ->
    let items_r =
      srs.next_work_items
      |> List.mapi (fun i (sitem : work_item) ->
        match Activity_resolver.resolve resolver sitem.activity_type_name with
        | Ok factory ->
          Ok (Work_item.create ~factory ~arguments:sitem.arguments)
        | Error e ->
          Error (Printf.sprintf "cannot deserialize work item %d: %s" i e))
    in
    Result.bind (collect items_r) (fun items ->
      let rs : Routing_slip.t =
        { Saga_types.completed_work_logs = logs; next_work_items = items }
      in
      Ok rs))
