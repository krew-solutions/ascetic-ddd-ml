(** Resolver mapping activity type names to factories.

    Serialization carries activity type names; deserialization rebuilds the
    routing slip by looking up factories through a resolver. Each service
    can hold its own resolver — there is no global registry — which makes
    isolation between services and tests straightforward. *)

type t = {
  resolve : string -> (Saga_types.factory, string) result;
  get_name : Saga_types.factory -> (string, string) result;
}

let resolve (r : t) (name : string) : (Saga_types.factory, string) result =
  r.resolve name

let get_name (r : t) (f : Saga_types.factory) : (string, string) result =
  r.get_name f

let create ~resolve ~get_name : t = { resolve; get_name }

(** A simple in-memory resolver that maintains a [name -> factory] mapping
    and uses the factory's produced [Activity.t] name for the reverse
    lookup. *)
module Map_based = struct
  type resolver = t

  type t = {
    mutable name_to_factory : (string * Saga_types.factory) list;
  }

  let empty () : t = { name_to_factory = [] }

  let register (r : t) ~(name : string) ~(factory : Saga_types.factory) : unit =
    let filtered =
      List.filter (fun (n, _) -> not (String.equal n name)) r.name_to_factory
    in
    r.name_to_factory <- filtered @ [ (name, factory) ]

  let lookup_factory (r : t) (name : string) : Saga_types.factory option =
    List.assoc_opt name r.name_to_factory

  let lookup_name (r : t) (f : Saga_types.factory) : string option =
    let activity = f () in
    let target = Activity.name activity in
    List.find_map
      (fun (n, candidate) ->
        let cand_name = Activity.name (candidate ()) in
        if String.equal cand_name target then Some n else None)
      r.name_to_factory

  let to_resolver (r : t) : resolver =
    let resolve name =
      match lookup_factory r name with
      | Some f -> Ok f
      | None ->
        Error (Printf.sprintf "activity type not registered: %s" name)
    in
    let get_name factory =
      match lookup_name r factory with
      | Some n -> Ok n
      | None ->
        let activity = factory () in
        let n = Activity.name activity in
        if n = "" then Error "activity type not registered"
        else Ok n
    in
    create ~resolve ~get_name
end
