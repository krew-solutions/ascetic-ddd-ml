type 's wire_producer = { publish : 's -> Message.t -> (unit, Bus_error.t) result }
type 's handler = 's -> Message.t -> (unit, Failure.t) result
type 's wire_consumer = { subscribe : 's handler -> (Subscription.t, Bus_error.t) result }

module Producer = struct
  type ('a, 's) t = {
    wire : 's wire_producer;
    encode : 'a -> Message.t;
    stages : Stage.t list;
  }

  let make wire ~encode = { wire; encode; stages = [] }
  let through t stage = { t with stages = t.stages @ [ stage ] }

  let publish t session value =
    match Stage.outbound t.stages (t.encode value) with
    | Error failure -> Error (Bus_error.Stage failure)
    | Ok message -> t.wire.publish session message
end

module Consumer = struct
  type ('a, 's) t = {
    wire : 's wire_consumer;
    decode : Message.t -> ('a, string) result;
    stages : Stage.t list;
  }

  let make wire ~decode = { wire; decode; stages = [] }
  let through t stage = { t with stages = t.stages @ [ stage ] }

  let subscribe t handler =
    t.wire.subscribe (fun session message ->
        Result.bind (Stage.inbound t.stages message) (fun message ->
            match t.decode message with
            | Ok value -> handler session value
            | Error reason ->
                (* A message the decoder cannot read will not be read on a
                   retry either: the verdict is permanent, and the inbox parks
                   it where an operator can see it, instead of marking it
                   processed as if it had been handled. *)
                Log.warn (fun m ->
                    m "bus[transactional]: decoding failed, parking: %s" reason);
                Error (Failure.permanent reason)))
end
