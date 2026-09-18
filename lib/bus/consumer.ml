type 'a t = {
  uri : string;
  group : string;
  wire : Adapter.consumer;
  decode : Message.t -> ('a, string) result;
  stages : Stage.t list;
}

let make ~uri ~group wire ~decode = { uri; group; wire; decode; stages = [] }
let through t stage = { t with stages = t.stages @ [ stage ] }

let subscribe t handler =
  t.wire.subscribe (fun message ->
      Result.bind (Stage.inbound t.stages message) (fun message ->
          match t.decode message with
          | Ok value -> handler value
          | Error reason ->
              Log.warn (fun m -> m "bus[%s/%s]: decoding failed: %s" t.uri t.group reason);
              Ok ()))
