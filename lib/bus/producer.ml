type 'a t = { wire : Adapter.producer; encode : 'a -> Message.t; stages : Stage.t list }

let make wire ~encode = { wire; encode; stages = [] }
let through t stage = { t with stages = t.stages @ [ stage ] }

let publish t value =
  match Stage.outbound t.stages (t.encode value) with
  | Error failure -> Error (Bus_error.Stage failure)
  | Ok message -> t.wire.publish message
