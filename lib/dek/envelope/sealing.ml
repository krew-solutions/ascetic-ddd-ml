module Message = Ascetic_bus.Message
module Failure = Ascetic_bus.Failure
module Stage = Ascetic_bus.Stage
module Bus_uri = Ascetic_bus.Bus_uri

type parameters = { reuse : Reuse.t; cache : float }

let default_parameters = { reuse = Reuse.default; cache = 300.0 }

type unsealed_channel = { destination : string; channel : string }

let pp_unsealed_channel ppf { destination; channel } =
  Format.fprintf ppf
    "the sealing was not built for the channel `%s`, which the destination `%s` names"
    channel destination

(* The header an inbox row goes out with: the channel it was received on. *)
let destination_header = "destination"

module By_destination = struct
  type t = { stages : (string * Stage.t) list }

  let destinations t = List.map fst t.stages
  let refused what = Error (Failure.permanent what)

  let stage_of t message =
    match Message.header message destination_header with
    | None -> refused (Printf.sprintf "the message has no `%s` header" destination_header)
    | Some destination when not (String.is_valid_utf_8 destination) ->
        refused (Printf.sprintf "the `%s` header is not text" destination_header)
    | Some destination -> (
        match List.assoc_opt (Bus_uri.without_key destination) t.stages with
        | Some stage -> Ok stage
        | None -> refused (Printf.sprintf "no stage for the destination `%s`" destination)
        )

  let stage t : Stage.t =
    {
      outbound =
        (fun message ->
          Result.bind (stage_of t message) (fun stage -> stage.outbound message));
      inbound =
        (fun message ->
          Result.bind (stage_of t message) (fun stage -> stage.inbound message));
    }
end

module Make
    (Sessions : Ascetic_session.Session_pool.S)
    (Kms : Ascetic_kms.Kms_port.S with type session = Sessions.session) =
struct
  module Cached_kms = Ascetic_kms.Cached.Make (Kms)
  module Envelope = Envelope_stage.Make (Sessions) (Cached_kms)

  type t = { stages : (string * Stage.t) list }

  let create ~clock ?(parameters = default_parameters) sessions ~channels kms =
    let stages =
      List.fold_left
        (fun stages channel ->
          if List.mem_assoc channel stages then stages
          else
            let cached = Cached_kms.create ~ttl:parameters.cache ~clock (kms channel) in
            let stage =
              Envelope.reusing (Envelope.create sessions cached) ~clock parameters.reuse
            in
            (channel, Envelope.stage stage) :: stages)
        [] channels
    in
    { stages = List.rev stages }

  let of_channel t channel = List.assoc_opt channel t.stages
  let channels t = List.map fst t.stages

  let by_destination t map =
    let ( let* ) = Result.bind in
    let* stages =
      List.fold_left
        (fun stages (destination, channel) ->
          let* stages = stages in
          match of_channel t channel with
          | Some stage -> Ok ((Bus_uri.without_key destination, stage) :: stages)
          | None -> Error { destination; channel })
        (Ok []) map
    in
    Ok { By_destination.stages = List.rev stages }
end
