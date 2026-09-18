module Schemes = Map.Make (String)

type t = Adapter.t Schemes.t

let empty = Schemes.empty

let register bus ~scheme adapter =
  if Schemes.mem scheme bus then Error (Bus_error.Already_registered scheme)
  else Ok (Schemes.add scheme adapter bus)

let adapter bus uri =
  Result.bind (Bus_uri.scheme uri) (fun scheme ->
      match Schemes.find_opt scheme bus with
      | Some adapter -> Ok adapter
      | None -> Error (Bus_error.Unknown_scheme scheme))

let ( let* ) = Result.bind

let consumer bus ~uri ~group ~decode =
  let* (adapter : Adapter.t) = adapter bus uri in
  let* wire = adapter.consumer ~uri ~group in
  Ok (Consumer.make ~uri ~group wire ~decode)

let producer bus ~uri ~encode =
  let* (adapter : Adapter.t) = adapter bus uri in
  let* wire = adapter.producer ~uri in
  Ok (Producer.make wire ~encode)
