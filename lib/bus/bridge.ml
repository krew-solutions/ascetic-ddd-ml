type target = Fixed of string | Header of string

type t = {
  bus : Bus.t;
  (* one producer per destination, made on first use *)
  producers : (string, Message.t Producer.t) Hashtbl.t;
}

let create bus = { bus; producers = Hashtbl.create 8 }

let producer t uri =
  match Hashtbl.find_opt t.producers uri with
  | Some producer -> Ok producer
  | None ->
      Result.map
        (fun producer ->
          Hashtbl.replace t.producers uri producer;
          producer)
        (Bus.producer t.bus ~uri ~encode:Fun.id)

let forward t target message =
  let refused error = Failure.transient (Bus_error.to_string error) in
  let uri =
    match target with
    | Fixed uri -> Ok uri
    | Header name -> (
        match Message.header message name with
        | Some uri -> Ok uri
        | None ->
            Error
              (Failure.transient
                 (Printf.sprintf "no `%s` header names a destination" name)))
  in
  Result.bind uri (fun uri ->
      Result.bind
        (Result.map_error refused (producer t uri))
        (fun producer -> Result.map_error refused (Producer.publish producer message)))

let run t ~from ~group target =
  Result.bind
    (Bus.consumer t.bus ~uri:from ~group ~decode:(fun message -> Ok message))
    (fun consumer -> Consumer.subscribe consumer (forward t target))
