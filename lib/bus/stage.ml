type t = {
  outbound : Message.t -> (Message.t, Failure.t) result;
  inbound : Message.t -> (Message.t, Failure.t) result;
}

let through stages message step =
  List.fold_left
    (fun message stage -> Result.bind message (step stage))
    (Ok message) stages

let outbound stages message = through stages message (fun stage -> stage.outbound)

let inbound stages message =
  through (List.rev stages) message (fun stage -> stage.inbound)
