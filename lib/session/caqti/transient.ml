let sqlstate code =
  let class_ prefix = String.length code >= 2 && String.sub code 0 2 = prefix in
  match code with
  | "40001" | "40P01" | "40003" | "57P01" | "57P02" | "57P03" -> true
  | _ -> class_ "08" || class_ "53"

(* The PostgreSQL driver keeps the SQLSTATE in its own message; a communication
   error, the socket rather than a statement, carries none and is of the
   moment. Another driver's message has no SQLSTATE the port can read: a
   defect, which is the loud side to err on. *)
let of_msg : Caqti_error.msg -> bool = function
  | Caqti_driver_postgresql.Result_error_msg { sqlstate = code; _ } -> sqlstate code
  | Caqti_driver_postgresql.Connection_error_msg _ -> true
  | _ -> false

let of_error : Caqti_error.t -> bool = function
  | `Connect_rejected _ | `Connect_failed _ -> true
  | `Post_connect error -> (
      match error with
      | `Request_failed { msg; _ } | `Response_failed { msg; _ } -> of_msg msg
      | `Encode_rejected _ | `Encode_failed _ | `Decode_rejected _ | `Response_rejected _
        ->
          false)
  | `Request_failed { msg; _ } | `Response_failed { msg; _ } -> of_msg msg
  | `Load_rejected _ | `Load_failed _ | `Encode_rejected _ | `Encode_failed _
  | `Decode_rejected _ | `Response_rejected _ ->
      false

let driver_error error =
  {
    Ascetic_session.Driver_error.text = Caqti_error.show error;
    transient = of_error error;
  }

let protect ~raised f =
  match f () with
  | outcome -> outcome
  | exception Postgresql.Error error ->
      let text = Postgresql.string_of_error error in
      let reason =
        match error with
        | Postgresql.Connection_failure _ -> Ascetic_session.Driver_error.transient text
        | _ -> Ascetic_session.Driver_error.defect text
      in
      Error (raised reason)
