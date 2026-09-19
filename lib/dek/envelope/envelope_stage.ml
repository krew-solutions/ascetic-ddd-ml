module Message = Ascetic_bus.Message
module Failure = Ascetic_bus.Failure
module Algorithm = Ascetic_kms.Algorithm
module Key = Ascetic_kms.Key
module Kms_error = Ascetic_kms.Kms_error
module Canonical = Ascetic_dek.Canonical

let tenant_id = "tenant_id"
let dek = "dek"
let dek_algorithm = "dek_algorithm"
let dek_bound_to = "dek_bound_to"
let message_id = "message_id"
let ( let* ) = Result.bind
let refused what = Error (Failure.permanent what)

(* A header as text; a message without it, or with bytes that are not text,
   is refused for good. *)
let header message name =
  match Message.header message name with
  | None -> refused (Printf.sprintf "the message has no `%s` header" name)
  | Some value when String.is_valid_utf_8 value -> Ok value
  | Some _ -> refused (Printf.sprintf "the `%s` header is not text" name)

(* What the payload is bound to: the canonical text of the named headers as
   name-value pairs, in order: [[["tenant_id","t1"],["message_id","..."]]]. *)
let associated_data message names =
  let* pairs =
    List.fold_right
      (fun name pairs ->
        let* pairs = pairs in
        let* value = header message name in
        Ok (`List [ `String name; `String value ] :: pairs))
      names (Ok [])
  in
  Ok (Canonical.to_string (`List pairs))

(* Which failures of the KMS no retry will mend: a key that is gone, a
   ciphertext that does not open, bytes that are not what they should be. The
   rest, the session, the database, Vault out of reach, are of the moment. *)
let verdict (error : Kms_error.t) =
  let text = Kms_error.to_string error in
  match error with
  | Kek_not_found _ | Decrypt | Wrong_key_version _ | No_key_of_version _
  | Unsupported_algorithm _ | Malformed _ ->
      Failure.permanent text
  | Entropy _ | Session _ | Database _ | Transport _ | Vault _ -> Failure.transient text

let of_kms result = Result.map_error verdict result

module Make
    (Sessions : Ascetic_session.Session_pool.S)
    (Kms : Ascetic_kms.Kms_port.S with type session = Sessions.session) =
struct
  (* A tenant's DEK in service at the sealing side. *)
  type serving = { key : Key.t; wrapped : string; since : Mtime.t; mutable used : int }

  type reusing = {
    reuse : Reuse.t;
    clock : Eio.Time.Mono.ty Eio.Resource.t;
    in_service : (string, serving) Hashtbl.t;
    mutex : Mutex.t;
        (** The sections under it touch memory and nothing else, so no fiber waits inside
            one; the lock is for stages shared by several domains. *)
  }

  type t = {
    sessions : Sessions.t;
    kms : Kms.t;
    algorithm : Algorithm.t;
    bound_to : string list;
    reusing : reusing option;
  }

  let create ?(algorithm = Algorithm.Aes_256_gcm) ?(bound_to = [ tenant_id; message_id ])
      sessions kms =
    { sessions; kms; algorithm; bound_to; reusing = None }

  let reusing t ~clock reuse =
    {
      t with
      reusing =
        Some
          {
            reuse;
            clock :> Eio.Time.Mono.ty Eio.Resource.t;
            in_service = Hashtbl.create 16;
            mutex = Mutex.create ();
          };
    }

  let in_kms_session t work =
    of_kms (Sessions.session t.sessions ~lift:(fun error -> Kms_error.Session error) work)

  (* The tenant's DEK in service, if one is and it may serve once more; counted
     as used. *)
  let serving t ~tenant =
    match t.reusing with
    | None -> None
    | Some r ->
        let now = Eio.Time.Mono.now r.clock in
        Mutex.protect r.mutex (fun () ->
            match Hashtbl.find_opt r.in_service tenant with
            | Some current
              when current.used < r.reuse.messages
                   && Mtime.Span.to_float_ns (Mtime.span current.since now) /. 1e9
                      < r.reuse.lifetime ->
                current.used <- current.used + 1;
                Some (current.key, current.wrapped)
            | Some _ | None -> None)

  (* Puts a fresh DEK in service for the tenant, used once. *)
  let serve t ~tenant (key, wrapped) =
    match t.reusing with
    | None -> ()
    | Some r ->
        let since = Eio.Time.Mono.now r.clock in
        Mutex.protect r.mutex (fun () ->
            Hashtbl.replace r.in_service tenant { key; wrapped; since; used = 1 })

  let outbound t message =
    let* tenant = header message tenant_id in
    let* aad = associated_data message t.bound_to in
    let* key, wrapped =
      match serving t ~tenant with
      | Some in_service -> Ok in_service
      | None ->
          let* fresh =
            in_kms_session t (fun session ->
                Kms.generate_dek t.kms session ~tenant_id:tenant)
          in
          serve t ~tenant fresh;
          Ok fresh
    in
    let* cipher = of_kms (Algorithm.cipher t.algorithm key ~aad) in
    let* sealed = of_kms (cipher.encrypt (Message.payload message)) in
    let with_header name value message = Message.with_header message name value in
    Ok
      (Message.with_payload message sealed
      |> with_header dek (Base64.encode_string wrapped)
      |> with_header dek_algorithm (Algorithm.to_string t.algorithm)
      |> with_header dek_bound_to (String.concat "," t.bound_to))

  let inbound t message =
    let* tenant = header message tenant_id in
    let* encoded = header message dek in
    let* wrapped =
      match Base64.decode encoded with
      | Ok wrapped -> Ok wrapped
      | Error (`Msg reason) ->
          refused (Printf.sprintf "the `%s` header is not base64: %s" dek reason)
    in
    let* algorithm =
      Result.bind (header message dek_algorithm) (fun name ->
          of_kms (Algorithm.of_string name))
    in
    let* bound_to = header message dek_bound_to in
    let* aad = associated_data message (String.split_on_char ',' bound_to) in
    let* key =
      in_kms_session t (fun session ->
          Kms.decrypt_dek t.kms session ~tenant_id:tenant wrapped)
    in
    let* cipher = of_kms (Algorithm.cipher algorithm key ~aad) in
    let* opened = of_kms (cipher.decrypt (Message.payload message)) in
    let without name message = Message.without_header message name in
    Ok
      (Message.with_payload message opened
      |> without dek |> without dek_algorithm |> without dek_bound_to)

  let stage t : Ascetic_bus.Stage.t = { outbound = outbound t; inbound = inbound t }
end
