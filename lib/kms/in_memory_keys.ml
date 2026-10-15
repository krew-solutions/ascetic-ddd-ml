module Make (Session : sig
  type t
end) =
struct
  type session = Session.t

  (* Keyrings by tenant and algorithm, and by tenant and purpose: the versions
     of each key, newest first. *)
  type t = {
    signing : (string * Signature_algorithm.t, Signing_key.t list) Hashtbl.t;
    mac : (string * string, Mac_key.t list) Hashtbl.t;
    mutex : Mutex.t;
  }

  let ( let* ) = Result.bind

  let create () =
    { signing = Hashtbl.create 8; mac = Hashtbl.create 8; mutex = Mutex.create () }

  let ring table id = Option.value (Hashtbl.find_opt table id) ~default:[]

  (* The version a ring of that length is at next. *)
  let next_version ring = Key_version.of_int (List.length ring + 1)

  (* The next version of the tenant's keyring of the algorithm, made by [make]
     for that version. *)
  let push t ~tenant_id algorithm make =
    Mutex.protect t.mutex (fun () ->
        let id = (tenant_id, algorithm) in
        let ring = ring t.signing id in
        let* version = next_version ring in
        let* key = make version in
        Hashtbl.replace t.signing id (key :: ring);
        Ok version)

  let register t ~tenant_id ~seed =
    push t ~tenant_id Signature_algorithm.Ed25519 (fun version ->
        Signing_key.of_seed ~tenant_id ~algorithm:Signature_algorithm.Ed25519 ~version
          seed)

  let sign t _session ~tenant_id algorithm message =
    Mutex.protect t.mutex (fun () ->
        let id = (tenant_id, algorithm) in
        let* key =
          match ring t.signing id with
          | key :: _ -> Ok key
          | [] ->
              let* key =
                Signing_key.generate ~tenant_id ~algorithm ~version:Key_version.first
              in
              Hashtbl.replace t.signing id [ key ];
              Ok key
        in
        Ok (Signing_key.sign key message))

  let verify t _session ~tenant_id message signature =
    let algorithm = Signature.algorithm signature in
    let version = Signature.key_version signature in
    let* key =
      Mutex.protect t.mutex (fun () ->
          match Hashtbl.find_opt t.signing (tenant_id, algorithm) with
          | None -> Error (Kms_error.Signing_key_not_found { tenant_id; algorithm })
          | Some ring -> (
              match
                List.find_opt
                  (fun key -> Key_version.equal (Signing_key.version key) version)
                  ring
              with
              | Some key -> Ok key
              | None -> Error (Kms_error.No_key_of_version (Key_version.to_int version))))
    in
    Public_key.verify (Signing_key.public key) message signature

  let rotate_signing_key t _session ~tenant_id algorithm =
    push t ~tenant_id algorithm (fun version ->
        Signing_key.generate ~tenant_id ~algorithm ~version)

  let delete_signing_key t _session ~tenant_id algorithm =
    Mutex.protect t.mutex (fun () -> Hashtbl.remove t.signing (tenant_id, algorithm));
    Ok ()

  let mac t _session ~tenant_id purpose message =
    Mutex.protect t.mutex (fun () ->
        let id = (tenant_id, Key_label.to_string purpose) in
        let* key =
          match ring t.mac id with
          | key :: _ -> Ok key
          | [] ->
              let* key =
                Mac_key.generate ~tenant_id ~purpose ~version:Key_version.first
              in
              Hashtbl.replace t.mac id [ key ];
              Ok key
        in
        Ok (Mac_key.mac key message))

  let delete_mac_key t _session ~tenant_id purpose =
    Mutex.protect t.mutex (fun () ->
        Hashtbl.remove t.mac (tenant_id, Key_label.to_string purpose));
    Ok ()
end
