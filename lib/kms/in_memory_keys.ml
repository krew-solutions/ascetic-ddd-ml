module Make (Session : sig
  type t
end) =
struct
  type session = Session.t

  module Versions = Map.Make (Key_version)

  (* One tenant's key: the versions that serve, by version, and the last
     version made. The next rotation makes the one after it, so a version
     retired is never numbered again. *)
  type 'key ring = { versions : 'key Versions.t; made : Key_version.t }

  type t = {
    signing : (string * Signature_algorithm.t, Signing_key.t ring) Hashtbl.t;
    mac : (string * string, Mac_key.t ring) Hashtbl.t;
    managed : bool;
    mutex : Mutex.t;
  }

  let ( let* ) = Result.bind

  let create ?(managed_keys = false) () =
    {
      signing = Hashtbl.create 8;
      mac = Hashtbl.create 8;
      managed = managed_keys;
      mutex = Mutex.create ();
    }

  let empty = { versions = Versions.empty; made = Key_version.of_int_exn 0 }

  (* The key of a version, if that version serves. *)
  let version ring version =
    match Versions.find_opt version ring.versions with
    | Some key -> Ok key
    | None -> Error (Kms_error.No_key_of_version (Key_version.to_int version))

  (* The latest version's key. *)
  let latest ring =
    match Versions.max_binding_opt ring.versions with
    | Some (_, key) -> Ok key
    | None -> Error (Kms_error.No_key_of_version (Key_version.to_int ring.made))

  (* The ring with its next version, made by [make]; answers both. *)
  let push ring make =
    let* version = Key_version.next ring.made in
    let* key = make version in
    Ok ({ versions = Versions.add version key ring.versions; made = version }, version)

  (* The ring with the versions below [bound] retired: above the latest is
     [No_key_of_version] and nothing goes; at or below the oldest kept,
     nothing goes either. The latest always stays. *)
  let retire_before ring bound =
    if Key_version.compare bound ring.made > 0 then
      Error (Kms_error.No_key_of_version (Key_version.to_int bound))
    else
      Ok
        {
          ring with
          versions =
            Versions.filter
              (fun kept _ -> Key_version.compare kept bound >= 0)
              ring.versions;
        }

  let is_empty ring = Versions.is_empty ring.versions

  (* The tenant's ring under [id] in [table], empty when there is none. *)
  let ring table id = Option.value (Hashtbl.find_opt table id) ~default:empty

  (* What [f] answers of the tenant's ring that has a key, [missing]
     otherwise, with the ring it leaves behind kept. Under the lock. *)
  let with_ring t table id ~missing f =
    Mutex.protect t.mutex (fun () ->
        let ring = ring table id in
        if is_empty ring then Error missing
        else
          let* ring, value = f ring in
          Hashtbl.replace table id ring;
          Ok value)

  (* What [f] answers of the tenant's ring, made empty when there is none,
     with the ring it leaves behind kept. Under the lock. *)
  let with_ring_or_empty t table id f =
    Mutex.protect t.mutex (fun () ->
        let* ring, value = f (ring table id) in
        Hashtbl.replace table id ring;
        Ok value)

  let signing_key_not_found ~tenant_id algorithm =
    Kms_error.Signing_key_not_found { tenant_id; algorithm }

  let mac_key_not_found ~tenant_id purpose =
    Kms_error.Mac_key_not_found { tenant_id; purpose = Key_label.to_string purpose }

  (* A rotation period, which nothing here could keep. *)
  let no_scheduler (options : Key_options.t) =
    match options.auto_rotate_period with
    | Some _ ->
        Error
          (Kms_error.Unsupported
             "automatic rotation: the in-memory keys rotate on request only")
    | None -> Ok ()

  let generate_signing ~tenant_id algorithm version =
    Signing_key.generate ~tenant_id ~algorithm ~version

  let register t ~tenant_id ~seed =
    let algorithm = Signature_algorithm.Ed25519 in
    with_ring_or_empty t t.signing (tenant_id, algorithm) (fun ring ->
        push ring (fun version -> Signing_key.of_seed ~tenant_id ~algorithm ~version seed))

  (* ---------------------------------------------------------------------- *)
  (* Signing                                                                  *)

  let sign t _session ~tenant_id algorithm message =
    let id = (tenant_id, algorithm) in
    let signed ring =
      let* key = latest ring in
      Ok (ring, Signing_key.sign key message)
    in
    if t.managed then
      with_ring t t.signing id
        ~missing:(signing_key_not_found ~tenant_id algorithm)
        signed
    else
      with_ring_or_empty t t.signing id (fun ring ->
          let* ring =
            if is_empty ring then
              Result.map fst (push ring (generate_signing ~tenant_id algorithm))
            else Ok ring
          in
          signed ring)

  let sign_with_version t _session ~tenant_id algorithm key_version message =
    let* key_version = Key_version.from_one key_version in
    with_ring t t.signing (tenant_id, algorithm)
      ~missing:(signing_key_not_found ~tenant_id algorithm) (fun ring ->
        let* key = version ring key_version in
        Ok (ring, Signing_key.sign key message))

  let verify t _session ~tenant_id message signature =
    let algorithm = Signature.algorithm signature in
    let* key =
      with_ring t t.signing (tenant_id, algorithm)
        ~missing:(signing_key_not_found ~tenant_id algorithm) (fun ring ->
          let* key = version ring (Signature.key_version signature) in
          Ok (ring, key))
    in
    Public_key.verify (Signing_key.public key) message signature

  let create_signing_key t _session ~tenant_id algorithm options =
    let* () = no_scheduler options in
    with_ring_or_empty t t.signing (tenant_id, algorithm) (fun ring ->
        if is_empty ring then push ring (generate_signing ~tenant_id algorithm)
        else Ok (ring, ring.made))

  let rotate_signing_key t _session ~tenant_id algorithm =
    with_ring t t.signing (tenant_id, algorithm)
      ~missing:(signing_key_not_found ~tenant_id algorithm) (fun ring ->
        push ring (generate_signing ~tenant_id algorithm))

  let retire_signing_versions_before t _session ~tenant_id algorithm bound =
    with_ring t t.signing (tenant_id, algorithm)
      ~missing:(signing_key_not_found ~tenant_id algorithm) (fun ring ->
        let* ring = retire_before ring bound in
        Ok (ring, ()))

  let delete_signing_key t _session ~tenant_id algorithm =
    Mutex.protect t.mutex (fun () -> Hashtbl.remove t.signing (tenant_id, algorithm));
    Ok ()

  let public_keys t _session ~tenant_id algorithm =
    with_ring t t.signing (tenant_id, algorithm)
      ~missing:(signing_key_not_found ~tenant_id algorithm) (fun ring ->
        let* keyring =
          Public_keyring.make algorithm
            (List.map
               (fun (version, key) -> (version, Signing_key.public key))
               (Versions.bindings ring.versions))
        in
        Ok (ring, keyring))

  (* ---------------------------------------------------------------------- *)
  (* MACs                                                                     *)

  let generate_mac ~tenant_id purpose version =
    Mac_key.generate ~tenant_id ~purpose ~version
  let mac_id ~tenant_id purpose = (tenant_id, Key_label.to_string purpose)

  let mac t _session ~tenant_id purpose message =
    let id = mac_id ~tenant_id purpose in
    let made ring =
      let* key = latest ring in
      Ok (ring, Mac_key.mac key message)
    in
    if t.managed then
      with_ring t t.mac id ~missing:(mac_key_not_found ~tenant_id purpose) made
    else
      with_ring_or_empty t t.mac id (fun ring ->
          let* ring =
            if is_empty ring then
              Result.map fst (push ring (generate_mac ~tenant_id purpose))
            else Ok ring
          in
          made ring)

  let create_mac_key t _session ~tenant_id purpose options =
    let* () = no_scheduler options in
    with_ring_or_empty t t.mac (mac_id ~tenant_id purpose) (fun ring ->
        if is_empty ring then push ring (generate_mac ~tenant_id purpose)
        else Ok (ring, ring.made))

  let rotate_mac_key t _session ~tenant_id purpose =
    with_ring t t.mac (mac_id ~tenant_id purpose)
      ~missing:(mac_key_not_found ~tenant_id purpose) (fun ring ->
        push ring (generate_mac ~tenant_id purpose))

  let retire_mac_versions_before t _session ~tenant_id purpose bound =
    with_ring t t.mac (mac_id ~tenant_id purpose)
      ~missing:(mac_key_not_found ~tenant_id purpose) (fun ring ->
        let* ring = retire_before ring bound in
        Ok (ring, ()))

  let delete_mac_key t _session ~tenant_id purpose =
    Mutex.protect t.mutex (fun () -> Hashtbl.remove t.mac (mac_id ~tenant_id purpose));
    Ok ()
end
