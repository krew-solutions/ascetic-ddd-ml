module Session = Ascetic_session_caqti.Caqti_session
module Identifier = Ascetic_session_caqti.Identifier
module Transient = Ascetic_session_caqti.Transient
module Error = Ascetic_kms.Kms_error
module Algorithm = Ascetic_kms.Algorithm
module Key = Ascetic_kms.Key
module Key_version = Ascetic_kms.Key_version
module Wrapped_key = Ascetic_kms.Wrapped_key
module Master_key = Ascetic_kms.Master_key
module Kek = Ascetic_kms.Kek
module Kek_options = Ascetic_kms.Kek_options
module Key_options = Ascetic_kms.Key_options
module Public_keyring = Ascetic_kms.Public_keyring
module Signature_algorithm = Ascetic_kms.Signature_algorithm
module Signature = Ascetic_kms.Signature
module Public_key = Ascetic_kms.Public_key
module Signing_key = Ascetic_kms.Signing_key
module Key_label = Ascetic_kms.Key_label
module Mac_key = Ascetic_kms.Mac_key

type session = Session.t

(* ------------------------------------------------------------------------ *)
(* Errors                                                                    *)

(* A value that could not be encoded or decoded is a defect of the stored
   data; anything else the driver reports is the database's, with the
   driver's verdict on whether it is of the moment. *)
let of_caqti (error : Caqti_error.t) =
  match error with
  | `Encode_rejected _ | `Encode_failed _ | `Decode_rejected _ ->
      Error.Malformed (Caqti_error.show error)
  | _ -> Error.Database (Transient.driver_error error)

(* A driver call returns its error, or raises one of the client library's on
   a connection whose server is gone: either way the adapter gets an error. *)
let caqti (call : unit -> ('a, [< Caqti_error.t ]) result) =
  Transient.protect
    ~raised:(fun reason -> Error.Database reason)
    (fun () ->
      Result.map_error (fun error -> of_caqti (error :> Caqti_error.t)) (call ()))

let session_error error = Error.Session error
let ( let* ) = Result.bind

(* ------------------------------------------------------------------------ *)
(* The statements                                                             *)

(* Built once per service: a request is prepared by the driver and cached per
   connection under its own identity. *)
module Requests = struct
  open Caqti_request.Infix
  open Caqti_type

  type kek_row = int * string * string * string
  (* key_version, encrypted_key, master_algorithm, key_algorithm *)

  type signing_row = int * string * string * string
  (* key_version, encrypted_key, public_key, master_algorithm *)

  type mac_row = int * string * string
  (* key_version, encrypted_key, master_algorithm *)

  type t = {
    lock_table : (string, int, [ `One ]) Caqti_request.t;
    ddl : string list;
    lock_name : (string * string, int, [ `One ]) Caqti_request.t;
    current : (string, kek_row, [ `One | `Zero ]) Caqti_request.t;
    by_version : (string * int, kek_row, [ `One | `Zero ]) Caqti_request.t;
    insert : (string * int * string * string * string, unit, [ `Zero ]) Caqti_request.t;
    delete : (string, unit, [ `Zero ]) Caqti_request.t;
    retire : (string * int, unit, [ `Zero ]) Caqti_request.t;
    latest : (string, int option, [ `One ]) Caqti_request.t;
    current_signing : (string * string, signing_row, [ `One | `Zero ]) Caqti_request.t;
    signing_by_version :
      (string * string * int, signing_row, [ `One | `Zero ]) Caqti_request.t;
    all_signing :
      (string * string, signing_row, [ `Many | `One | `Zero ]) Caqti_request.t;
    any_signing : (string * string, int, [ `One | `Zero ]) Caqti_request.t;
    latest_signing : (string * string, int option, [ `One ]) Caqti_request.t;
    insert_signing :
      (string * string * int * string * string * string, unit, [ `Zero ]) Caqti_request.t;
    retire_signing : (string * string * int, unit, [ `Zero ]) Caqti_request.t;
    delete_signing : (string * string, unit, [ `Zero ]) Caqti_request.t;
    current_mac : (string * string, mac_row, [ `One | `Zero ]) Caqti_request.t;
    latest_mac : (string * string, int option, [ `One ]) Caqti_request.t;
    insert_mac :
      (string * string * int * string * string, unit, [ `Zero ]) Caqti_request.t;
    retire_mac : (string * string * int, unit, [ `Zero ]) Caqti_request.t;
    delete_mac : (string * string, unit, [ `Zero ]) Caqti_request.t;
  }

  let make ~table ~signing_table ~mac_table =
    let table = Identifier.to_string table in
    let signing = Identifier.to_string signing_table in
    let mac = Identifier.to_string mac_table in
    let sprintf = Printf.sprintf in
    let kek_row = t4 int octets string string in
    let signing_row = t4 int octets octets string in
    let mac_row = t3 int octets string in
    {
      lock_table =
        (string ->! int) "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1))) AS l";
      ddl =
        [
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  \"tenant_id\" VARCHAR(128) NOT NULL,\n\
            \  \"key_version\" INTEGER NOT NULL,\n\
            \  \"encrypted_key\" BYTEA NOT NULL,\n\
            \  \"master_algorithm\" VARCHAR(32) NOT NULL,\n\
            \  \"key_algorithm\" VARCHAR(32) NOT NULL,\n\
            \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
            \  CONSTRAINT %s_pk PRIMARY KEY (\"tenant_id\", \"key_version\")\n\
             )"
            table table;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  \"tenant_id\" VARCHAR(128) NOT NULL,\n\
            \  \"algorithm\" VARCHAR(32) NOT NULL,\n\
            \  \"key_version\" INTEGER NOT NULL,\n\
            \  \"encrypted_key\" BYTEA NOT NULL,\n\
            \  \"public_key\" BYTEA NOT NULL,\n\
            \  \"master_algorithm\" VARCHAR(32) NOT NULL,\n\
            \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
            \  CONSTRAINT %s_pk PRIMARY KEY (\"tenant_id\", \"algorithm\", \
             \"key_version\")\n\
             )"
            signing signing;
          sprintf
            "CREATE TABLE IF NOT EXISTS %s (\n\
            \  \"tenant_id\" VARCHAR(128) NOT NULL,\n\
            \  \"purpose\" VARCHAR(64) NOT NULL,\n\
            \  \"key_version\" INTEGER NOT NULL,\n\
            \  \"encrypted_key\" BYTEA NOT NULL,\n\
            \  \"master_algorithm\" VARCHAR(32) NOT NULL,\n\
            \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
            \  CONSTRAINT %s_pk PRIMARY KEY (\"tenant_id\", \"purpose\", \"key_version\")\n\
             )"
            mac mac;
        ];
      lock_name =
        (t2 string string ->! int)
          "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1), hashtext($2))) AS l";
      current =
        (string ->? kek_row)
          (sprintf
             "SELECT key_version, encrypted_key, master_algorithm, key_algorithm FROM %s \
              WHERE tenant_id = $1 ORDER BY key_version DESC LIMIT 1"
             table);
      by_version =
        (t2 string int ->? kek_row)
          (sprintf
             "SELECT key_version, encrypted_key, master_algorithm, key_algorithm FROM %s \
              WHERE tenant_id = $1 AND key_version = $2"
             table);
      insert =
        (t5 string int octets string string ->. unit)
          (sprintf
             "INSERT INTO %s (tenant_id, key_version, encrypted_key, master_algorithm, \
              key_algorithm) VALUES ($1, $2, $3, $4, $5)"
             table);
      delete = (string ->. unit) (sprintf "DELETE FROM %s WHERE tenant_id = $1" table);
      retire =
        (t2 string int ->. unit)
          (sprintf "DELETE FROM %s WHERE tenant_id = $1 AND key_version < $2" table);
      latest =
        (string ->! option int)
          (sprintf "SELECT max(key_version) FROM %s WHERE tenant_id = $1" table);
      current_signing =
        (t2 string string ->? signing_row)
          (sprintf
             "SELECT key_version, encrypted_key, public_key, master_algorithm FROM %s \
              WHERE tenant_id = $1 AND algorithm = $2 ORDER BY key_version DESC LIMIT 1"
             signing);
      signing_by_version =
        (t3 string string int ->? signing_row)
          (sprintf
             "SELECT key_version, encrypted_key, public_key, master_algorithm FROM %s \
              WHERE tenant_id = $1 AND algorithm = $2 AND key_version = $3"
             signing);
      all_signing =
        (t2 string string ->* signing_row)
          (sprintf
             "SELECT key_version, encrypted_key, public_key, master_algorithm FROM %s \
              WHERE tenant_id = $1 AND algorithm = $2 ORDER BY key_version"
             signing);
      any_signing =
        (t2 string string ->? int)
          (sprintf "SELECT 1 FROM %s WHERE tenant_id = $1 AND algorithm = $2 LIMIT 1"
             signing);
      latest_signing =
        (t2 string string ->! option int)
          (sprintf
             "SELECT max(key_version) FROM %s WHERE tenant_id = $1 AND algorithm = $2"
             signing);
      insert_signing =
        (t6 string string int octets octets string ->. unit)
          (sprintf
             "INSERT INTO %s (tenant_id, algorithm, key_version, encrypted_key, \
              public_key, master_algorithm) VALUES ($1, $2, $3, $4, $5, $6)"
             signing);
      retire_signing =
        (t3 string string int ->. unit)
          (sprintf
             "DELETE FROM %s WHERE tenant_id = $1 AND algorithm = $2 AND key_version < $3"
             signing);
      delete_signing =
        (t2 string string ->. unit)
          (sprintf "DELETE FROM %s WHERE tenant_id = $1 AND algorithm = $2" signing);
      current_mac =
        (t2 string string ->? mac_row)
          (sprintf
             "SELECT key_version, encrypted_key, master_algorithm FROM %s WHERE \
              tenant_id = $1 AND purpose = $2 ORDER BY key_version DESC LIMIT 1"
             mac);
      latest_mac =
        (t2 string string ->! option int)
          (sprintf "SELECT max(key_version) FROM %s WHERE tenant_id = $1 AND purpose = $2"
             mac);
      insert_mac =
        (t5 string string int octets string ->. unit)
          (sprintf
             "INSERT INTO %s (tenant_id, purpose, key_version, encrypted_key, \
              master_algorithm) VALUES ($1, $2, $3, $4, $5)"
             mac);
      retire_mac =
        (t3 string string int ->. unit)
          (sprintf
             "DELETE FROM %s WHERE tenant_id = $1 AND purpose = $2 AND key_version < $3"
             mac);
      delete_mac =
        (t2 string string ->. unit)
          (sprintf "DELETE FROM %s WHERE tenant_id = $1 AND purpose = $2" mac);
    }
end

type t = {
  master_key : Key.t;
  master_algorithm : Algorithm.t;
  table : Identifier.t;
  signing_table : Identifier.t;
  mac_table : Identifier.t;
  context : string option;
  managed_keys : bool;
  requests : Requests.t;
}

let default_table = Identifier.of_string_exn "kms_keys"
let default_signing_table = Identifier.of_string_exn "kms_signing_keys"
let default_mac_table = Identifier.of_string_exn "kms_mac_keys"

let create
    ?(master_algorithm = Algorithm.Aes_256_gcm)
    ?(table = default_table)
    ?(signing_table = default_signing_table)
    ?(mac_table = default_mac_table)
    ?derivation_context
    ?(managed_keys = false)
    master_key =
  {
    master_key;
    master_algorithm;
    table;
    signing_table;
    mac_table;
    context = derivation_context;
    managed_keys;
    requests = Requests.make ~table ~signing_table ~mac_table;
  }

let table t = t.table
let signing_table t = t.signing_table
let mac_table t = t.mac_table
let derivation_context t = t.context

let setup t session =
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val Session.connection tx) in
      let* () =
        List.fold_left
          (fun done_ table ->
            let* () = done_ in
            let* _ =
              caqti (fun () -> C.find t.requests.lock_table (Identifier.to_string table))
            in
            Ok ())
          (Ok ())
          [ t.table; t.signing_table; t.mac_table ]
      in
      let open Caqti_request.Infix in
      List.fold_left
        (fun done_ ddl ->
          let* () = done_ in
          caqti (fun () ->
              C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true ddl) ()))
        (Ok ()) t.requests.ddl)

(* ------------------------------------------------------------------------ *)
(* Rows                                                                       *)

(* The column is INTEGER; a version is a count. *)
let max_stored = 0x7FFF_FFFF

let stored_version version =
  let version = Key_version.to_int version in
  if version <= max_stored then Ok version
  else
    Error
      (Error.Malformed (Printf.sprintf "key version %d does not fit the column" version))

let read_version stored =
  if stored < 0 then
    Error (Error.Malformed (Printf.sprintf "key version %d is not a count" stored))
  else Key_version.of_int stored

(* The master key for the algorithm: the configured one for what is made now,
   the row's for what is read back. *)
let master_key t algorithm = Master_key.make t.master_key algorithm

(* A KEK from its row, unwrapped by the master key under the algorithm the
   row names, serving this instance's context. *)
let load_kek
    t
    ~tenant_id
    ((version, encrypted_key, master_algorithm, key_algorithm) : Requests.kek_row) =
  let* version = read_version version in
  let* wrapped = Wrapped_key.parse encrypted_key in
  let* master_algorithm = Algorithm.of_string master_algorithm in
  let* algorithm = Algorithm.of_string key_algorithm in
  let* master = master_key t master_algorithm in
  Master_key.load_kek ?context:t.context master ~tenant_id ~version ~algorithm wrapped

(* The advisory lock of one keyring in the table, for the transaction: what
   makes a first key or a rotation holds it, so that two meeting a new keyring
   make one key, and two rotations make the next two versions. *)
let lock t session ~table ~name =
  let module C = (val Session.connection session) in
  let* _ =
    caqti (fun () -> C.find t.requests.lock_name (Identifier.to_string table, name))
  in
  Ok ()

(* Serializes the making and rotating of one tenant's KEKs until the end of
   the transaction. *)
let lock_tenant t session ~tenant_id = lock t session ~table:t.table ~name:tenant_id

(* ------------------------------------------------------------------------ *)
(* KEKs                                                                       *)

(* The tenant's KEK of the highest version, if it has one. *)
let current_kek t session ~tenant_id =
  let module C = (val Session.connection session) in
  let* row = caqti (fun () -> C.find_opt t.requests.current tenant_id) in
  match row with
  | Some row -> Result.map Option.some (load_kek t ~tenant_id row)
  | None -> Ok None

(* The tenant's KEK of the version. *)
let kek t session ~tenant_id version =
  let module C = (val Session.connection session) in
  let* stored = stored_version version in
  let* row = caqti (fun () -> C.find_opt t.requests.by_version (tenant_id, stored)) in
  match row with
  | Some row -> load_kek t ~tenant_id row
  | None ->
      Error
        (Error.Kek_not_found
           { tenant_id; key_version = Some (Key_version.to_int version) })

let save_kek t session kek =
  let module C = (val Session.connection session) in
  let* stored = stored_version (Kek.version kek) in
  caqti (fun () ->
      C.exec t.requests.insert
        ( Kek.tenant_id kek,
          stored,
          Wrapped_key.to_bytes (Kek.wrapped kek),
          Algorithm.to_string t.master_algorithm,
          Algorithm.to_string (Kek.algorithm kek) ))

(* Makes the tenant's first KEK, serving this instance's context, under the
   tenant's lock, which the caller holds. *)
let make_first_kek t tx ~tenant_id =
  let* master = master_key t t.master_algorithm in
  let* kek = Master_key.generate_kek ?context:t.context master ~tenant_id in
  let* () = save_kek t tx kek in
  Ok kek

(* The tenant's current KEK, made if there is none, unless the keys are
   managed elsewhere, when none is [Kek_not_found]: in a scope of its own,
   under the tenant's lock, and read again after taking it, in case another
   transaction made it meanwhile. The scope is what makes the lock hold over
   the read and the insert when the caller has no transaction open. *)
let get_or_create_current_kek t session ~tenant_id =
  let* found = current_kek t session ~tenant_id in
  match found with
  | Some kek -> Ok kek
  | None when t.managed_keys ->
      Error (Error.Kek_not_found { tenant_id; key_version = None })
  | None ->
      Session.atomic session ~lift:session_error (fun tx ->
          let* () = lock_tenant t tx ~tenant_id in
          let* found = current_kek t tx ~tenant_id in
          match found with
          | Some kek -> Ok kek
          | None -> make_first_kek t tx ~tenant_id)

(* ------------------------------------------------------------------------ *)
(* The KEK port                                                               *)

let encrypt_dek t session ~tenant_id dek =
  let* kek = get_or_create_current_kek t session ~tenant_id in
  Result.map Wrapped_key.to_bytes (Kek.wrap kek dek)

let decrypt_dek t session ~tenant_id encrypted_dek =
  let* wrapped = Wrapped_key.parse encrypted_dek in
  let* kek = kek t session ~tenant_id (Wrapped_key.key_version wrapped) in
  Kek.unwrap kek wrapped

let generate_dek t session ~tenant_id =
  let* kek = get_or_create_current_kek t session ~tenant_id in
  let* dek, wrapped = Kek.generate_dek kek in
  Ok (dek, Wrapped_key.to_bytes wrapped)

let rewrap_dek t session ~tenant_id encrypted_dek =
  let* wrapped = Wrapped_key.parse encrypted_dek in
  let* from = kek t session ~tenant_id (Wrapped_key.key_version wrapped) in
  let* current = get_or_create_current_kek t session ~tenant_id in
  Result.map Wrapped_key.to_bytes (Kek.rewrap current ~from wrapped)

(* ------------------------------------------------------------------------ *)
(* Custody: what every kind shares                                            *)

let signing_key_not_found ~tenant_id algorithm =
  Error.Signing_key_not_found { tenant_id; algorithm }

let mac_key_not_found ~tenant_id purpose =
  Error.Mac_key_not_found { tenant_id; purpose = Key_label.to_string purpose }

(* A rotation period, which a table cannot keep. *)
let no_scheduler auto_rotate_period =
  match auto_rotate_period with
  | Some _ ->
      Error
        (Error.Unsupported
           "automatic rotation: the PostgreSQL KMS rotates on request only")
  | None -> Ok ()

(* The latest version among the rows [request] selects, [None] when there
   are none: what custody reads under the keyring's lock before it makes,
   rotates or retires. *)
let latest_version request =
  let* stored = caqti request in
  match stored with
  | Some stored -> Result.map Option.some (read_version stored)
  | None -> Ok None

(* Retires the versions below [bound] of a keyring whose latest is [latest]
   by [delete], the rows below the bound: a bound above the latest is
   [No_key_of_version] and nothing is deleted, not the whole key; one at or
   below the oldest kept deletes nothing, the deletion being monotonic by
   nature. *)
let retire_below ~latest ~bound delete =
  if Key_version.compare bound latest > 0 then
    Error (Error.No_key_of_version (Key_version.to_int bound))
  else
    let* stored = stored_version bound in
    delete stored

(* ------------------------------------------------------------------------ *)
(* Custody of the KEK                                                         *)

(* Makes the tenant's first KEK under the tenant's lock, or answers the
   version of the one there is. [derived] has nothing to enforce here, this
   adapter has no policy to pin a context by and the binding to a context is
   the wrap's, so a key that exists is taken as made with the options asked.
   Automatic rotation is refused: there is no scheduler in a table. *)
let create_kek t session ~tenant_id (options : Kek_options.t) =
  let* () = no_scheduler options.auto_rotate_period in
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_tenant t tx ~tenant_id in
      let* current = current_kek t tx ~tenant_id in
      match current with
      | Some current -> Ok (Kek.version current)
      | None -> Result.map Kek.version (make_first_kek t tx ~tenant_id))

(* The next version under the tenant's lock; a tenant without a KEK is
   [Kek_not_found]: a rotation makes no first version. *)
let rotate_kek t session ~tenant_id =
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_tenant t tx ~tenant_id in
      let* current = current_kek t tx ~tenant_id in
      match current with
      | None -> Error (Error.Kek_not_found { tenant_id; key_version = None })
      | Some current ->
          let* master = master_key t t.master_algorithm in
          let* kek = Master_key.rotate_kek master current in
          let* () = save_kek t tx kek in
          Ok (Kek.version kek))

(* Deletes the rows of the versions below the version, under the tenant's
   lock, after reading the latest: what a retired version wrapped is
   [Kek_not_found] of that version from then on. *)
let retire_kek_versions_before t session ~tenant_id bound =
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val Session.connection tx) in
      let* () = lock_tenant t tx ~tenant_id in
      let* latest = latest_version (fun () -> C.find t.requests.latest tenant_id) in
      match latest with
      | None -> Error (Error.Kek_not_found { tenant_id; key_version = None })
      | Some latest ->
          retire_below ~latest ~bound (fun stored ->
              caqti (fun () -> C.exec t.requests.retire (tenant_id, stored))))

let delete_kek t session ~tenant_id =
  let module C = (val Session.connection session) in
  caqti (fun () -> C.exec t.requests.delete tenant_id)

(* ------------------------------------------------------------------------ *)
(* Signing keys                                                               *)

(* The key from its row: the private half unwrapped under the key's name,
   the public half derived from it rather than read; and the stored public
   half, for the check. *)
let load_signing_key
    t
    ~tenant_id
    ~algorithm
    ((version, encrypted_key, public_key, master_algorithm) : Requests.signing_row) =
  let* version = read_version version in
  let* wrapped = Wrapped_key.parse encrypted_key in
  let* master_algorithm = Algorithm.of_string master_algorithm in
  let* master = master_key t master_algorithm in
  let* private_key =
    Master_key.unwrap master ~tenant_id:(Signing_key.name_of ~tenant_id algorithm) wrapped
  in
  let* key = Signing_key.load ~tenant_id ~algorithm ~version private_key in
  Ok (key, public_key)

(* The key of a row with its public half derived, and the stored public half
   checked against it: a writer of the table without the master key can
   change the column, never the wrapped private half, so a mismatch is a
   tamper, refused rather than published or verified under (ADR-0035). *)
let checked_signing_key t ~tenant_id ~algorithm row =
  let* key, stored = load_signing_key t ~tenant_id ~algorithm row in
  if String.equal stored (Public_key.to_string (Signing_key.public key)) then Ok key
  else
    Error
      (Error.Malformed
         (Format.asprintf "the stored public half of `%s` v%a is not its private half's"
            (Signing_key.name key) Key_version.pp (Signing_key.version key)))

(* The latest signing key of the tenant and algorithm, if any. *)
let current_signing_key t session ~tenant_id algorithm =
  let module C = (val Session.connection session) in
  let* row =
    caqti (fun () ->
        C.find_opt t.requests.current_signing
          (tenant_id, Signature_algorithm.to_string algorithm))
  in
  match row with
  | Some row ->
      Result.map (fun (key, _) -> Some key) (load_signing_key t ~tenant_id ~algorithm row)
  | None -> Ok None

(* That version of the tenant's signing key of the algorithm, its public
   half derived and checked: what signs at a named version and what
   verifies. Nothing is made. [Signing_key_not_found] when the tenant has no
   key of the algorithm, [No_key_of_version] when it has the key and not
   that version. *)
let signing_key_of_version t session ~tenant_id algorithm version =
  let module C = (val Session.connection session) in
  let name = Signature_algorithm.to_string algorithm in
  let* stored = stored_version version in
  let* row =
    caqti (fun () -> C.find_opt t.requests.signing_by_version (tenant_id, name, stored))
  in
  match row with
  | Some row -> checked_signing_key t ~tenant_id ~algorithm row
  | None -> (
      (* No such version: the tenant may still have the key, at others. *)
      let* any = caqti (fun () -> C.find_opt t.requests.any_signing (tenant_id, name)) in
      match any with
      | Some _ -> Error (Error.No_key_of_version (Key_version.to_int version))
      | None -> Error (signing_key_not_found ~tenant_id algorithm))

(* Saves the key: the private half wrapped by the master key under the key's
   name, the public half beside it. *)
let save_signing_key t session key =
  let module C = (val Session.connection session) in
  let* master = master_key t t.master_algorithm in
  let* wrapped =
    Master_key.wrap master ~tenant_id:(Signing_key.name key) (Signing_key.private_key key)
  in
  let* stored = stored_version (Signing_key.version key) in
  caqti (fun () ->
      C.exec t.requests.insert_signing
        ( Signing_key.tenant_id key,
          Signature_algorithm.to_string (Signing_key.algorithm key),
          stored,
          Wrapped_key.to_bytes wrapped,
          Public_key.to_string (Signing_key.public key),
          Algorithm.to_string t.master_algorithm ))

(* The signing keyring's lock, for the transaction. *)
let lock_signing t session ~tenant_id algorithm =
  lock t session ~table:t.signing_table ~name:(Signing_key.name_of ~tenant_id algorithm)

let latest_signing_version t session ~tenant_id algorithm =
  let module C = (val Session.connection session) in
  latest_version (fun () ->
      C.find t.requests.latest_signing (tenant_id, Signature_algorithm.to_string algorithm))

(* The latest signing key, made under the keyring's lock if the tenant has
   none of the algorithm, unless the keys are managed elsewhere, when none
   is [Signing_key_not_found]. *)
let get_or_create_signing_key t session ~tenant_id algorithm =
  let* found = current_signing_key t session ~tenant_id algorithm in
  match found with
  | Some key -> Ok key
  | None when t.managed_keys -> Error (signing_key_not_found ~tenant_id algorithm)
  | None ->
      Session.atomic session ~lift:session_error (fun tx ->
          let* () = lock_signing t tx ~tenant_id algorithm in
          let* found = current_signing_key t tx ~tenant_id algorithm in
          match found with
          | Some key -> Ok key
          | None ->
              let* key =
                Signing_key.generate ~tenant_id ~algorithm ~version:Key_version.first
              in
              let* () = save_signing_key t tx key in
              Ok key)

(* ---- The signing port ---- *)

let sign t session ~tenant_id algorithm message =
  let* key = get_or_create_signing_key t session ~tenant_id algorithm in
  Ok (Signing_key.sign key message)

let sign_with_version t session ~tenant_id algorithm key_version message =
  let* key_version = Key_version.from_one key_version in
  let* key = signing_key_of_version t session ~tenant_id algorithm key_version in
  Ok (Signing_key.sign key message)

let verify t session ~tenant_id message signature =
  let* key =
    signing_key_of_version t session ~tenant_id
      (Signature.algorithm signature)
      (Signature.key_version signature)
  in
  Public_key.verify (Signing_key.public key) message signature

(* ---- Custody of the signing keys: every write under the keyring's lock ---- *)

(* Version 1 when the tenant has no key of the algorithm, the latest version
   otherwise; a rotation period is refused: there is no scheduler in a
   table. *)
let create_signing_key t session ~tenant_id algorithm (options : Key_options.t) =
  let* () = no_scheduler options.auto_rotate_period in
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_signing t tx ~tenant_id algorithm in
      let* latest = latest_signing_version t tx ~tenant_id algorithm in
      match latest with
      | Some latest -> Ok latest
      | None ->
          let* key =
            Signing_key.generate ~tenant_id ~algorithm ~version:Key_version.first
          in
          let* () = save_signing_key t tx key in
          Ok Key_version.first)

let rotate_signing_key t session ~tenant_id algorithm =
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_signing t tx ~tenant_id algorithm in
      let* latest = latest_signing_version t tx ~tenant_id algorithm in
      match latest with
      | None -> Error (signing_key_not_found ~tenant_id algorithm)
      | Some latest ->
          let* version = Key_version.next latest in
          let* key = Signing_key.generate ~tenant_id ~algorithm ~version in
          let* () = save_signing_key t tx key in
          Ok version)

(* Deletes the rows of the versions below the version, after reading the
   latest: a bound above it retires nothing. *)
let retire_signing_versions_before t session ~tenant_id algorithm bound =
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val Session.connection tx) in
      let* () = lock_signing t tx ~tenant_id algorithm in
      let* latest = latest_signing_version t tx ~tenant_id algorithm in
      match latest with
      | None -> Error (signing_key_not_found ~tenant_id algorithm)
      | Some latest ->
          retire_below ~latest ~bound (fun stored ->
              caqti (fun () ->
                  C.exec t.requests.retire_signing
                    (tenant_id, Signature_algorithm.to_string algorithm, stored))))

let delete_signing_key t session ~tenant_id algorithm =
  let module C = (val Session.connection session) in
  caqti (fun () ->
      C.exec t.requests.delete_signing (tenant_id, Signature_algorithm.to_string algorithm))

(* ---- The public keys ---- *)

(* The public halves of the keyring's versions there are, those an operator
   has not deleted, each derived from its wrapped private half and checked
   against the stored one, so that what is published is what the master
   key's holder made (ADR-0035). *)
let public_keys t session ~tenant_id algorithm =
  let module C = (val Session.connection session) in
  let* rows =
    caqti (fun () ->
        C.collect_list t.requests.all_signing
          (tenant_id, Signature_algorithm.to_string algorithm))
  in
  match rows with
  | [] -> Error (signing_key_not_found ~tenant_id algorithm)
  | rows ->
      let* keys =
        List.fold_right
          (fun row keys ->
            let* keys = keys in
            let* key = checked_signing_key t ~tenant_id ~algorithm row in
            Ok ((Signing_key.version key, Signing_key.public key) :: keys))
          rows (Ok [])
      in
      Public_keyring.make algorithm keys

(* ------------------------------------------------------------------------ *)
(* MAC keys                                                                   *)

(* The latest MAC key of the tenant and purpose, if any, unwrapped under the
   key's name. *)
let current_mac_key t session ~tenant_id purpose =
  let module C = (val Session.connection session) in
  let* row =
    caqti (fun () ->
        C.find_opt t.requests.current_mac (tenant_id, Key_label.to_string purpose))
  in
  match row with
  | None -> Ok None
  | Some (version, encrypted_key, master_algorithm) ->
      let* version = read_version version in
      let* wrapped = Wrapped_key.parse encrypted_key in
      let* master_algorithm = Algorithm.of_string master_algorithm in
      let* master = master_key t master_algorithm in
      let* key =
        Master_key.unwrap master ~tenant_id:(Mac_key.name_of ~tenant_id purpose) wrapped
      in
      Ok (Some (Mac_key.load ~tenant_id ~purpose ~version key))

(* Saves the MAC key, wrapped by the master key under the key's name. *)
let save_mac_key t session key =
  let module C = (val Session.connection session) in
  let* master = master_key t t.master_algorithm in
  let* wrapped = Master_key.wrap master ~tenant_id:(Mac_key.name key) (Mac_key.key key) in
  let* stored = stored_version (Mac_key.version key) in
  caqti (fun () ->
      C.exec t.requests.insert_mac
        ( Mac_key.tenant_id key,
          Key_label.to_string (Mac_key.purpose key),
          stored,
          Wrapped_key.to_bytes wrapped,
          Algorithm.to_string t.master_algorithm ))

(* The MAC keyring's lock, for the transaction. *)
let lock_mac t session ~tenant_id purpose =
  lock t session ~table:t.mac_table ~name:(Mac_key.name_of ~tenant_id purpose)

let latest_mac_version t session ~tenant_id purpose =
  let module C = (val Session.connection session) in
  latest_version (fun () ->
      C.find t.requests.latest_mac (tenant_id, Key_label.to_string purpose))

(* The latest MAC key, made under the keyring's lock if the tenant has none
   for the purpose, unless the keys are managed elsewhere, when none is
   [Mac_key_not_found]. *)
let get_or_create_mac_key t session ~tenant_id purpose =
  let* found = current_mac_key t session ~tenant_id purpose in
  match found with
  | Some key -> Ok key
  | None when t.managed_keys -> Error (mac_key_not_found ~tenant_id purpose)
  | None ->
      Session.atomic session ~lift:session_error (fun tx ->
          let* () = lock_mac t tx ~tenant_id purpose in
          let* found = current_mac_key t tx ~tenant_id purpose in
          match found with
          | Some key -> Ok key
          | None ->
              let* key =
                Mac_key.generate ~tenant_id ~purpose ~version:Key_version.first
              in
              let* () = save_mac_key t tx key in
              Ok key)

let mac t session ~tenant_id purpose message =
  let* key = get_or_create_mac_key t session ~tenant_id purpose in
  Ok (Mac_key.mac key message)

(* ---- Custody of the MAC keys: every write under the keyring's lock ---- *)

let create_mac_key t session ~tenant_id purpose (options : Key_options.t) =
  let* () = no_scheduler options.auto_rotate_period in
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_mac t tx ~tenant_id purpose in
      let* latest = latest_mac_version t tx ~tenant_id purpose in
      match latest with
      | Some latest -> Ok latest
      | None ->
          let* key = Mac_key.generate ~tenant_id ~purpose ~version:Key_version.first in
          let* () = save_mac_key t tx key in
          Ok Key_version.first)

let rotate_mac_key t session ~tenant_id purpose =
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_mac t tx ~tenant_id purpose in
      let* latest = latest_mac_version t tx ~tenant_id purpose in
      match latest with
      | None -> Error (mac_key_not_found ~tenant_id purpose)
      | Some latest ->
          let* version = Key_version.next latest in
          let* key = Mac_key.generate ~tenant_id ~purpose ~version in
          let* () = save_mac_key t tx key in
          Ok version)

let retire_mac_versions_before t session ~tenant_id purpose bound =
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val Session.connection tx) in
      let* () = lock_mac t tx ~tenant_id purpose in
      let* latest = latest_mac_version t tx ~tenant_id purpose in
      match latest with
      | None -> Error (mac_key_not_found ~tenant_id purpose)
      | Some latest ->
          retire_below ~latest ~bound (fun stored ->
              caqti (fun () ->
                  C.exec t.requests.retire_mac
                    (tenant_id, Key_label.to_string purpose, stored))))

let delete_mac_key t session ~tenant_id purpose =
  let module C = (val Session.connection session) in
  caqti (fun () -> C.exec t.requests.delete_mac (tenant_id, Key_label.to_string purpose))
