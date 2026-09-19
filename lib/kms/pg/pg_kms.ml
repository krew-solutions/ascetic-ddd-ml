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

  type row = int * string * string * string
  (* key_version, encrypted_key, master_algorithm, key_algorithm *)

  type t = {
    lock_table : (string, int, [ `One ]) Caqti_request.t;
    ddl : string;
    lock_tenant : (string * string, int, [ `One ]) Caqti_request.t;
    current : (string, row, [ `One | `Zero ]) Caqti_request.t;
    by_version : (string * int, row, [ `One | `Zero ]) Caqti_request.t;
    insert : (string * int * string * string * string, unit, [ `Zero ]) Caqti_request.t;
    delete : (string, unit, [ `Zero ]) Caqti_request.t;
  }

  let make table =
    let table = Identifier.to_string table in
    let sprintf = Printf.sprintf in
    let row = t4 int octets string string in
    {
      lock_table =
        (string ->! int) "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1))) AS l";
      ddl =
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
      lock_tenant =
        (t2 string string ->! int)
          "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1), hashtext($2))) AS l";
      current =
        (string ->? row)
          (sprintf
             "SELECT key_version, encrypted_key, master_algorithm, key_algorithm FROM %s \
              WHERE tenant_id = $1 ORDER BY key_version DESC LIMIT 1"
             table);
      by_version =
        (t2 string int ->? row)
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
    }
end

type t = {
  master_key : Key.t;
  master_algorithm : Algorithm.t;
  table : Identifier.t;
  requests : Requests.t;
}

let default_table = Identifier.of_string_exn "kms_keys"

let create ?(master_algorithm = Algorithm.Aes_256_gcm) ?(table = default_table) master_key
    =
  { master_key; master_algorithm; table; requests = Requests.make table }

let table t = t.table

let setup t session =
  Session.atomic session ~lift:session_error (fun tx ->
      let module C = (val Session.connection tx) in
      let* _ =
        caqti (fun () -> C.find t.requests.lock_table (Identifier.to_string t.table))
      in
      let open Caqti_request.Infix in
      caqti (fun () ->
          C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true t.requests.ddl) ()))

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
   row names. *)
let load_kek t ~tenant_id
    ((version, encrypted_key, master_algorithm, key_algorithm) : Requests.row) =
  let* version = read_version version in
  let* wrapped = Wrapped_key.parse encrypted_key in
  let* master_algorithm = Algorithm.of_string master_algorithm in
  let* algorithm = Algorithm.of_string key_algorithm in
  let* master = master_key t master_algorithm in
  Master_key.load_kek master ~tenant_id ~version ~algorithm wrapped

(* ------------------------------------------------------------------------ *)
(* Keys                                                                       *)

(* Serializes the making and rotating of one tenant's keys until the end of
   the transaction. *)
let lock_tenant t session ~tenant_id =
  let module C = (val Session.connection session) in
  let* _ =
    caqti (fun () ->
        C.find t.requests.lock_tenant (Identifier.to_string t.table, tenant_id))
  in
  Ok ()

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

(* The tenant's current KEK, made if there is none: in a scope of its own,
   under the tenant's lock, and read again after taking it, in case another
   transaction made it meanwhile. The scope is what makes the lock hold over
   the read and the insert when the caller has no transaction open. *)
let get_or_create_current_kek t session ~tenant_id =
  let* found = current_kek t session ~tenant_id in
  match found with
  | Some kek -> Ok kek
  | None ->
      Session.atomic session ~lift:session_error (fun tx ->
          let* () = lock_tenant t tx ~tenant_id in
          let* found = current_kek t tx ~tenant_id in
          match found with
          | Some kek -> Ok kek
          | None ->
              let* master = master_key t t.master_algorithm in
              let* kek = Master_key.generate_kek master ~tenant_id in
              let* () = save_kek t tx kek in
              Ok kek)

(* ------------------------------------------------------------------------ *)
(* The port                                                                   *)

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

let rotate_kek t session ~tenant_id =
  Session.atomic session ~lift:session_error (fun tx ->
      let* () = lock_tenant t tx ~tenant_id in
      let* master = master_key t t.master_algorithm in
      let* current = current_kek t tx ~tenant_id in
      let* kek =
        match current with
        | Some current -> Master_key.rotate_kek master current
        | None -> Master_key.generate_kek master ~tenant_id
      in
      let* () = save_kek t tx kek in
      Ok (Kek.version kek))

let rewrap_dek t session ~tenant_id encrypted_dek =
  let* wrapped = Wrapped_key.parse encrypted_dek in
  let* from = kek t session ~tenant_id (Wrapped_key.key_version wrapped) in
  let* current = get_or_create_current_kek t session ~tenant_id in
  Result.map Wrapped_key.to_bytes (Kek.rewrap current ~from wrapped)

let delete_kek t session ~tenant_id =
  let module C = (val Session.connection session) in
  caqti (fun () -> C.exec t.requests.delete tenant_id)
