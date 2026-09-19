module Session = Ascetic_session_caqti.Caqti_session
module Identifier = Ascetic_session_caqti.Identifier
module Transient = Ascetic_session_caqti.Transient
module Error = Ascetic_dek.Dek_error
module Resource = Ascetic_dek.Resource
module Versioned_cipher = Ascetic_dek.Versioned_cipher
module Keyring = Ascetic_dek.Keyring
module Algorithm = Ascetic_kms.Algorithm
module Key_version = Ascetic_kms.Key_version

(* ------------------------------------------------------------------------ *)
(* Errors                                                                    *)

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
let of_kms result = Result.map_error (fun error -> Error.Kms error) result
let ( let* ) = Result.bind

(* ------------------------------------------------------------------------ *)
(* The statements                                                             *)

(* Built once per store. The resource's id travels as its canonical text and
   is compared as JSONB, so an id is found however its text was spelled when
   it was written. *)
module Requests = struct
  open Caqti_request.Infix
  open Caqti_type

  type row = int * string * string
  (* version, encrypted_dek, algorithm *)

  type t = {
    lock_table : (string, int, [ `One ]) Caqti_request.t;
    ddl : string;
    lock_resource : (string * string, int, [ `One ]) Caqti_request.t;
    latest : (string * string * string, row, [ `One | `Zero ]) Caqti_request.t;
    by_version : (string * string * string * int, row, [ `One | `Zero ]) Caqti_request.t;
    every : (string * string * string, row, [ `Many | `One | `Zero ]) Caqti_request.t;
    insert :
      (string * string * string * int * string * string, unit, [ `Zero ]) Caqti_request.t;
    of_tenant :
      (string, string * string * int * string, [ `Many | `One | `Zero ]) Caqti_request.t;
    update : (string * string * string * string * int, unit, [ `Zero ]) Caqti_request.t;
    delete : (string * string * string, unit, [ `Zero ]) Caqti_request.t;
  }

  let make table =
    let table = Identifier.to_string table in
    let sprintf = Printf.sprintf in
    let row = t3 int octets string in
    let resource = t3 string string string in
    {
      lock_table =
        (string ->! int) "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1))) AS l";
      ddl =
        sprintf
          "CREATE TABLE IF NOT EXISTS %s (\n\
          \  \"tenant_id\" VARCHAR(128) NOT NULL,\n\
          \  \"kind\" VARCHAR(128) NOT NULL,\n\
          \  \"resource_id\" JSONB NOT NULL,\n\
          \  \"version\" INTEGER NOT NULL,\n\
          \  \"encrypted_dek\" BYTEA NOT NULL,\n\
          \  \"algorithm\" VARCHAR(32) NOT NULL,\n\
          \  \"created_at\" TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,\n\
          \  CONSTRAINT %s_pk PRIMARY KEY (\"tenant_id\", \"kind\", \"resource_id\", \
           \"version\")\n\
           )"
          table table;
      lock_resource =
        (t2 string string ->! int)
          "SELECT 1 FROM (SELECT pg_advisory_xact_lock(hashtext($1), hashtext($2))) AS l";
      latest =
        (resource ->? row)
          (sprintf
             "SELECT version, encrypted_dek, algorithm FROM %s WHERE tenant_id = $1 AND \
              kind = $2 AND resource_id = $3::jsonb ORDER BY version DESC LIMIT 1"
             table);
      by_version =
        (t4 string string string int ->? row)
          (sprintf
             "SELECT version, encrypted_dek, algorithm FROM %s WHERE tenant_id = $1 AND \
              kind = $2 AND resource_id = $3::jsonb AND version = $4"
             table);
      every =
        (resource ->* row)
          (sprintf
             "SELECT version, encrypted_dek, algorithm FROM %s WHERE tenant_id = $1 AND \
              kind = $2 AND resource_id = $3::jsonb ORDER BY version"
             table);
      insert =
        (t6 string string string int octets string ->. unit)
          (sprintf
             "INSERT INTO %s (tenant_id, kind, resource_id, version, encrypted_dek, \
              algorithm) VALUES ($1, $2, $3::jsonb, $4, $5, $6)"
             table);
      of_tenant =
        (string ->* t4 string string int octets)
          (sprintf
             "SELECT kind, resource_id::text, version, encrypted_dek FROM %s WHERE \
              tenant_id = $1"
             table);
      update =
        (t5 octets string string string int ->. unit)
          (sprintf
             "UPDATE %s SET encrypted_dek = $1 WHERE tenant_id = $2 AND kind = $3 AND \
              resource_id = $4::jsonb AND version = $5"
             table);
      delete =
        (resource ->. unit)
          (sprintf
             "DELETE FROM %s WHERE tenant_id = $1 AND kind = $2 AND resource_id = \
              $3::jsonb"
             table);
    }
end

(* The column is INTEGER; a version is a count. *)
let max_stored = 0x7FFF_FFFF

let stored_version version =
  let version = Key_version.to_int version in
  if version <= max_stored then Ok version
  else
    Error
      (Error.Malformed (Printf.sprintf "DEK version %d does not fit the column" version))

let read_version stored =
  if stored < 0 then
    Error (Error.Malformed (Printf.sprintf "DEK version %d is not a count" stored))
  else of_kms (Key_version.of_int stored)

let default_table = Identifier.of_string_exn "deks"

module Make (Kms : Ascetic_kms.Kms_port.S with type session = Session.t) = struct
  type session = Session.t

  type t = {
    kms : Kms.t;
    table : Identifier.t;
    algorithm : Algorithm.t;
    requests : Requests.t;
  }

  let create ?(algorithm = Algorithm.Aes_256_gcm) ?(table = default_table) kms =
    { kms; table; algorithm; requests = Requests.make table }

  let table t = t.table
  let kms t = t.kms

  let setup t session =
    Session.atomic session ~lift:session_error (fun tx ->
        let module C = (val Session.connection tx) in
        let* _ =
          caqti (fun () -> C.find t.requests.lock_table (Identifier.to_string t.table))
        in
        let open Caqti_request.Infix in
        caqti (fun () ->
            C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true t.requests.ddl) ()))

  let key_of resource =
    (Resource.tenant_id resource, Resource.kind resource, Resource.id_text resource)

  (* The resource's cipher over the DEK, as the version, bound to the resource
     as associated data. *)
  let cipher resource ~version ~algorithm dek =
    let* cipher =
      of_kms (Algorithm.cipher algorithm dek ~aad:(Resource.canonical resource))
    in
    Ok (Versioned_cipher.make version cipher)

  (* The cipher of a stored row: the DEK unwrapped through the KMS, over the
     resource. *)
  let unwrap t session resource ((version, encrypted_dek, algorithm) : Requests.row) =
    let* version = read_version version in
    let* algorithm = of_kms (Algorithm.of_string algorithm) in
    let* dek =
      of_kms
        (Kms.decrypt_dek t.kms session ~tenant_id:(Resource.tenant_id resource)
           encrypted_dek)
    in
    cipher resource ~version ~algorithm dek

  (* The resource's row of the highest version, if any. *)
  let latest t session resource =
    let module C = (val Session.connection session) in
    caqti (fun () -> C.find_opt t.requests.latest (key_of resource))

  (* Serializes the making of one resource's first key until the end of the
     transaction. *)
  let lock_resource t session resource =
    let module C = (val Session.connection session) in
    let* _ =
      caqti (fun () ->
          C.find t.requests.lock_resource
            (Identifier.to_string t.table, Resource.canonical resource))
    in
    Ok ()

  let insert t session resource ~version encrypted_dek =
    let module C = (val Session.connection session) in
    let tenant_id, kind, id = key_of resource in
    let* stored = stored_version version in
    caqti (fun () ->
        C.exec t.requests.insert
          (tenant_id, kind, id, stored, encrypted_dek, Algorithm.to_string t.algorithm))

  (* The scope is what makes the lock hold over the read and the insert when
     the caller has no transaction open; inside one it is a savepoint, and the
     lock stays with the transaction. Another caller may make the key while
     this one waits for the lock: read again after taking it. *)
  let get_or_create t session resource =
    let* found = latest t session resource in
    match found with
    | Some row -> unwrap t session resource row
    | None ->
        Session.atomic session ~lift:session_error (fun tx ->
            let* () = lock_resource t tx resource in
            let* found = latest t tx resource in
            match found with
            | Some row -> unwrap t tx resource row
            | None ->
                let* dek, encrypted_dek =
                  of_kms
                    (Kms.generate_dek t.kms tx ~tenant_id:(Resource.tenant_id resource))
                in
                let version = Key_version.first in
                let* () = insert t tx resource ~version encrypted_dek in
                cipher resource ~version ~algorithm:t.algorithm dek)

  let get t session resource version =
    let module C = (val Session.connection session) in
    let tenant_id, kind, id = key_of resource in
    let* stored = stored_version version in
    let* found =
      caqti (fun () -> C.find_opt t.requests.by_version (tenant_id, kind, id, stored))
    in
    match found with
    | Some row -> unwrap t session resource row
    | None ->
        Error
          (Error.Dek_not_found { resource; version = Some (Key_version.to_int version) })

  let get_all t session resource =
    let module C = (val Session.connection session) in
    let* rows = caqti (fun () -> C.collect_list t.requests.every (key_of resource)) in
    let* keyring =
      List.fold_left
        (fun keyring row ->
          let* keyring = keyring in
          let* versioned = unwrap t session resource row in
          let version = Versioned_cipher.version versioned
          and cipher = Versioned_cipher.unversioned versioned in
          Ok
            (Some
               (match keyring with
               | None -> Keyring.make version cipher
               | Some keyring -> Keyring.add keyring version cipher)))
        (Ok None) rows
    in
    match keyring with
    | Some keyring -> Ok keyring
    | None -> Error (Error.Dek_not_found { resource; version = None })

  let rewrap t session ~tenant_id =
    let module C = (val Session.connection session) in
    let* rows = caqti (fun () -> C.collect_list t.requests.of_tenant tenant_id) in
    List.fold_left
      (fun count (kind, id, version, encrypted_dek) ->
        let* count = count in
        let* rewrapped = of_kms (Kms.rewrap_dek t.kms session ~tenant_id encrypted_dek) in
        let* () =
          caqti (fun () ->
              C.exec t.requests.update (rewrapped, tenant_id, kind, id, version))
        in
        Ok (count + 1))
      (Ok 0) rows

  let delete t session resource =
    let module C = (val Session.connection session) in
    caqti (fun () -> C.exec t.requests.delete (key_of resource))
end
