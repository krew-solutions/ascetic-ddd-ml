(** The examples of the README, compiled and run, so that what it shows is known to work.
    The one over PostgreSQL needs [TEST_DATABASE_URL] and is skipped without it. *)

open Ascetic_kms

let ( let* ) = Result.bind

(* The model. *)
let model () =
  let* master_key = Algorithm.generate_key Algorithm.Aes_256_gcm in
  let* master = Master_key.make master_key Algorithm.Aes_256_gcm in
  let* kek = Master_key.generate_kek master ~tenant_id:"tenant-1" in
  let* dek, wrapped = Kek.generate_dek kek in
  let* rotated = Master_key.rotate_kek master kek in
  let* rewrapped = Kek.rewrap rotated ~from:kek wrapped in
  let* again = Kek.unwrap rotated rewrapped in
  assert (Key.equal dek again);
  Ok ()

(* The service, inside the caller's transaction. *)
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool

let lift error = Kms_error.Session error

let service sessions master_key =
  let kms =
    Pg_kms.create
      ~table:(Ascetic_session_caqti.Identifier.of_string_exn "kms_keys_readme")
      master_key
  in
  Pool.session sessions ~lift (fun session ->
      let* () = Pg_kms.setup kms session in
      Session.atomic session ~lift (fun tx ->
          let* () = Pg_kms.delete_kek kms tx ~tenant_id:"tenant-1" in
          let* dek, wrapped = Pg_kms.generate_dek kms tx ~tenant_id:"tenant-1" in
          let* version = Pg_kms.rotate_kek kms tx ~tenant_id:"tenant-1" in
          let* rewrapped = Pg_kms.rewrap_dek kms tx ~tenant_id:"tenant-1" wrapped in
          let* again = Pg_kms.decrypt_dek kms tx ~tenant_id:"tenant-1" rewrapped in
          assert (Key_version.to_int version = 2 && Key.equal dek again);
          Ok ()))

let ok what = function
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let () =
  Eio_main.run @@ fun env ->
  let over_postgresql =
    match Sys.getenv_opt "TEST_DATABASE_URL" with
    | None -> []
    | Some url ->
        [
          Alcotest.test_case "the service, inside the caller's transaction" `Quick
            (fun () ->
              Eio.Switch.run @@ fun sw ->
              match
                Caqti_eio_unix.connect_pool ~sw
                  ~stdenv:(env :> Caqti_eio.stdenv)
                  (Uri.of_string url)
              with
              | Error err -> Alcotest.failf "connect_pool: %a" Caqti_error.pp err
              | Ok pool -> (
                  match Algorithm.generate_key Algorithm.Aes_256_gcm with
                  | Error e -> Alcotest.failf "generate_key: %a" Kms_error.pp e
                  | Ok master_key -> ok "service" (service (Pool.of_pool pool) master_key)
                  ));
        ]
  in
  Alcotest.run "Kms README"
    [
      ( "examples",
        Alcotest.test_case "the model" `Quick (fun () -> ok "model" (model ()))
        :: over_postgresql );
    ]
