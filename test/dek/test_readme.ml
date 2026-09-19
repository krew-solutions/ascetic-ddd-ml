(** The examples of the README, compiled and run, so that what it shows is known to work.
    They need [TEST_DATABASE_URL] and are skipped without it. *)

module Pg_kms = Ascetic_kms_pg.Pg_kms
module Deks = Ascetic_dek_pg.Pg_dek_store.Make (Pg_kms)
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
open Ascetic_dek

let ( let* ) = Result.bind

(* How the session's and the KMS's failures are carried in the store's error. *)
let lift error = Dek_error.Session error
let of_kms result = Result.map_error (fun error -> Dek_error.Kms error) result

let store sessions master_key =
  let deks =
    Deks.create
      ~table:(Identifier.of_string_exn "deks_readme")
      (Pg_kms.create ~table:(Identifier.of_string_exn "kms_keys_dek_readme") master_key)
  in
  let order = Resource.make ~tenant_id:"tenant-1" ~kind:"Order" (`String "order-7") in
  Pool.session sessions ~lift (fun session ->
      let* () = of_kms (Pg_kms.setup (Deks.kms deks) session) in
      let* () = Deks.setup deks session in
      Session.atomic session ~lift (fun tx ->
          let* () = Deks.delete deks tx order in
          let* () = of_kms (Pg_kms.delete_kek (Deks.kms deks) tx ~tenant_id:"tenant-1") in
          let* cipher = Deks.get_or_create deks tx order in
          let* sealed = of_kms (Versioned_cipher.encrypt cipher "the order's events") in
          let* keyring = Deks.get_all deks tx order in
          let* opened = of_kms (Keyring.decrypt keyring sealed) in
          assert (opened = "the order's events");
          Ok ()))

(* The envelope stage, as a composition root wires it: types only. *)
module Sealing = Ascetic_dek_envelope.Envelope_stage.Make (Pool) (Pg_kms)

let _wiring kms_sessions kms outbox inbox ~sw ~clock ~destination ~encode ~decode =
  let module Transactional = Ascetic_bus.Transactional in
  let sealing = Sealing.stage (Sealing.create kms_sessions kms) in
  let placed =
    Transactional.Producer.through
      (Ascetic_outbox.Outbox_channel.producer outbox ~destination ~encode)
      sealing
  in
  let orders =
    Transactional.Consumer.through
      (Ascetic_inbox.Inbox_channel.consumer ~sw ~clock inbox ~decode)
      sealing
  in
  (placed, orders)

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] dek README examples: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      Eio_main.run @@ fun env ->
      Alcotest.run "Dek README"
        [
          ( "examples",
            [
              Alcotest.test_case "the store" `Quick (fun () ->
                  Eio.Switch.run @@ fun sw ->
                  match
                    Caqti_eio_unix.connect_pool ~sw
                      ~stdenv:(env :> Caqti_eio.stdenv)
                      (Uri.of_string url)
                  with
                  | Error err -> Alcotest.failf "connect_pool: %a" Caqti_error.pp err
                  | Ok pool -> (
                      match
                        Ascetic_kms.Algorithm.generate_key
                          Ascetic_kms.Algorithm.Aes_256_gcm
                      with
                      | Error e ->
                          Alcotest.failf "generate_key: %a" Ascetic_kms.Kms_error.pp e
                      | Ok master_key -> (
                          match store (Pool.of_pool pool) master_key with
                          | Ok () -> ()
                          | Error e -> Alcotest.failf "store: %a" Dek_error.pp e)));
            ] );
        ]
