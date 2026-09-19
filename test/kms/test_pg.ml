(** Integration tests for the PostgreSQL key management service. They need a live
    database, named by [TEST_DATABASE_URL], and are skipped without one. *)

open Ascetic_kms
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

let lift error = Kms_error.Session error
let ( let* ) = Result.bind

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let key () = get "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)
let error = Alcotest.testable Kms_error.pp Kms_error.equal
let a_key = Alcotest.testable Key.pp Key.equal
let unwrapped = Alcotest.(result a_key error)

let version_of wrapped =
  Key_version.to_int (Wrapped_key.key_version (get "parse" (Wrapped_key.parse wrapped)))

let hex text =
  String.init
    (String.length text / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * i) 2)))

(* ------------------------------------------------------------------------ *)
(* The fixture                                                                *)

type fixture = {
  sessions : Pool.t;
  kms : Pg_kms.t;
  table : string;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
}

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let with_session f body = get "session" (Pool.session f.sessions ~lift body)

(* Runs the body in one transaction. *)
let atomic f body =
  Pool.session f.sessions ~lift (fun session -> Session.atomic session ~lift body)

(* A table of its own per test; a fresh master key unless one is given. *)
let with_fixture ?master_key ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let pool_config = Caqti_pool_config.create ~max_size:8 () in
  let sessions =
    match Caqti_eio_unix.connect_pool ~pool_config ~sw ~stdenv uri with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let table = "kms_keys_" ^ name in
  let master_key = match master_key with Some key -> key | None -> key () in
  let kms = Pg_kms.create ~table:(Identifier.of_string_exn table) master_key in
  let f =
    {
      sessions;
      kms;
      table;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
    }
  in
  with_session f (fun session ->
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" table);
      Pg_kms.setup kms session);
  body f

let versions_of f tenant_id =
  with_session f (fun session ->
      let module C = (val Session.connection session) in
      let open Caqti_request.Infix in
      let request =
        (Caqti_type.string ->* Caqti_type.int)
          ~oneshot:true
          (Printf.sprintf
             "SELECT key_version FROM %s WHERE tenant_id = $1 ORDER BY key_version"
             f.table)
      in
      match C.collect_list request tenant_id with
      | Ok versions -> Ok versions
      | Error err -> Alcotest.failf "versions_of: %a" Caqti_error.pp err)

let rotate f tx tenant_id = Pg_kms.rotate_kek f.kms tx ~tenant_id

(* ------------------------------------------------------------------------ *)
(* The service                                                                *)

let test_a_dek_wrapped_after_a_rotation_unwraps env uri =
  with_fixture ~name:"rotate_and_wrap" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let dek = key () in
         let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         Ok ()))

let test_a_generated_dek_is_thirty_two_bytes_and_unwraps env uri =
  with_fixture ~name:"generate_dek" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let* dek, wrapped = Pg_kms.generate_dek f.kms tx ~tenant_id:"1" in
         Alcotest.(check int) "thirty-two bytes" 32 (Key.length dek);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         Ok ()))

let test_each_rotation_is_the_next_version env uri =
  with_fixture ~name:"rotation_versions" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* first = rotate f tx "1" in
         let* second = rotate f tx "1" in
         Alcotest.(check (list int))
           "one, then two" [ 1; 2 ]
           (List.map Key_version.to_int [ first; second ]);
         Ok ()));
  Alcotest.(check (list int)) "both rows" [ 1; 2 ] (versions_of f "1")

let test_what_an_earlier_version_wrapped_still_unwraps env uri =
  with_fixture ~name:"unwrap_after_rotation" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let dek = key () in
         let* wrapped_v1 = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
         let* _ = rotate f tx "1" in
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped_v1);
         Ok ()))

let test_a_rewrap_moves_a_dek_to_the_current_version env uri =
  with_fixture ~name:"rewrap" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let dek = key () in
         let* wrapped_v1 = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
         let* _ = rotate f tx "1" in
         let* wrapped_v2 = Pg_kms.rewrap_dek f.kms tx ~tenant_id:"1" wrapped_v1 in
         Alcotest.(check bool) "another wrapped form" false (wrapped_v1 = wrapped_v2);
         Alcotest.(check int) "was under one" 1 (version_of wrapped_v1);
         Alcotest.(check int) "is under two" 2 (version_of wrapped_v2);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped_v2);
         Ok ()))

let test_deleting_the_keks_shreds_what_they_wrapped env uri =
  with_fixture ~name:"crypto_shredding" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" (key ()) in
         let* () = Pg_kms.delete_kek f.kms tx ~tenant_id:"1" in
         Alcotest.check unwrapped "the key is gone"
           (Error (Kms_error.Kek_not_found { tenant_id = "1"; key_version = Some 1 }))
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         (* Nothing to delete is not an error. *)
         Pg_kms.delete_kek f.kms tx ~tenant_id:"1"));
  Alcotest.(check (list int)) "no rows" [] (versions_of f "1")

let test_one_tenant_s_key_does_not_unwrap_another_s_dek env uri =
  with_fixture ~name:"tenant_isolation" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = rotate f tx "1" in
         let* _ = rotate f tx "2" in
         let dek = key () in
         let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
         Alcotest.check unwrapped "its own tenant" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         Alcotest.check unwrapped "another tenant" (Error Kms_error.Decrypt)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"2" wrapped);
         Ok ()))

let test_the_first_contact_makes_the_key env uri =
  with_fixture ~name:"first_contact" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let dek = key () in
         let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
         Alcotest.(check int) "version one" 1 (version_of wrapped);
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         Ok ()));
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f "1")

let test_a_key_made_in_a_transaction_rolled_back_is_not_there env uri =
  with_fixture ~name:"rolled_back" env uri @@ fun f ->
  let outcome =
    atomic f (fun tx ->
        let* _ = rotate f tx "1" in
        Error (Kms_error.Malformed "the command failed after the key was made"))
  in
  Alcotest.(check bool) "failed" true (Result.is_error outcome);
  Alcotest.(check (list int)) "no rows" [] (versions_of f "1")

(* Runs [first] until it says it has made its key, then [second] beside it;
   checks that the second waits while the first holds its transaction open,
   then lets the first go. *)
let one_after_the_other f ~first ~second =
  let made, set_made = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  Eio.Switch.run @@ fun sw ->
  let first =
    Eio.Fiber.fork_promise ~sw (fun () ->
        atomic f (fun tx ->
            let* value = first tx in
            Eio.Promise.resolve set_made ();
            Eio.Promise.await release;
            Ok value))
  in
  Eio.Promise.await made;
  let second = Eio.Fiber.fork_promise ~sw (fun () -> atomic f second) in
  (* While the first holds the lock in its open transaction, the second waits. *)
  Eio.Time.Mono.sleep f.clock 0.3;
  Alcotest.(check bool) "the second waits" false (Eio.Promise.is_resolved second);
  Eio.Promise.resolve set_release ();
  (get "first" (Eio.Promise.await_exn first), get "second" (Eio.Promise.await_exn second))

(* Two transactions meet a new tenant: the first makes the key and holds its
   transaction open; the second waits on the tenant's lock, it neither fails
   on the primary key nor makes a version of its own, and, once the first
   commits, uses version 1. *)
let test_two_first_contacts_at_once_share_one_key env uri =
  with_fixture ~name:"first_contacts_at_once" env uri @@ fun f ->
  let first, (dek, wrapped) =
    one_after_the_other f
      ~first:(fun tx -> Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" (key ()))
      ~second:(fun tx ->
        let dek = key () in
        let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"1" dek in
        Ok (dek, wrapped))
  in
  Alcotest.(check int) "the first under version one" 1 (version_of first);
  Alcotest.(check int) "the second too" 1 (version_of wrapped);
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f "1");
  get "atomic"
    (atomic f (fun tx ->
         Alcotest.check unwrapped "unwraps" (Ok dek)
           (Pg_kms.decrypt_dek f.kms tx ~tenant_id:"1" wrapped);
         Ok ()))

(* Two rotations at once: the second waits for the first to commit, then
   makes the version after it. *)
let test_two_rotations_at_once_make_versions_one_and_two env uri =
  with_fixture ~name:"rotations_at_once" env uri @@ fun f ->
  let first, second =
    one_after_the_other f
      ~first:(fun tx -> rotate f tx "1")
      ~second:(fun tx -> rotate f tx "1")
  in
  Alcotest.(check (list int))
    "one, then two" [ 1; 2 ]
    (List.map Key_version.to_int [ first; second ]);
  Alcotest.(check (list int)) "both rows" [ 1; 2 ] (versions_of f "1")

(* Opens the pool's connections, so that fibers started together reach the
   database together: a pool connects on demand, and connecting takes longer
   than the statements under test. *)
let warm_up f ~connections =
  Eio.Fiber.List.iter
    (fun _ ->
      with_session f (fun session ->
          let module C = (val Session.connection session) in
          let open Caqti_request.Infix in
          match
            C.find
              ((Caqti_type.unit ->! Caqti_type.string)
                 ~oneshot:true "SELECT pg_sleep(0.2)::text")
              ()
          with
          | Ok _ -> Ok ()
          | Error err -> Alcotest.failf "warm_up: %a" Caqti_error.pp err))
    (List.init connections Fun.id)

(* Callers with no transaction of their own, as a stage of the bus is: the
   making runs in a scope of its own, so the lock holds over the read and the
   insert, and eight callers meeting a new tenant at once make one key. Every
   tenant is a race of its own, so one run is several tries. *)
let test_first_contacts_at_once_outside_a_transaction_share_one_key env uri =
  with_fixture ~name:"first_contacts_bare" env uri @@ fun f ->
  warm_up f ~connections:8;
  List.iter
    (fun tenant_id ->
      let outcomes =
        Eio.Fiber.List.map
          (fun _ ->
            Pool.session f.sessions ~lift (fun session ->
                Pg_kms.generate_dek f.kms session ~tenant_id))
          (List.init 8 Fun.id)
      in
      List.iter
        (fun outcome ->
          let _, wrapped = get ("generate_dek for " ^ tenant_id) outcome in
          Alcotest.(check int) "under version one" 1 (version_of wrapped))
        outcomes;
      Alcotest.(check (list int))
        ("one row for " ^ tenant_id) [ 1 ] (versions_of f tenant_id))
    (List.init 10 (Printf.sprintf "tenant-%d"))

let test_two_setups_at_once_agree env uri =
  with_fixture ~name:"setups_at_once" env uri @@ fun f ->
  with_session f (fun session ->
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" f.table);
      Ok ());
  let setup () =
    Pool.session f.sessions ~lift (fun session -> Pg_kms.setup f.kms session)
  in
  let a, b = Eio.Fiber.pair setup setup in
  get "the first setup" a;
  get "the second setup" b;
  ignore (get "atomic" (atomic f (fun tx -> rotate f tx "1")))

(* A row the Python port wrote, its first KEK for [tenant-1] under the master
   key [00 01 .. 1f], is read here, and unwraps the DEK it wrapped. *)
let test_a_key_the_python_port_wrote_is_read env uri =
  let kek_v1 =
    "00000001a81366893e77f9851f6b13b9f2e4181704bcc43283f37b81741bc0fd0137c70d02cf28faaa22ac0a7b228367770147acc3f14d68debe95405cac4fc7"
  and dek_under_v1 =
    "00000001b3d9d1393913407b952682e4e96b8c8c21d18b59211e65daf7a6060c07a8d24fed1899f548955ca16f07d75ce27ba7d225f03e83ae932f8c348a3628"
  in
  with_fixture
    ~master_key:(Key.of_string (String.init 32 Char.chr))
    ~name:"python_row" env uri
  @@ fun f ->
  with_session f (fun session ->
      let module C = (val Session.connection session) in
      let open Caqti_request.Infix in
      let insert =
        (Caqti_type.octets ->. Caqti_type.unit)
          ~oneshot:true
          (Printf.sprintf
             "INSERT INTO %s (tenant_id, key_version, encrypted_key, master_algorithm, \
              key_algorithm) VALUES ('tenant-1', 1, $1, 'AES-256-GCM', 'AES-256-GCM')"
             f.table)
      in
      match C.exec insert (hex kek_v1) with
      | Ok () -> Ok ()
      | Error err -> Alcotest.failf "insert: %a" Caqti_error.pp err);
  get "atomic"
    (atomic f (fun tx ->
         let* dek =
           Pg_kms.decrypt_dek f.kms tx ~tenant_id:"tenant-1" (hex dek_under_v1)
         in
         Alcotest.check a_key "the DEK the Python port wrapped"
           (Key.of_string (String.init 32 (fun i -> Char.chr (0x40 + i))))
           dek;
         (* And the row is the current key: nothing new is made. *)
         let* wrapped = Pg_kms.encrypt_dek f.kms tx ~tenant_id:"tenant-1" dek in
         Alcotest.(check int) "under the row's version" 1 (version_of wrapped);
         Ok ()));
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f "tenant-1")

let test_a_master_key_of_the_wrong_length_is_refused_at_first_use env uri =
  with_fixture ~master_key:(Key.of_string "too short") ~name:"short_master" env uri
  @@ fun f ->
  let outcome = atomic f (fun tx -> rotate f tx "1") in
  Alcotest.(check bool)
    "malformed" true
    (match outcome with Error (Kms_error.Malformed _) -> true | _ -> false);
  Alcotest.(check (list int)) "no rows" [] (versions_of f "1")

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (fun () -> test env uri) in
  [
    case "a dek wrapped after a rotation unwraps"
      test_a_dek_wrapped_after_a_rotation_unwraps;
    case "a generated dek is thirty two bytes and unwraps"
      test_a_generated_dek_is_thirty_two_bytes_and_unwraps;
    case "each rotation is the next version" test_each_rotation_is_the_next_version;
    case "what an earlier version wrapped still unwraps"
      test_what_an_earlier_version_wrapped_still_unwraps;
    case "a rewrap moves a dek to the current version"
      test_a_rewrap_moves_a_dek_to_the_current_version;
    case "deleting the keks shreds what they wrapped"
      test_deleting_the_keks_shreds_what_they_wrapped;
    case "one tenant's key does not unwrap another's dek"
      test_one_tenant_s_key_does_not_unwrap_another_s_dek;
    case "the first contact makes the key" test_the_first_contact_makes_the_key;
    case "a key made in a transaction rolled back is not there"
      test_a_key_made_in_a_transaction_rolled_back_is_not_there;
    case "two first contacts at once share one key"
      test_two_first_contacts_at_once_share_one_key;
    case "two rotations at once make versions one and two"
      test_two_rotations_at_once_make_versions_one_and_two;
    case "first contacts at once outside a transaction share one key"
      test_first_contacts_at_once_outside_a_transaction_share_one_key;
    case "two setups at once agree" test_two_setups_at_once_agree;
    case "a key the python port wrote is read" test_a_key_the_python_port_wrote_is_read;
    case "a master key of the wrong length is refused at first use"
      test_a_master_key_of_the_wrong_length_is_refused_at_first_use;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] kms integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env -> Alcotest.run "Pg_kms" [ ("integration", cases env uri) ]
