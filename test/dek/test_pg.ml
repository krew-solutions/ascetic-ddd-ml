(** Integration tests for the PostgreSQL DEK store, plus the lock on a new resource. They
    need a live database, named by [TEST_DATABASE_URL], and are skipped without one. *)

open Ascetic_dek
module Kms_error = Ascetic_kms.Kms_error
module Algorithm = Ascetic_kms.Algorithm
module Key_version = Ascetic_kms.Key_version
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Store = Ascetic_dek_pg.Pg_dek_store.Make (Pg_kms)
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier

let lift error = Dek_error.Session error
let ( let* ) = Result.bind

let get what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Dek_error.pp e

let of_kms result = Result.map_error (fun error -> Dek_error.Kms error) result
let kms_error = Alcotest.testable Kms_error.pp Kms_error.equal
let bytes = Alcotest.(result string kms_error)
let order tenant_id id = Resource.make ~tenant_id ~kind:"Order" (`String id)

let version_of sealed =
  match Key_version.read ~what:"versioned" sealed with
  | Ok (version, _) -> version
  | Error e -> Alcotest.failf "version_of: %a" Kms_error.pp e

let seal cipher plaintext = of_kms (Versioned_cipher.encrypt cipher plaintext)

(* ------------------------------------------------------------------------ *)
(* The fixture                                                                *)

type fixture = {
  sessions : Pool.t;
  deks : Store.t;
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

(* Tables of its own per test; a fresh master key. *)
let with_fixture ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let sessions =
    match
      Caqti_eio_unix.connect_pool
        ~pool_config:(Caqti_pool_config.create ~max_size:8 ())
        ~sw ~stdenv uri
    with
    | Ok pool -> Pool.of_pool pool
    | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err
  in
  let table = "deks_" ^ name and kms_table = "kms_keys_dek_" ^ name in
  let master_key =
    match Algorithm.generate_key Algorithm.Aes_256_gcm with
    | Ok key -> key
    | Error e -> Alcotest.failf "generate_key: %a" Kms_error.pp e
  in
  let kms = Pg_kms.create ~table:(Identifier.of_string_exn kms_table) master_key in
  let deks = Store.create ~table:(Identifier.of_string_exn table) kms in
  let f =
    {
      sessions;
      deks;
      table;
      clock = (Eio.Stdenv.mono_clock env :> Eio.Time.Mono.ty Eio.Resource.t);
    }
  in
  with_session f (fun session ->
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s, %s" table kms_table);
      let* () = of_kms (Pg_kms.setup kms session) in
      Store.setup deks session);
  body f

let versions_of f resource =
  with_session f (fun session ->
      let module C = (val Session.connection session) in
      let open Caqti_request.Infix in
      let request =
        (Caqti_type.(t3 string string string) ->* Caqti_type.int)
          ~oneshot:true
          (Printf.sprintf
             "SELECT version FROM %s WHERE tenant_id = $1 AND kind = $2 AND resource_id \
              = $3::jsonb ORDER BY version"
             f.table)
      in
      match
        C.collect_list request
          (Resource.tenant_id resource, Resource.kind resource, Resource.id_text resource)
      with
      | Ok versions -> Ok versions
      | Error err -> Alcotest.failf "versions_of: %a" Caqti_error.pp err)

let rotate_kek f tx tenant_id =
  of_kms (Pg_kms.rotate_kek (Store.kms f.deks) tx ~tenant_id)

(* ------------------------------------------------------------------------ *)
(* The store                                                                  *)

let test_the_first_contact_makes_version_one env uri =
  with_fixture ~name:"first_contact" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* cipher = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* sealed = seal cipher "hello world" in
         Alcotest.(check bool) "sealed" false (String.equal sealed "hello world");
         Alcotest.(check int)
           "under version one" 1
           (Key_version.to_int (version_of sealed));
         Ok ()));
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f (order "1" "order-1"))

let test_a_second_contact_gives_the_same_key env uri =
  with_fixture ~name:"second_contact" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* first = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* second = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* sealed = seal first "hello" in
         Alcotest.check bytes "the second opens what the first sealed" (Ok "hello")
           (Versioned_cipher.decrypt second sealed);
         Ok ()));
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f (order "1" "order-1"))

let test_a_known_version_is_fetched_by_it env uri =
  with_fixture ~name:"known_version" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* cipher = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* sealed = seal cipher "hello" in
         let* loaded = Store.get f.deks tx (order "1" "order-1") (version_of sealed) in
         Alcotest.(check int)
           "version one" 1
           (Key_version.to_int (Versioned_cipher.version loaded));
         Alcotest.check bytes "opens" (Ok "hello")
           (Versioned_cipher.decrypt loaded sealed);
         Ok ()))

let test_a_missing_dek_is_reported env uri =
  with_fixture ~name:"missing" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let resource = order "1" "order-1" in
         Alcotest.(check bool)
           "not found, and the version is named" true
           (match Store.get f.deks tx resource Key_version.first with
           | Error (Dek_error.Dek_not_found { resource = named; version = Some 1 }) ->
               Resource.equal named resource
           | _ -> false);
         Ok ()))

let test_two_resources_get_two_keys env uri =
  with_fixture ~name:"two_resources" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* one = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* two = Store.get_or_create f.deks tx (order "1" "order-2") in
         let* sealed = seal one "hello" in
         Alcotest.check bytes "refused" (Error Kms_error.Decrypt)
           (Versioned_cipher.decrypt two sealed);
         Ok ()))

let test_two_tenants_get_two_keys env uri =
  with_fixture ~name:"two_tenants" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* one = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* two = Store.get_or_create f.deks tx (order "2" "order-1") in
         let* sealed = seal one "hello" in
         Alcotest.check bytes "refused" (Error Kms_error.Decrypt)
           (Versioned_cipher.decrypt two sealed);
         Ok ()))

let test_a_dek_survives_a_kek_rotation env uri =
  with_fixture ~name:"kek_rotation" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* cipher = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* sealed = seal cipher "hello" in
         let* _ = rotate_kek f tx "1" in
         let* loaded = Store.get f.deks tx (order "1" "order-1") (version_of sealed) in
         Alcotest.check bytes "opens" (Ok "hello")
           (Versioned_cipher.decrypt loaded sealed);
         Ok ()))

let test_deleting_the_dek_forgets_the_resource env uri =
  with_fixture ~name:"delete" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* () = Store.delete f.deks tx (order "1" "order-1") in
         Alcotest.(check bool)
           "gone" true
           (match Store.get f.deks tx (order "1" "order-1") Key_version.first with
           | Error (Dek_error.Dek_not_found _) -> true
           | _ -> false);
         Ok ()));
  Alcotest.(check (list int)) "no rows" [] (versions_of f (order "1" "order-1"))

let test_a_rewrap_after_a_rotation_moves_every_dek_of_the_tenant env uri =
  with_fixture ~name:"rewrap" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* one = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* two = Store.get_or_create f.deks tx (order "1" "order-2") in
         let* _other_tenant = Store.get_or_create f.deks tx (order "2" "order-1") in
         let* sealed_one = seal one "hello" in
         let* sealed_two = seal two "hello" in
         let* _ = rotate_kek f tx "1" in
         let* moved = Store.rewrap f.deks tx ~tenant_id:"1" in
         Alcotest.(check int) "the tenant's two, not the other tenant's" 2 moved;
         let* one = Store.get f.deks tx (order "1" "order-1") (version_of sealed_one) in
         let* two = Store.get f.deks tx (order "1" "order-2") (version_of sealed_two) in
         Alcotest.check bytes "the first opens" (Ok "hello")
           (Versioned_cipher.decrypt one sealed_one);
         Alcotest.check bytes "the second opens" (Ok "hello")
           (Versioned_cipher.decrypt two sealed_two);
         Ok ()))

let test_the_keyring_opens_every_version_and_seals_under_the_newest env uri =
  with_fixture ~name:"keyring" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let resource = order "1" "order-1" in
         let* v1 = Store.get_or_create f.deks tx resource in
         let* sealed_v1 = seal v1 "hello" in
         (* A second version, as an algorithm migration would write it. *)
         let* _, wrapped =
           of_kms (Pg_kms.generate_dek (Store.kms f.deks) tx ~tenant_id:"1")
         in
         let module C = (val Session.connection tx) in
         let open Caqti_request.Infix in
         let insert =
           (Caqti_type.(t4 string string string octets) ->. Caqti_type.unit)
             ~oneshot:true
             (Printf.sprintf
                "INSERT INTO %s (tenant_id, kind, resource_id, version, encrypted_dek, \
                 algorithm) VALUES ($1, $2, $3::jsonb, 2, $4, 'AES-256-GCM')"
                f.table)
         in
         (match
            C.exec insert
              ( Resource.tenant_id resource,
                Resource.kind resource,
                Resource.id_text resource,
                wrapped )
          with
         | Ok () -> ()
         | Error err -> Alcotest.failf "insert: %a" Caqti_error.pp err);
         let* keyring = Store.get_all f.deks tx resource in
         Alcotest.(check (list int))
           "both versions" [ 1; 2 ]
           (List.map Key_version.to_int (Keyring.versions keyring));
         Alcotest.check bytes "opens version one's" (Ok "hello")
           (Keyring.decrypt keyring sealed_v1);
         let* sealed_v2 = of_kms (Keyring.encrypt keyring "hello") in
         Alcotest.(check int)
           "seals under version two" 2
           (Key_version.to_int (version_of sealed_v2));
         Alcotest.check bytes "opens its own" (Ok "hello")
           (Keyring.decrypt keyring sealed_v2);
         let* v2 = Store.get f.deks tx resource (Key_version.of_int_exn 2) in
         Alcotest.check bytes "and version two alone opens it" (Ok "hello")
           (Versioned_cipher.decrypt v2 sealed_v2);
         Ok ()))

let test_a_keyring_of_nothing_is_reported env uri =
  with_fixture ~name:"empty_keyring" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         Alcotest.(check bool)
           "not found, no version named" true
           (match Store.get_all f.deks tx (order "1" "order-1") with
           | Error (Dek_error.Dek_not_found { version = None; _ }) -> true
           | _ -> false);
         Ok ()))

let test_shredding_the_tenant_s_kek_shreds_its_deks env uri =
  with_fixture ~name:"crypto_shredding" env uri @@ fun f ->
  get "atomic"
    (atomic f (fun tx ->
         let* _ = Store.get_or_create f.deks tx (order "1" "order-1") in
         let* () = of_kms (Pg_kms.delete_kek (Store.kms f.deks) tx ~tenant_id:"1") in
         Alcotest.(check bool)
           "the KMS has no key to unwrap with" true
           (match Store.get f.deks tx (order "1" "order-1") Key_version.first with
           | Error (Dek_error.Kms (Kms_error.Kek_not_found _)) -> true
           | _ -> false);
         Ok ()))

let test_a_composite_id_is_found_however_it_was_built env uri =
  with_fixture ~name:"composite_id" env uri @@ fun f ->
  let built_one_way =
    Resource.make ~tenant_id:"1" ~kind:"Order"
      (`Assoc [ ("shop", `Int 7); ("number", `String "A-1") ])
  and built_another =
    Resource.make ~tenant_id:"1" ~kind:"Order"
      (`Assoc [ ("number", `String "A-1"); ("shop", `Int 7) ])
  in
  get "atomic"
    (atomic f (fun tx ->
         let* one = Store.get_or_create f.deks tx built_one_way in
         let* other = Store.get_or_create f.deks tx built_another in
         let* sealed = seal one "hello" in
         Alcotest.check bytes "one key" (Ok "hello")
           (Versioned_cipher.decrypt other sealed);
         Ok ()));
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f built_another)

(* Two transactions meet a new resource: the first makes the key and holds its
   transaction open; the second waits on the resource's lock, and once the
   first commits, uses the same key. *)
let test_two_first_contacts_at_once_share_one_dek env uri =
  with_fixture ~name:"first_contacts_at_once" env uri @@ fun f ->
  let resource = order "1" "order-1" in
  let made, set_made = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  let sealed_first, sealed_second =
    Eio.Switch.run @@ fun sw ->
    let first =
      Eio.Fiber.fork_promise ~sw (fun () ->
          atomic f (fun tx ->
              let* cipher = Store.get_or_create f.deks tx resource in
              let* sealed = seal cipher "first" in
              Eio.Promise.resolve set_made ();
              Eio.Promise.await release;
              Ok sealed))
    in
    Eio.Promise.await made;
    let second =
      Eio.Fiber.fork_promise ~sw (fun () ->
          atomic f (fun tx ->
              let* cipher = Store.get_or_create f.deks tx resource in
              seal cipher "second"))
    in
    Eio.Time.Mono.sleep f.clock 0.3;
    Alcotest.(check bool) "the second waits" false (Eio.Promise.is_resolved second);
    Eio.Promise.resolve set_release ();
    ( get "first" (Eio.Promise.await_exn first),
      get "second" (Eio.Promise.await_exn second) )
  in
  Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f resource);
  get "atomic"
    (atomic f (fun tx ->
         let* cipher = Store.get f.deks tx resource Key_version.first in
         Alcotest.check bytes "the first's" (Ok "first")
           (Versioned_cipher.decrypt cipher sealed_first);
         Alcotest.check bytes "the second's" (Ok "second")
           (Versioned_cipher.decrypt cipher sealed_second);
         Ok ()))

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

(* Callers with no transaction of their own: the making runs in a scope of its
   own, so eight of them meeting a new resource at once make one key, of a
   tenant that is new too. Every resource is a race of its own. *)
let test_first_contacts_at_once_outside_a_transaction_share_one_dek env uri =
  with_fixture ~name:"first_contacts_bare" env uri @@ fun f ->
  warm_up f ~connections:8;
  List.iter
    (fun n ->
      let resource = order (Printf.sprintf "tenant-%d" n) "order-1" in
      let sealed =
        Eio.Fiber.List.map
          (fun _ ->
            Pool.session f.sessions ~lift (fun session ->
                let* cipher = Store.get_or_create f.deks session resource in
                seal cipher "hello"))
          (List.init 8 Fun.id)
      in
      Alcotest.(check (list int)) "one row" [ 1 ] (versions_of f resource);
      get "atomic"
        (atomic f (fun tx ->
             let* cipher = Store.get f.deks tx resource Key_version.first in
             List.iter
               (fun sealed ->
                 Alcotest.check bytes "every one opens under the one key" (Ok "hello")
                   (Versioned_cipher.decrypt cipher (get "get_or_create" sealed)))
               sealed;
             Ok ())))
    (List.init 10 Fun.id)

let test_two_setups_at_once_agree env uri =
  with_fixture ~name:"setups_at_once" env uri @@ fun f ->
  with_session f (fun session ->
      exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" f.table);
      Ok ());
  let setup () =
    Pool.session f.sessions ~lift (fun session -> Store.setup f.deks session)
  in
  let a, b = Eio.Fiber.pair setup setup in
  get "the first setup" a;
  get "the second setup" b;
  ignore
    (get "atomic"
       (atomic f (fun tx -> Store.get_or_create f.deks tx (order "1" "order-1"))))

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (fun () -> test env uri) in
  [
    case "the first contact makes version one" test_the_first_contact_makes_version_one;
    case "a second contact gives the same key" test_a_second_contact_gives_the_same_key;
    case "a known version is fetched by it" test_a_known_version_is_fetched_by_it;
    case "a missing dek is reported" test_a_missing_dek_is_reported;
    case "two resources get two keys" test_two_resources_get_two_keys;
    case "two tenants get two keys" test_two_tenants_get_two_keys;
    case "a dek survives a kek rotation" test_a_dek_survives_a_kek_rotation;
    case "deleting the dek forgets the resource"
      test_deleting_the_dek_forgets_the_resource;
    case "a rewrap after a rotation moves every dek of the tenant"
      test_a_rewrap_after_a_rotation_moves_every_dek_of_the_tenant;
    case "the keyring opens every version and seals under the newest"
      test_the_keyring_opens_every_version_and_seals_under_the_newest;
    case "a keyring of nothing is reported" test_a_keyring_of_nothing_is_reported;
    case "shredding the tenant's kek shreds its deks"
      test_shredding_the_tenant_s_kek_shreds_its_deks;
    case "a composite id is found however it was built"
      test_a_composite_id_is_found_however_it_was_built;
    case "two first contacts at once share one dek"
      test_two_first_contacts_at_once_share_one_dek;
    case "first contacts at once outside a transaction share one dek"
      test_first_contacts_at_once_outside_a_transaction_share_one_dek;
    case "two setups at once agree" test_two_setups_at_once_agree;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] dek integration tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Pg_dek_store" [ ("integration", cases env uri) ]
