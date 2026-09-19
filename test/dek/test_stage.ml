(** The envelope stage on a PostgreSQL KMS: a message sealed on the way out opens on the
    way in, only for its tenant; and end to end, the outbox and the inbox hold nothing but
    ciphertext while the two ends see the clear. Needs a live database, named by
    [TEST_DATABASE_URL], and is skipped without one. *)

module Envelope = Ascetic_dek_envelope.Envelope_stage
module Reuse = Ascetic_dek_envelope.Reuse
module Message = Ascetic_bus.Message
module Failure = Ascetic_bus.Failure
module Kms_error = Ascetic_kms.Kms_error
module Algorithm = Ascetic_kms.Algorithm
module Pg_kms = Ascetic_kms_pg.Pg_kms
module Cached_kms = Ascetic_kms.Cached.Make (Pg_kms)
module Session = Ascetic_session_caqti.Caqti_session
module Pool = Ascetic_session_caqti.Caqti_session_pool
module Identifier = Ascetic_session_caqti.Identifier
module Sealing = Envelope.Make (Pool) (Pg_kms)
module Opening = Envelope.Make (Pool) (Cached_kms)

let lift error = Kms_error.Session error

let kms what = function
  | Ok value -> value
  | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

let passed what = function
  | Ok message -> message
  | Error failure -> Alcotest.failf "%s: %a" what Failure.pp failure

let master_key () = kms "generate_key" (Algorithm.generate_key Algorithm.Aes_256_gcm)

let exec session sql =
  let module C = (val Session.connection session) in
  let open Caqti_request.Infix in
  match C.exec ((Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql) () with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err

let connect ~sw env uri =
  match
    Caqti_eio_unix.connect_pool
      ~pool_config:(Caqti_pool_config.create ~max_size:8 ())
      ~sw
      ~stdenv:(env :> Caqti_eio.stdenv)
      uri
  with
  | Ok pool -> Pool.of_pool pool
  | Error err -> Alcotest.failf "connect_pool failed: %a" Caqti_error.pp err

(* A KMS in a table of its own per test, set up afresh. *)
let kms_in ?(master_key = master_key ()) sessions table =
  let service = Pg_kms.create ~table:(Identifier.of_string_exn table) master_key in
  kms "setup"
    (Pool.session sessions ~lift (fun session ->
         exec session (Printf.sprintf "DROP TABLE IF EXISTS %s" table);
         Pg_kms.setup service session));
  service

let with_stage ~name env uri body =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let service = kms_in sessions ("kms_keys_stage_" ^ name) in
  body sessions service (Sealing.create sessions service)

let identified tenant message_id payload =
  let header name value message = Message.with_header message name value in
  Message.make payload
  |> header Envelope.tenant_id tenant
  |> header Envelope.message_id message_id

let message tenant payload =
  identified tenant "00000000-0000-4000-8000-000000000001" payload

let key_headers = [ Envelope.dek; Envelope.dek_algorithm; Envelope.dek_bound_to ]

(* The sealed payload and its key headers of [from], under the identity of
   [onto]: what a substitution in a store or a broker looks like. *)
let moved ~from onto =
  List.fold_left
    (fun moved name ->
      Message.with_header moved name (Option.get (Message.header from name)))
    (Message.with_payload onto (Message.payload from))
    key_headers

let contains ~sub text =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = sub || at (i + 1))
  in
  at 0

let is_permanent = function
  | Error failure -> Failure.is_permanent failure
  | Ok _ -> false

let test_a_message_is_sealed_on_the_way_out_and_opened_on_the_way_in env uri =
  with_stage ~name:"round_trip" env uri @@ fun _ _ sealing ->
  let sealed =
    passed "outbound" (Sealing.outbound sealing (message "tenant-1" "the order's events"))
  in
  Alcotest.(check bool)
    "no plaintext on the wire" false
    (contains ~sub:"order" (Message.payload sealed));
  Alcotest.(check (option string))
    "the cipher is named" (Some "AES-256-GCM")
    (Message.header sealed Envelope.dek_algorithm);
  Alcotest.(check (option string))
    "the binding is named" (Some "tenant_id,message_id")
    (Message.header sealed Envelope.dek_bound_to);
  Alcotest.(check bool)
    "the wrapped key goes along" true
    (Option.is_some (Message.header sealed Envelope.dek));
  Alcotest.(check (option string))
    "the tenant stays" (Some "tenant-1")
    (Message.header sealed Envelope.tenant_id);
  let opened = passed "inbound" (Sealing.inbound sealing sealed) in
  Alcotest.(check string) "the clear" "the order's events" (Message.payload opened);
  List.iter
    (fun name ->
      Alcotest.(check (option string))
        (name ^ " is gone") None (Message.header opened name))
    key_headers;
  Alcotest.(check (option string))
    "the tenant stays" (Some "tenant-1")
    (Message.header opened Envelope.tenant_id);
  Alcotest.(check bool)
    "the id stays" true
    (Option.is_some (Message.header opened Envelope.message_id));
  (* Two messages of one tenant get two keys. *)
  let again =
    passed "outbound" (Sealing.outbound sealing (message "tenant-1" "the order's events"))
  in
  Alcotest.(check bool)
    "another key" false
    (Message.header again Envelope.dek = Message.header sealed Envelope.dek)

let test_another_tenant_does_not_open_it env uri =
  with_stage ~name:"other_tenant" env uri @@ fun _ _ sealing ->
  let sealed =
    passed "outbound" (Sealing.outbound sealing (message "tenant-1" "secret"))
  in
  (* The other tenant has keys of its own. *)
  ignore (passed "outbound" (Sealing.outbound sealing (message "tenant-2" "its own")));
  let relabelled =
    Message.with_header
      (Message.without_header sealed Envelope.tenant_id)
      Envelope.tenant_id "tenant-2"
  in
  Alcotest.(check bool)
    "refused for good" true
    (is_permanent (Sealing.inbound sealing relabelled))

let test_a_payload_moved_under_another_message_does_not_open env uri =
  with_stage ~name:"moved" env uri @@ fun _ _ sealing ->
  let a =
    passed "outbound"
      (Sealing.outbound sealing
         (identified "tenant-1" "00000000-0000-4000-8000-00000000000a" "shipped"))
  in
  let b = identified "tenant-1" "00000000-0000-4000-8000-00000000000b" "cancelled" in
  Alcotest.(check bool)
    "refused for good" true
    (is_permanent (Sealing.inbound sealing (moved ~from:a b)));
  (* The same message again, as at-least-once delivery brings it, opens. *)
  List.iter
    (fun _ ->
      Alcotest.(check string)
        "opens" "shipped"
        (Message.payload (passed "inbound" (Sealing.inbound sealing a))))
    [ (); () ]

let test_the_binding_is_the_sealing_side_s_and_travels_with_the_message env uri =
  (* Two stages over one KMS: one bound to the tenant and the id, one to the
     tenant alone. *)
  with_stage ~name:"binding" env uri @@ fun sessions service by_identity ->
  let by_tenant = Sealing.create ~bound_to:[ Envelope.tenant_id ] sessions service in
  (* A message with no id is refused under the default binding ... *)
  let unidentified =
    Message.with_header (Message.make "secret") Envelope.tenant_id "tenant-1"
  in
  Alcotest.(check bool)
    "refused for good" true
    (is_permanent (Sealing.outbound by_identity unidentified));
  (* ... and sealed by the stage bound to the tenant alone, which the other
     opens, reading the binding from the message. *)
  let sealed = passed "outbound" (Sealing.outbound by_tenant unidentified) in
  Alcotest.(check (option string))
    "the binding is named" (Some "tenant_id")
    (Message.header sealed Envelope.dek_bound_to);
  Alcotest.(check string)
    "opens" "secret"
    (Message.payload (passed "inbound" (Sealing.inbound by_identity sealed)))

(* A stage that reuses a DEK seals so many messages under one key, then draws
   a fresh one; every one opens, through a cache that unwraps each key once. *)
let test_a_reused_dek_serves_so_many_messages_and_each_opens env uri =
  with_stage ~name:"reuse" env uri @@ fun sessions service sealing ->
  let clock = Eio.Stdenv.mono_clock env in
  let sealing =
    Sealing.reusing sealing ~clock { Reuse.messages = 2; lifetime = 3600.0 }
  in
  let cached = Cached_kms.create ~clock service in
  let opening = Opening.create sessions cached in
  let sealed =
    List.map
      (fun i ->
        passed "outbound"
          (Sealing.outbound sealing
             (identified
                (if i < 4 then "tenant-1" else "tenant-2")
                (Printf.sprintf "00000000-0000-4000-8000-0000000000%02d" i)
                (Printf.sprintf "message %d" i))))
      [ 0; 1; 2; 3; 4 ]
  in
  let deks = List.map (fun m -> Option.get (Message.header m Envelope.dek)) sealed in
  let dek = List.nth deks in
  Alcotest.(check bool) "two messages under one key" true (dek 0 = dek 1);
  Alcotest.(check bool) "then a fresh one" false (dek 1 = dek 2);
  Alcotest.(check bool) "which serves two as well" true (dek 2 = dek 3);
  Alcotest.(check bool) "another tenant, another key" false (dek 3 = dek 4);
  List.iteri
    (fun i message ->
      Alcotest.(check string)
        "opens"
        (Printf.sprintf "message %d" i)
        (Message.payload (passed "inbound" (Opening.inbound opening message))))
    sealed;
  Alcotest.(check int) "each key was unwrapped once and kept" 3 (Cached_kms.length cached)

let test_a_reused_dek_serves_so_long env uri =
  with_stage ~name:"reuse_lifetime" env uri @@ fun _ _ sealing ->
  let clock = Eio_mock.Clock.Mono.make () in
  let at seconds =
    Eio_mock.Clock.Mono.set_time clock
      (Mtime.of_uint64_ns (Int64.of_float (seconds *. 1e9)))
  in
  at 1.0;
  let sealing =
    Sealing.reusing sealing ~clock { Reuse.messages = 1_000; lifetime = 60.0 }
  in
  let dek_at seconds =
    at seconds;
    Option.get
      (Message.header
         (passed "outbound" (Sealing.outbound sealing (message "tenant-1" "x")))
         Envelope.dek)
  in
  let first = dek_at 1.0 in
  Alcotest.(check bool) "still serving a minute short" true (dek_at 60.0 = first);
  Alcotest.(check bool) "a fresh one after its lifetime" false (dek_at 62.0 = first)

let test_a_message_without_a_tenant_is_refused_for_good env uri =
  with_stage ~name:"no_tenant" env uri @@ fun _ _ sealing ->
  Alcotest.(check bool)
    "on the way out" true
    (is_permanent (Sealing.outbound sealing (Message.make "secret")));
  Alcotest.(check bool)
    "on the way in" true
    (is_permanent (Sealing.inbound sealing (Message.make "secret")))

let test_a_header_that_is_not_what_it_should_be_is_refused_for_good env uri =
  with_stage ~name:"bad_headers" env uri @@ fun _ _ sealing ->
  let sealed =
    passed "outbound" (Sealing.outbound sealing (message "tenant-1" "secret"))
  in
  let replaced name value =
    Message.with_header (Message.without_header sealed name) name value
  in
  List.iter
    (fun (what, broken) ->
      Alcotest.(check bool) what true (is_permanent (Sealing.inbound sealing broken)))
    [
      ("a key that is not base64", replaced Envelope.dek "not base64!");
      ("a cipher this library has not", replaced Envelope.dek_algorithm "ROT13");
      ( "a binding to a header the message has not",
        replaced Envelope.dek_bound_to "tenant_id,absent" );
      ("a tenant that is not text", replaced Envelope.tenant_id "\xff\xfe");
      ("no key at all", Message.without_header sealed Envelope.dek);
    ]

let test_a_shredded_tenant_s_messages_are_refused_for_good env uri =
  with_stage ~name:"shredded" env uri @@ fun sessions service sealing ->
  let sealed =
    passed "outbound" (Sealing.outbound sealing (message "tenant-1" "secret"))
  in
  kms "delete_kek"
    (Pool.session sessions ~lift (fun session ->
         Pg_kms.delete_kek service session ~tenant_id:"tenant-1"));
  Alcotest.(check bool)
    "refused for good" true
    (is_permanent (Sealing.inbound sealing sealed))

let test_a_kms_out_of_reach_is_a_failure_of_the_moment env _uri =
  Eio.Switch.run @@ fun sw ->
  let unreachable =
    connect ~sw env (Uri.of_string "postgresql://nobody:nothing@127.0.0.1:1/nowhere")
  in
  let sealing = Sealing.create unreachable (Pg_kms.create (master_key ())) in
  match Sealing.outbound sealing (message "tenant-1" "secret") with
  | Ok _ -> Alcotest.fail "sealed without a KMS"
  | Error failure ->
      Alcotest.(check bool) "to be tried again" false (Failure.is_permanent failure)

(* The whole path of ADR-0001: the command's transaction seals into the
   outbox, a bridge carries the bytes into the inbox, and the inbox's consumer
   opens them for the handler. Both tables hold ciphertext; both ends see the
   clear. *)
let test_the_outbox_and_the_inbox_hold_nothing_but_ciphertext env uri =
  let module Bus = Ascetic_bus.Bus in
  let module Bridge = Ascetic_bus.Bridge in
  let module Transactional = Ascetic_bus.Transactional in
  let module Subscription = Ascetic_bus.Subscription in
  let module Outbox = Ascetic_outbox.Pg_outbox in
  let module Outbox_channel = Ascetic_outbox.Outbox_channel in
  let module Inbox = Ascetic_inbox.Pg_inbox in
  let module Inbox_channel = Ascetic_inbox.Inbox_channel in
  let bus what = function
    | Ok value -> value
    | Error e -> Alcotest.failf "%s: %a" what Ascetic_bus.Bus_error.pp e
  in
  with_stage ~name:"end_to_end" env uri @@ fun sessions _ sealing ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.mono_clock env in
  let id = Identifier.of_string_exn in
  let outbox : Failure.t Outbox.t =
    Outbox.create ~outbox_table:(id "dek_stage_outbox")
      ~offsets_table:(id "dek_stage_outbox_offsets")
      sessions
  in
  let inbox =
    Inbox.create ~table:(id "dek_stage_inbox") ~sequence:(id "dek_stage_inbox_seq")
      sessions
  in
  (match
     Pool.session sessions ~lift:Fun.id (fun session ->
         exec session
           "DROP TABLE IF EXISTS dek_stage_outbox, dek_stage_outbox_meta, \
            dek_stage_outbox_offsets, dek_stage_inbox, dek_stage_inbox_meta, \
            dek_stage_inbox_slots";
         exec session "DROP SEQUENCE IF EXISTS dek_stage_inbox_seq";
         (match Outbox.setup outbox session with
         | Ok () -> ()
         | Error e ->
             Alcotest.failf "outbox setup: %s"
               (Ascetic_outbox.Outbox_error.to_string Failure.to_string e));
         (match Inbox.setup inbox session with
         | Ok () -> ()
         | Error e ->
             Alcotest.failf "inbox setup: %s" (Ascetic_inbox.Inbox_error.to_string e));
         Ok ())
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "session: %a" Ascetic_session.Session_error.pp e);
  let registry =
    bus "register outbox"
      (Bus.register Bus.empty ~scheme:Outbox_channel.scheme
         (Outbox_channel.adapter ~sw ~clock
            ~loops:{ Ascetic_outbox.Loops.default with poll_interval = 0.02 }
            outbox))
  in
  let registry =
    bus "register inbox"
      (Bus.register registry ~scheme:Inbox_channel.scheme (Inbox_channel.adapter inbox))
  in
  let dispatcher =
    bus "bridge"
      (Bridge.run (Bridge.create registry) ~from:"outbox://all" ~group:"dispatcher"
         (Bridge.Header "destination"))
  in
  (* The consuming end opens what it receives. *)
  let received = ref [] in
  let orders =
    Transactional.Consumer.through
      (Inbox_channel.consumer ~sw ~clock
         ~loops:{ Ascetic_inbox.Loops.default with poll_interval = 0.02 } inbox
         ~decode:(fun message -> Ok (Message.payload message)))
      (Sealing.stage sealing)
  in
  let processing =
    bus "subscribe"
      (Transactional.Consumer.subscribe orders (fun _tx payload ->
           received := payload :: !received;
           Ok ()))
  in
  (* The producing end seals inside the command's transaction. *)
  let placed =
    Transactional.Producer.through
      (Outbox_channel.producer outbox ~destination:"inbox://orders/order-7"
         ~encode:(fun payload ->
           let header name value message = Message.with_header message name value in
           Message.make payload
           |> header Envelope.tenant_id "tenant-1"
           |> header "stream_type" "Order"
           |> header "stream_id" "\"order-7\""
           |> header "stream_position" "1"
           |> header Envelope.message_id "00000000-0000-4000-8000-000000000007"))
      (Sealing.stage sealing)
  in
  (match
     Pool.session sessions ~lift:Fun.id (fun session ->
         Session.atomic session ~lift:Fun.id (fun tx ->
             bus "publish"
               (Transactional.Producer.publish placed tx "order placed in the clear");
             Ok ()))
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "commit: %a" Ascetic_session.Session_error.pp e);
  let rec wait tries =
    if !received = [] && tries > 0 then begin
      Eio.Time.Mono.sleep clock 0.02;
      wait (tries - 1)
    end
  in
  wait 1000;
  Alcotest.(check (list string))
    "delivered in the clear"
    [ "order placed in the clear" ]
    !received;
  (* Neither table holds the clear; both hold the wrapped key beside it. *)
  let count sql =
    match
      Pool.session sessions ~lift:Fun.id (fun session ->
          let module C = (val Session.connection session) in
          let open Caqti_request.Infix in
          match C.find ((Caqti_type.unit ->! Caqti_type.int) ~oneshot:true sql) () with
          | Ok n -> Ok n
          | Error err -> Alcotest.failf "%s: %a" sql Caqti_error.pp err)
    with
    | Ok n -> n
    | Error e -> Alcotest.failf "session: %a" Ascetic_session.Session_error.pp e
  in
  let at_rest =
    "(SELECT payload, metadata FROM dek_stage_outbox UNION ALL SELECT payload, metadata \
     FROM dek_stage_inbox) AS at_rest"
  in
  Alcotest.(check int)
    "one row in the outbox, one in the inbox" 2
    (count ("SELECT count(*) FROM " ^ at_rest));
  Alcotest.(check int)
    "ciphertext at rest" 0
    (count
       ("SELECT count(*) FROM " ^ at_rest
      ^ " WHERE position('clear'::bytea in payload) > 0"));
  Alcotest.(check int)
    "the wrapped key and the cipher's name beside it" 2
    (count
       ("SELECT count(*) FROM " ^ at_rest
      ^ " WHERE metadata->>'dek' IS NOT NULL AND metadata->>'dek_algorithm' = \
         'AES-256-GCM'"));
  Subscription.cancel dispatcher;
  Subscription.cancel processing

let cases env uri =
  let case name test = Alcotest.test_case name `Quick (fun () -> test env uri) in
  [
    case "a message is sealed on the way out and opened on the way in"
      test_a_message_is_sealed_on_the_way_out_and_opened_on_the_way_in;
    case "another tenant does not open it" test_another_tenant_does_not_open_it;
    case "a payload moved under another message does not open"
      test_a_payload_moved_under_another_message_does_not_open;
    case "the binding is the sealing side's and travels with the message"
      test_the_binding_is_the_sealing_side_s_and_travels_with_the_message;
    case "a reused dek serves so many messages and each opens"
      test_a_reused_dek_serves_so_many_messages_and_each_opens;
    case "a reused dek serves so long" test_a_reused_dek_serves_so_long;
    case "a message without a tenant is refused for good"
      test_a_message_without_a_tenant_is_refused_for_good;
    case "a header that is not what it should be is refused for good"
      test_a_header_that_is_not_what_it_should_be_is_refused_for_good;
    case "a shredded tenant's messages are refused for good"
      test_a_shredded_tenant_s_messages_are_refused_for_good;
    case "a kms out of reach is a failure of the moment"
      test_a_kms_out_of_reach_is_a_failure_of_the_moment;
    case "the outbox and the inbox hold nothing but ciphertext"
      test_the_outbox_and_the_inbox_hold_nothing_but_ciphertext;
  ]

let () =
  match Sys.getenv_opt "TEST_DATABASE_URL" with
  | None ->
      print_endline "[skip] envelope stage tests: TEST_DATABASE_URL is not set";
      exit 0
  | Some url ->
      let uri = Uri.of_string url in
      Eio_main.run @@ fun env ->
      Alcotest.run "Envelope_stage" [ ("integration", cases env uri) ]
