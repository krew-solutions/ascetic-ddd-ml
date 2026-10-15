(** The envelope stage on a PostgreSQL KMS: a message sealed on the way out opens on the
    way in, only for its tenant and only on its channel; end to end, the outbox and the
    inbox hold nothing but ciphertext while the two ends see the clear; and the sealing of
    a service's channels, with the stage an inbox of several channels chooses by the row.
    Needs a live database, named by [TEST_DATABASE_URL], and is skipped without one. *)

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

(* The KMS of a table under a master key, as an instance serving a channel
   of the bus when one is named: the context, and no key made on first
   contact, which is custody's (ADR-0024). *)
let kms_of ?derivation_context table master_key =
  let id = Identifier.of_string_exn in
  Pg_kms.create ~table:(id table)
    ~signing_table:(id (table ^ "_s"))
    ~mac_table:(id (table ^ "_m"))
    ?derivation_context
    ~managed_keys:(Option.is_some derivation_context)
    master_key

(* A KMS in a table of its own per test, set up afresh. *)
let kms_in ?(master_key = master_key ()) sessions table =
  let service = kms_of table master_key in
  kms "setup"
    (Pool.session sessions ~lift (fun session ->
         exec session
           (Printf.sprintf "DROP TABLE IF EXISTS %s, %s_s, %s_m" table table table);
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

(* One stage serves one channel (ADR-0024), and the stage of an inbox that
   receives several channels is chosen by the row (ADR-0025). *)

module Channels = Ascetic_dek_envelope.Sealing.Make (Pool) (Pg_kms)

let payload what = function
  | Ok message -> Message.payload message
  | Error failure -> Alcotest.failf "%s: %a" what Failure.pp failure

let permanent_saying what ~sub = function
  | Error failure ->
      Alcotest.(check bool) (what ^ ": for good") true (Failure.is_permanent failure);
      Alcotest.(check bool)
        (what ^ ": the reason") true
        (contains ~sub (Failure.message failure))
  | Ok _ -> Alcotest.failf "%s: opened" what

(* A table of KEKs with a key for [tenant-1] made by custody, for the stages
   that make none themselves. *)
let tenant_with_a_key sessions table =
  let master = master_key () in
  let custody = kms_in ~master_key:master sessions table in
  ignore
    (kms "create_kek"
       (Pool.session sessions ~lift (fun session ->
            Session.atomic session ~lift (fun tx ->
                Pg_kms.create_kek custody tx ~tenant_id:"tenant-1"
                  Ascetic_kms.Kek_options.default))));
  master

(* The stage of one channel over the table's KMS serving it as the context. *)
let stage_of sessions table master context =
  Sealing.stage
    (Sealing.create sessions (kms_of ~derivation_context:context table master))

(* The sealing of the channels, as a composition root builds it: the KMS of
   the table under the master, an instance per channel serving the channel
   as its context, making no key. *)
let sealing_of env sessions table master channels =
  Channels.create ~clock:(Eio.Stdenv.mono_clock env) sessions ~channels (fun channel ->
      kms_of ~derivation_context:channel table master)

let row_of sealed destination = Message.with_header sealed "destination" destination

let test_the_channel_s_other_end_opens_what_was_sealed_for_it_and_no_other_channel_does
    env
    uri =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_channel" in
  let master = tenant_with_a_key sessions table in
  let producer = stage_of sessions table master "policy.lifecycle" in
  let consumer = stage_of sessions table master "policy.lifecycle" in
  let sealed =
    passed "outbound" (producer.outbound (message "tenant-1" "a policy was created"))
  in
  Alcotest.(check string)
    "opens at the other end" "a policy was created"
    (payload "inbound" (consumer.inbound sealed));
  (* Another channel's consumer, over the same key, is refused for good: the
     DEK was drawn for the channel, and unwraps there and nowhere else. *)
  let other = stage_of sessions table master "integration.command" in
  Alcotest.(check bool)
    "another channel is refused for good" true
    (is_permanent (other.inbound sealed))

let test_the_sealing_of_the_channels_is_built_up_front_and_answers_for_those_alone env uri
    =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_sealing" in
  let master = tenant_with_a_key sessions table in
  (* The producer's sealing and the consumer's, built apart from one list. *)
  let channels = [ "policy.lifecycle"; "integration.command" ] in
  let producer = sealing_of env sessions table master channels in
  let consumer = sealing_of env sessions table master channels in
  Alcotest.(check (list string)) "the channels" channels (Channels.channels producer);
  Alcotest.(check bool)
    "a typo is no channel" true
    (Option.is_none (Channels.of_channel producer "policy.lifecycl"));
  let stage sealing channel = Option.get (Channels.of_channel sealing channel) in
  let sealed =
    passed "outbound"
      ((stage producer "policy.lifecycle").outbound
         (message "tenant-1" "a policy was created"))
  in
  Alcotest.(check string)
    "opens" "a policy was created"
    (payload "inbound" ((stage consumer "policy.lifecycle").inbound sealed));
  (* The other channel's stage, over the same key, is refused for good. *)
  Alcotest.(check bool)
    "the other channel refuses for good" true
    (is_permanent ((stage consumer "integration.command").inbound sealed))

let by_destination sealing map =
  match Channels.by_destination sealing map with
  | Ok stage -> stage
  | Error unsealed ->
      Alcotest.failf "by_destination: %a" Ascetic_dek_envelope.Sealing.pp_unsealed_channel
        unsealed

(* One inbox receives two channels; one stage, chosen by the row's
   destination, opens each row with the stage of the channel it was sealed
   under, and the key after the destination is left aside. *)
let test_one_stage_opens_the_rows_of_two_channels_by_their_destination env uri =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_bydest" in
  let master = tenant_with_a_key sessions table in
  let channels = [ "device.lifecycle"; "inbox://policy-commands" ] in
  let producers = sealing_of env sessions table master channels in
  let consumer =
    by_destination
      (sealing_of env sessions table master channels)
      [
        ("inbox://device.lifecycle", "device.lifecycle");
        ("inbox://policy-commands", "inbox://policy-commands");
      ]
  in
  Alcotest.(check (list string))
    "the destinations, keys left aside"
    [ "inbox://device.lifecycle"; "inbox://policy-commands" ]
    (List.sort compare
       (Ascetic_dek_envelope.Sealing.By_destination.destinations consumer));
  let stage = Ascetic_dek_envelope.Sealing.By_destination.stage consumer in
  let sealed_by channel id text =
    passed "outbound"
      ((Option.get (Channels.of_channel producers channel)).outbound
         (identified "tenant-1" id text))
  in
  let managed =
    sealed_by "device.lifecycle" "00000000-0000-4000-8000-000000000011" "device managed"
  in
  let resolve =
    sealed_by "inbox://policy-commands" "00000000-0000-4000-8000-000000000012"
      "resolve bundle"
  in
  Alcotest.(check string)
    "the device's row" "device managed"
    (payload "inbound"
       (stage.inbound (row_of managed "inbox://device.lifecycle/tenant-1")));
  Alcotest.(check string)
    "the command's row" "resolve bundle"
    (payload "inbound"
       (stage.inbound (row_of resolve "inbox://policy-commands/tenant-1")))

(* A row sealed under one channel but named for another's destination is
   opened with the other's stage and does not open: the binding is the
   channel's, and the verdict is for good. *)
let test_a_row_under_another_channel_s_destination_does_not_open env uri =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_bydest_other" in
  let master = tenant_with_a_key sessions table in
  let channels = [ "device.lifecycle"; "inbox://policy-commands" ] in
  let producers = sealing_of env sessions table master channels in
  let consumer =
    by_destination
      (sealing_of env sessions table master channels)
      [
        ("inbox://device.lifecycle", "device.lifecycle");
        ("inbox://policy-commands", "inbox://policy-commands");
      ]
  in
  let managed =
    passed "outbound"
      ((Option.get (Channels.of_channel producers "device.lifecycle")).outbound
         (identified "tenant-1" "00000000-0000-4000-8000-000000000013" "device managed"))
  in
  Alcotest.(check bool)
    "another channel's stage refuses for good" true
    (is_permanent
       ((Ascetic_dek_envelope.Sealing.By_destination.stage consumer).inbound
          (row_of managed "inbox://policy-commands/tenant-1")))

(* A row whose destination the stage was not built for, or without one, is
   refused for good before any stage, and so before any KMS, is asked. *)
let test_a_destination_outside_the_map_is_refused_for_good env uri =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_bydest_outside" in
  let master = tenant_with_a_key sessions table in
  let sealing = sealing_of env sessions table master [ "device.lifecycle" ] in
  let consumer =
    Ascetic_dek_envelope.Sealing.By_destination.stage
      (by_destination sealing [ ("inbox://device.lifecycle", "device.lifecycle") ])
  in
  let sealed =
    passed "outbound"
      ((Option.get (Channels.of_channel sealing "device.lifecycle")).outbound
         (identified "tenant-1" "00000000-0000-4000-8000-000000000014" "device managed"))
  in
  permanent_saying "outside the map" ~sub:"no stage for the destination"
    (consumer.inbound (row_of sealed "inbox://telemetry.semantic/tenant-1"));
  permanent_saying "no destination" ~sub:"no `destination` header"
    (consumer.inbound sealed)

(* A map naming a channel the sealing was not built for is refused at
   construction, before any row arrives. *)
let test_a_map_naming_a_channel_the_sealing_lacks_is_refused_up_front env uri =
  Eio.Switch.run @@ fun sw ->
  let sessions = connect ~sw env uri in
  let table = "ks_stage_bydest_lacking" in
  let master = tenant_with_a_key sessions table in
  let sealing = sealing_of env sessions table master [ "device.lifecycle" ] in
  match
    Channels.by_destination sealing
      [ ("inbox://telemetry.semantic", "telemetry.semantic") ]
  with
  | Ok _ -> Alcotest.fail "built for a channel the sealing lacks"
  | Error { destination; channel } ->
      Alcotest.(check string) "the channel" "telemetry.semantic" channel;
      Alcotest.(check string) "the destination" "inbox://telemetry.semantic" destination

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
    case "the channel's other end opens what was sealed for it and no other channel does"
      test_the_channel_s_other_end_opens_what_was_sealed_for_it_and_no_other_channel_does;
    case "the sealing of the channels is built up front and answers for those alone"
      test_the_sealing_of_the_channels_is_built_up_front_and_answers_for_those_alone;
    case "one stage opens the rows of two channels by their destination"
      test_one_stage_opens_the_rows_of_two_channels_by_their_destination;
    case "a row under another channel's destination does not open"
      test_a_row_under_another_channel_s_destination_does_not_open;
    case "a destination outside the map is refused for good"
      test_a_destination_outside_the_map_is_refused_for_good;
    case "a map naming a channel the sealing lacks is refused up front"
      test_a_map_naming_a_channel_the_sealing_lacks_is_refused_up_front;
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
