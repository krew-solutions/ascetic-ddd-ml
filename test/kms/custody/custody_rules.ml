(** One body of tests for the custody of signing and MAC keys, run on every adapter that
    serves it, in memory, PostgreSQL, Vault, so that the rules of ADR-0036 hold the same
    everywhere, and the in-memory keys a consumer's tests run on behave as the adapters
    do. Each test file brings its adapter, built with managed keys, and the scope it runs
    in. *)

open Ascetic_kms

(** An adapter serving the signing and MAC ports and their custody. *)
module type KEYS = sig
  type t
  type session

  include Signing_port.S with type t := t and type session := session
  include Signing_key_custody.S with type t := t and type session := session
  include Public_key_port.S with type t := t and type session := session
  include Mac_port.S with type t := t and type session := session
  include Mac_key_custody.S with type t := t and type session := session
end

module Make (Keys : KEYS) = struct
  type scope = {
    atomic :
      'a. (Keys.session -> Keys.t -> ('a, Kms_error.t) result) -> ('a, Kms_error.t) result;
  }
  (** The scope a test runs the adapter in: one transaction per call. *)

  let ( let* ) = Result.bind
  let ed25519 = Signature_algorithm.Ed25519
  let v = Key_version.of_int_exn
  let versions = List.map Key_version.to_int
  let error = Alcotest.testable Kms_error.pp Kms_error.equal

  let get what = function
    | Ok value -> value
    | Error e -> Alcotest.failf "%s: %a" what Kms_error.pp e

  let is_signing_key_not_found = function
    | Error (Kms_error.Signing_key_not_found _) -> true
    | _ -> false

  let is_mac_key_not_found = function
    | Error (Kms_error.Mac_key_not_found _) -> true
    | _ -> false

  let shown = function
    | Ok _ -> "Ok"
    | Error e -> Kms_error.to_string e

  (* The versions the keyring publishes, or the refusal. *)
  let published c ~tenant =
    c.atomic (fun tx keys ->
        Result.map Public_keyring.versions
          (Keys.public_keys keys tx ~tenant_id:tenant ed25519))

  (** A signing key's custody, end to end: made once, rotated, retired, never below the
      floor, never the latest, and deleted; and what signs meanwhile makes no key. *)
  let signing_custody c ~tenant =
    let sign () =
      c.atomic (fun tx keys -> Keys.sign keys tx ~tenant_id:tenant ed25519 "hello")
    in
    let rotate () =
      c.atomic (fun tx keys -> Keys.rotate_signing_key keys tx ~tenant_id:tenant ed25519)
    in
    let create () =
      c.atomic (fun tx keys ->
          Keys.create_signing_key keys tx ~tenant_id:tenant ed25519 Key_options.default)
    in
    let retire ?(tenant = tenant) bound =
      c.atomic (fun tx keys ->
          Keys.retire_signing_versions_before keys tx ~tenant_id:tenant ed25519 (v bound))
    in
    (* Managed, nothing signs before custody makes the key, and nothing is
       made by trying. *)
    let unsigned = sign () in
    Alcotest.(check bool)
      ("no key to sign with: " ^ shown unsigned)
      true
      (is_signing_key_not_found unsigned);
    Alcotest.(check bool)
      "a refused signature leaves no key behind" true
      (is_signing_key_not_found (published c ~tenant));
    let rotated = rotate () in
    Alcotest.(check bool)
      ("a rotation makes no first version: " ^ shown rotated)
      true
      (is_signing_key_not_found rotated);
    (* Made once: a second making answers the latest and makes nothing. The
       calls are sequenced by [let]: a list's elements are not. *)
    let made = get "create" (create ()) in
    let again = get "create again" (create ()) in
    Alcotest.(check (list int)) "made once" [ 1; 1 ] (versions [ made; again ]);
    let first = get "sign" (sign ()) in
    Alcotest.(check int)
      "the key custody made signs" 1
      (Key_version.to_int (Signature.key_version first));
    (* Rotated twice; the earlier versions still verify. *)
    let second = get "rotate" (rotate ()) in
    let third = get "rotate" (rotate ()) in
    let latest = get "create" (create ()) in
    Alcotest.(check (list int))
      "rotated twice, then the latest answered" [ 2; 3; 3 ]
      (versions [ second; third; latest ]);
    Alcotest.(check (list int))
      "a keyring of three" [ 1; 2; 3 ]
      (versions (get "keyring" (published c ~tenant)));
    (* Retired below 2: version 1 neither signs, verifies nor is published. *)
    get "retire" (retire 2);
    Alcotest.(check (list int))
      "two left" [ 2; 3 ]
      (versions (get "keyring" (published c ~tenant)));
    Alcotest.(check (result (Alcotest.testable Signature.pp Signature.equal) error))
      "the retired version signs nothing" (Error (Kms_error.No_key_of_version 1))
      (c.atomic (fun tx keys ->
           Keys.sign_with_version keys tx ~tenant_id:tenant ed25519 (v 1) "hello"));
    Alcotest.(check (result unit error))
      "nor verifies" (Error (Kms_error.No_key_of_version 1))
      (c.atomic (fun tx keys -> Keys.verify keys tx ~tenant_id:tenant "hello" first));
    (* A stale or a repeated retirement does nothing, the floor is never
       lowered; a bound above the latest is refused and retires nothing. *)
    List.iter (fun stale -> get "a stale retirement" (retire stale)) [ 0; 1; 2 ];
    Alcotest.(check (list int))
      "still two" [ 2; 3 ]
      (versions (get "keyring" (published c ~tenant)));
    Alcotest.(check (result unit error))
      "above the latest" (Error (Kms_error.No_key_of_version 4)) (retire 4);
    Alcotest.(check (list int))
      "nothing retired" [ 2; 3 ]
      (versions (get "keyring" (published c ~tenant)));
    (* A version that still signs may be retired, the latest stays. *)
    get "the compromise path retires a signing version" (retire 3);
    Alcotest.(check (list int))
      "the latest alone" [ 3 ]
      (versions (get "keyring" (published c ~tenant)));
    Alcotest.(check int)
      "the latest signs" 3
      (Key_version.to_int (Signature.key_version (get "sign" (sign ()))));
    (* Retirement of a key the tenant has not got is named. *)
    let absent = retire ~tenant:"zz-nobody" 1 in
    Alcotest.(check bool)
      ("a tenant without the key: " ^ shown absent)
      true
      (is_signing_key_not_found absent);
    (* Deleted, twice; then nothing signs and a rotation makes nothing. *)
    get "a deletion of nothing is not an error"
      (c.atomic (fun tx keys ->
           let* () = Keys.delete_signing_key keys tx ~tenant_id:tenant ed25519 in
           Keys.delete_signing_key keys tx ~tenant_id:tenant ed25519));
    Alcotest.(check bool) "gone" true (is_signing_key_not_found (published c ~tenant));
    let rotated = rotate () in
    Alcotest.(check bool)
      ("no rotation of a deleted key: " ^ shown rotated)
      true
      (is_signing_key_not_found rotated)

  (** A MAC key's custody, end to end, as the signing key's. *)
  let mac_custody c ~tenant =
    let purpose = get "label" (Key_label.make "etag") in
    let mac () =
      c.atomic (fun tx keys -> Keys.mac keys tx ~tenant_id:tenant purpose "hello")
    in
    let rotate () =
      c.atomic (fun tx keys -> Keys.rotate_mac_key keys tx ~tenant_id:tenant purpose)
    in
    let create () =
      c.atomic (fun tx keys ->
          Keys.create_mac_key keys tx ~tenant_id:tenant purpose Key_options.default)
    in
    let retire bound =
      c.atomic (fun tx keys ->
          Keys.retire_mac_versions_before keys tx ~tenant_id:tenant purpose (v bound))
    in
    let unmade = mac () in
    Alcotest.(check bool)
      ("no key to mac with: " ^ shown unmade)
      true
      (match unmade with
      | Error (Kms_error.Mac_key_not_found { purpose = "etag"; _ }) -> true
      | _ -> false);
    let rotated = rotate () in
    Alcotest.(check bool)
      ("a rotation makes no first version: " ^ shown rotated)
      true
      (is_mac_key_not_found rotated);
    let made = get "create" (create ()) in
    let again = get "create again" (create ()) in
    let first = get "mac" (mac ()) in
    Alcotest.(check (list int))
      "made once, and the key makes under version one" [ 1; 1; 1 ]
      (versions [ made; again; Mac.key_version first ]);
    let second = get "rotate" (rotate ()) in
    let before = get "mac" (mac ()) in
    get "retire" (retire 2);
    get "a stale retirement" (retire 1);
    let after = get "mac" (mac ()) in
    Alcotest.(check (list int))
      "rotated, and a MAC is made with the latest" [ 2; 2 ]
      (versions [ second; Mac.key_version before ]);
    Alcotest.(check bool)
      "a retirement changes nothing a MAC is made with" true (Mac.equal before after);
    Alcotest.(check (result unit error))
      "above the latest" (Error (Kms_error.No_key_of_version 3)) (retire 3);
    get "a deletion of nothing is not an error"
      (c.atomic (fun tx keys ->
           let* () = Keys.delete_mac_key keys tx ~tenant_id:tenant purpose in
           Keys.delete_mac_key keys tx ~tenant_id:tenant purpose));
    let gone = mac () in
    Alcotest.(check bool) ("gone: " ^ shown gone) true (is_mac_key_not_found gone)
end
