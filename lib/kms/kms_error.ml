(** What can go wrong with keys, as a value. *)

type t =
  | Kek_not_found of { tenant_id : string; key_version : int option }
      (** The tenant has no key-encryption key, or none of that version; [None] when any
          version would have done. *)
  | Decrypt
      (** The ciphertext did not open: the wrong key, another tenant's, another channel's,
          or bytes changed since it was sealed. Authenticated encryption tells no more
          than that, by design. *)
  | Wrong_key_version of { expected : int; found : int }
      (** The ciphertext was sealed under another version of the key than the one asked to
          open it: [expected] is the version of the key that was asked, [found] the one
          the ciphertext names. *)
  | No_key_of_version of int
      (** The ciphertext or signature names a version of the key that the holder does not
          have, or no longer serves: a keyring without that version, a version an
          operator retired. *)
  | Signing_key_not_found of { tenant_id : string; algorithm : Signature_algorithm.t }
      (** The tenant has no signing key of the algorithm. *)
  | Mac_key_not_found of { tenant_id : string; purpose : string }
      (** The tenant has no MAC key for the purpose. *)
  | Invalid_signature
      (** The signature is not the key's over these bytes: tampering, another message, or
          another key of the same version. *)
  | Unsupported_algorithm of string
      (** An algorithm name this library does not implement, read from storage. *)
  | Malformed of string
      (** Bytes that are not what they were taken for: a ciphertext too short to carry its
          version, a nonce and a tag; a key of the wrong length for its algorithm; a
          version that is not a count; a name a key cannot have. *)
  | Conflict of { tenant_id : string; detail : string }
      (** The key exists with other options than the ones asked for, derived where a plain
          one was asked or the reverse, and an option that is fixed at creation cannot be
          changed on it. *)
  | Forbidden of { tenant_id : string; operation : string }
      (** The KMS refused the operation under this identity: the policy does not allow it
          on this key, or the derivation context is not one the policy admits. The key
          exists; no retry under the same identity and context will mend it. *)
  | Unsupported of string
      (** An option this adapter cannot execute, automatic rotation where there is no
          scheduler, say. Named so that it is refused, not ignored. *)
  | Entropy of string  (** The operating system gave no randomness. *)
  | Session of Ascetic_session.Session_error.t
      (** The session could not be opened or closed. *)
  | Database of Ascetic_session.Driver_error.t  (** The database refused a statement. *)
  | Transport of string
      (** Vault could not be reached, or its answer could not be read. *)
  | Vault of { status : int; meth : string; path : string; message : string }
      (** Vault answered, and refused: the HTTP status, the method and the path, under the
          mount, of the request refused, and what Vault said, joined; empty when it said
          nothing. *)

let equal (a : t) (b : t) = a = b

let pp ppf = function
  | Kek_not_found { tenant_id; key_version = None } ->
      Format.fprintf ppf "tenant `%s` has no key-encryption key" tenant_id
  | Kek_not_found { tenant_id; key_version = Some version } ->
      Format.fprintf ppf "tenant `%s` has no key-encryption key of version %d" tenant_id
        version
  | Decrypt ->
      Format.pp_print_string ppf
        "the ciphertext did not open: wrong key, wrong tenant, or changed bytes"
  | Wrong_key_version { expected; found } ->
      Format.fprintf ppf
        "the ciphertext was sealed under key version %d, this key is version %d" found
        expected
  | No_key_of_version version -> Format.fprintf ppf "no key of version %d at hand" version
  | Signing_key_not_found { tenant_id; algorithm } ->
      Format.fprintf ppf "tenant `%s` has no %a signing key" tenant_id
        Signature_algorithm.pp algorithm
  | Mac_key_not_found { tenant_id; purpose } ->
      Format.fprintf ppf "tenant `%s` has no MAC key for `%s`" tenant_id purpose
  | Invalid_signature ->
      Format.pp_print_string ppf "the signature is not the key's over these bytes"
  | Unsupported_algorithm name -> Format.fprintf ppf "unsupported algorithm `%s`" name
  | Malformed what -> Format.fprintf ppf "malformed: %s" what
  | Conflict { tenant_id; detail } ->
      Format.fprintf ppf "tenant `%s`'s key exists otherwise: %s" tenant_id detail
  | Forbidden { tenant_id; operation } ->
      Format.fprintf ppf
        "the KMS refused %s on tenant `%s`'s key under this identity or context" operation
        tenant_id
  | Unsupported what -> Format.fprintf ppf "unsupported here: %s" what
  | Entropy reason ->
      Format.fprintf ppf "no randomness from the operating system: %s" reason
  | Session error ->
      Format.fprintf ppf "session: %a" Ascetic_session.Session_error.pp error
  | Database reason ->
      Format.fprintf ppf "database: %a" Ascetic_session.Driver_error.pp reason
  | Transport reason -> Format.fprintf ppf "vault unreachable: %s" reason
  | Vault { status; meth; path; message } ->
      Format.fprintf ppf "vault: %d for %s %s: %s" status meth path message

let to_string error = Format.asprintf "%a" pp error
