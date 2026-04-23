(** Forgettable Payload — wraps PII in event sourced logs.

    PII is encrypted with the aggregate's DEK. When the right to be
    forgotten is exercised, the DEK is destroyed, making all PII
    in the event log unrecoverable while non-PII data remains intact. *)

type 'a t = {
  encrypted : string option;  (** Encrypted PII bytes, or None if forgotten *)
  plaintext_cache : 'a option; (** In-memory cache of decrypted value *)
}

let create ~encrypt_fn value =
  let encrypted = Some (encrypt_fn value) in
  { encrypted; plaintext_cache = Some value }

let decrypt ~decrypt_fn t =
  match t.plaintext_cache with
  | Some v -> Some v
  | None -> (
      match t.encrypted with
      | None -> None  (* Forgotten *)
      | Some enc -> decrypt_fn enc)

let forget _t = { encrypted = None; plaintext_cache = None }

let is_forgotten t = Option.is_none t.encrypted

let encrypted_bytes t = t.encrypted
