(** What can go wrong with data-encryption keys, as a value. *)

type t =
  | Dek_not_found of { resource : Resource.t; version : int option }
      (** The resource has no DEK, or none of that version; [None] when any version would
          have done. *)
  | Kms of Ascetic_kms.Kms_error.t
      (** The key management service refused, or a cipher over a DEK did. *)
  | Session of Ascetic_session.Session_error.t
      (** The session could not be opened or closed. *)
  | Database of Ascetic_session.Driver_error.t  (** The database refused a statement. *)
  | Malformed of string
      (** A stored value could not be read back: a version that is not a count. *)

let equal a b =
  match (a, b) with
  | Dek_not_found a, Dek_not_found b ->
      Resource.equal a.resource b.resource && a.version = b.version
  | Kms a, Kms b -> Ascetic_kms.Kms_error.equal a b
  | Session a, Session b -> a = b
  | Database a, Database b -> a = b
  | Malformed a, Malformed b -> String.equal a b
  | (Dek_not_found _ | Kms _ | Session _ | Database _ | Malformed _), _ -> false

let pp ppf = function
  | Dek_not_found { resource; version = None } ->
      Format.fprintf ppf "%a has no data-encryption key" Resource.pp resource
  | Dek_not_found { resource; version = Some version } ->
      Format.fprintf ppf "%a has no data-encryption key of version %d" Resource.pp
        resource version
  | Kms error -> Format.fprintf ppf "kms: %a" Ascetic_kms.Kms_error.pp error
  | Session error ->
      Format.fprintf ppf "session: %a" Ascetic_session.Session_error.pp error
  | Database reason ->
      Format.fprintf ppf "database: %a" Ascetic_session.Driver_error.pp reason
  | Malformed what -> Format.fprintf ppf "malformed: %s" what

let to_string error = Format.asprintf "%a" pp error
