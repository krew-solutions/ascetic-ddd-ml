(** What can go wrong on the bus, as a value. *)

type t =
  | Unknown_scheme of string
      (** The URI's scheme has no registered adapter, or the URI has no scheme. *)
  | Already_registered of string  (** The scheme is already bound to an adapter. *)
  | Already_in_group of { uri : string; group : string }
      (** A second consumer joined the same [(uri, group)] on an adapter that allows one:
          a configuration bug in a monolithic deployment. *)
  | Transport of string  (** The transport failed; the text is its own. *)
  | Stage of Failure.t  (** A stage of the wire refused the message on its way out. *)

let equal (a : t) (b : t) = a = b

let pp ppf = function
  | Unknown_scheme scheme ->
      Format.fprintf ppf "no adapter registered for scheme `%s`" scheme
  | Already_registered scheme ->
      Format.fprintf ppf "scheme `%s` is already registered" scheme
  | Already_in_group { uri; group } ->
      Format.fprintf ppf "a consumer already exists in group `%s` on `%s`" group uri
  | Transport reason -> Format.fprintf ppf "transport: %s" reason
  | Stage failure -> Format.fprintf ppf "stage: %a" Failure.pp failure

let to_string error = Format.asprintf "%a" pp error
