(** What the Vault adapter needs of an HTTP client. *)

type request = {
  meth : string;  (** The HTTP method, upper-case. *)
  url : string;  (** The full URL, [https://vault:8200/v1/transit/keys/tenant-1]. *)
  token : string;  (** The token, for the [X-Vault-Token] header. *)
  body : Yojson.Safe.t option;  (** The JSON body, if the call has one. *)
}
(** One call to Vault. *)

type response = {
  status : int;  (** The HTTP status. *)
  body : Yojson.Safe.t option;
      (** The body as JSON; [None] for an empty body, such as a [204]. *)
}
(** What Vault answered. *)

(** An HTTP client as the Vault adapter uses it. [ascetic_ddd.kms.vault.cohttp] implements
    it over [cohttp-eio]; any other client is a few lines in the application. *)
module type S = sig
  type client

  val send : client -> request -> (response, string) result
  (** Sends the request and reads the whole answer. A status Vault chose, whatever it is,
      is a response; only not reaching Vault, or an answer that is not JSON, is an error,
      and the text says which. *)
end
