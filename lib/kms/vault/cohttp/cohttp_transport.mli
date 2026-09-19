(** {!Ascetic_kms_vault.Vault_transport.S} over a [cohttp-eio] client.

    Built only where [cohttp-eio] is installed. Whether the client speaks TLS is the
    application's choice, the [https] argument the client is made with. Each call opens a
    connection and closes it when the answer is read: the client keeps none. *)

include Ascetic_kms_vault.Vault_transport.S with type client = Cohttp_eio.Client.t

val max_answer : int
(** The longest answer read, a mebibyte: Transit's answers are a few hundred bytes. *)
