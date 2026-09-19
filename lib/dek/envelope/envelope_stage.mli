(** The envelope stage of the bus: a DEK per message, wrapped through the KMS, carried in
    a header.

    On the way out the stage draws a fresh DEK for the message's tenant from the KMS,
    seals the payload with it, and puts the DEK, wrapped by the tenant's KEK, in the [dek]
    header, base64, with the cipher's name in [dek_algorithm]. On the way in it reads the
    header back, unwraps the DEK through the KMS, and opens the payload. Nothing between
    the two ends, the outbox, a broker, the inbox, holds a payload in the clear, and the
    receiving side needs no table of keys, only a KMS that holds the tenant's KEK
    (ADR-0001). This is the envelope of Tink's [KmsEnvelopeAead] and of the AWS Encryption
    SDK, with the wrapped key in a header rather than in front of the ciphertext.

    The stage reaches the KMS through a session pool of its own: the KMS's session is not
    the data's, Vault's is HTTP and a PostgreSQL KMS may be in another database, and a key
    drawn for a message that is then rolled back rests nowhere. A fresh DEK per message is
    one KMS call per message; where that is a network round trip, {!Make.reusing} keeps a
    tenant's DEK for so many messages or so long, as the AWS Encryption SDK's caching
    materials manager does, and the opening side keeps what it unwraps with
    {!Ascetic_kms.Cached} in front of its KMS.

    The payload is bound to the message's identity: its associated data is the canonical
    text of the headers the stage is bound to, the tenant and the [message_id] unless told
    otherwise, so a payload moved under another message's headers does not open, while the
    same message delivered again does. The names go along in the [dek_bound_to] header,
    and the opening side reads them from there, so the two sides cannot disagree; changing
    that header helps nobody, since the associated data was fixed at sealing. The tenant's
    id is the [tenant_id] header, and names the KEK.

    A message missing a header it is bound to cannot be sealed, and one that arrives
    without one, or whose key or payload will not open, or whose tenant's KEK is gone, is
    refused for good: [Ascetic_bus.Failure.Permanent], which the inbox parks at once. A
    KMS that cannot be reached is a failure of the moment, and the message is tried again.
*)

val tenant_id : string
(** The header naming the tenant, part of the associated data of the payload: [tenant_id].
*)

val dek : string
(** The header carrying the message's DEK, wrapped by the tenant's KEK, base64: [dek]. *)

val dek_algorithm : string
(** The header naming the cipher the payload is sealed with: [dek_algorithm]. *)

val dek_bound_to : string
(** The header naming the headers the payload is bound to, comma-separated:
    [dek_bound_to]. *)

val message_id : string
(** The header carrying the message's identity across the bus: [message_id]. *)

(** The stage over a KMS, reached through sessions of a pool. *)
module Make
    (Sessions : Ascetic_session.Session_pool.S)
    (Kms : Ascetic_kms.Kms_port.S with type session = Sessions.session) : sig
  type t

  val create :
    ?algorithm:Ascetic_kms.Algorithm.t ->
    ?bound_to:string list ->
    Sessions.t ->
    Kms.t ->
    t
  (** A stage sealing with AES-256-GCM, drawing and unwrapping DEKs through the KMS in
      sessions of the pool, binding every payload to the message's [tenant_id] and
      [message_id], unless told otherwise. What was sealed before opens with the cipher
      and under the binding its headers name. [bound_to] names the headers, in this order:
      a message without one of them is not sealed. A header's value is bound as the text
      it travels as, so bind to headers whose text is stable on the way, not to ones the
      channels structure and print again. *)

  val reusing : t -> clock:_ Eio.Time.Mono.t -> Reuse.t -> t
  (** The same stage sealing a tenant's messages under one DEK for as long as the reuse
      allows, instead of a fresh one per message. The wrapped key travels in every message
      as before; the opening side needs no change, and with a cache in front of its KMS
      unwraps it once. *)

  val outbound :
    t -> Ascetic_bus.Message.t -> (Ascetic_bus.Message.t, Ascetic_bus.Failure.t) result
  (** Seals the message. *)

  val inbound :
    t -> Ascetic_bus.Message.t -> (Ascetic_bus.Message.t, Ascetic_bus.Failure.t) result
  (** Opens the message, and takes the three key headers off. *)

  val stage : t -> Ascetic_bus.Stage.t
  (** The stage as the bus takes it: what [through] is given. *)
end
