(** A stage of the wire: what a message goes through between the typed layer and the
    transport.

    A producer encodes a value into a message, then the message goes out through its
    stages, in order; a consumer's message comes in through the same stages, in reverse,
    before it is decoded. Sealing is one stage, placed exactly here so that nothing
    between the two ends sees a payload in the clear (ADR-0001); compression could be
    another. The bus knows nothing of what a stage does: bytes in, bytes out, headers if
    the stage needs them.

    A stage's failure is the message's. Outbound, the publish fails and the caller's
    transaction with it. Inbound, the message is not decoded and not skipped: the handling
    fails, and a transport that can, redelivers: a key service away for a moment must not
    lose a message. A failure that no retry can mend is {!Failure.Permanent}. *)

type t = {
  outbound : Message.t -> (Message.t, Failure.t) result;
      (** The message as it goes out, transformed. *)
  inbound : Message.t -> (Message.t, Failure.t) result;
      (** The message as it came in, transformed back. *)
}

val outbound : t list -> Message.t -> (Message.t, Failure.t) result
(** Through every stage, first to last. *)

val inbound : t list -> Message.t -> (Message.t, Failure.t) result
(** Back through every stage, last to first. *)
