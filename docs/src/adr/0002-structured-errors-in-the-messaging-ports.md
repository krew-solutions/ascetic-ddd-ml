# ADR-0002: Structured errors in the messaging ports

## Status

Accepted (2026-09-14). Amended (2026-09-17) by the port of the reference
outbox and inbox over the session: the cases are now `Session` of
`Session_error.t`, `Database` of `Driver_error.t`, `Subscriber of 'e` and
`Malformed`; the database and session cases carry the driver's verdict on
whether the failure is of the moment, which objection 3 below said was not
available (ADR-0009); `Caqti_error_kind` stays with `unit_of_work` only. The
inbox has no `Subscriber` case, a failing subscriber being an `Outcome.t`
(ADR-0004); the outbox keeps `'e`.

## Context

Every operation of the outbox and inbox ports returned `(_, string) result`,
the string being the rendered Caqti error or whatever the subscriber
returned. Once `run` and the iterators stopped hiding failures (they now
return the first error), the caller had to decide what to do with one, and
a string cannot be decided on: a database that went away asks for a
reconnect and a retry, a missing table asks for a person, a subscriber
that declined a message asks for a business decision. The Rust port of
these blocks carries an enum with a variant per source and a boxed cause
for the subscriber's own error.

## Decision

Each port declares

```ocaml
module Error : sig
  type 'e t =
    | Connection of string   (* no connection: retry when reachable    *)
    | Request of string      (* a statement refused or failed: look    *)
    | Malformed of string    (* a value could not be (de)coded: defect *)
    | Subscriber of 'e       (* the subscriber declined a message      *)
end
```

and every operation returns `(_, 'e Error.t) result`. The subscriber's own
error type is the parameter `'e`, so a supervisor matching `Subscriber e`
holds the subscriber's value, not its rendering. Operations without a
subscriber never produce `Subscriber`; their error type stays polymorphic
in `'e`, so it unifies with the caller's without conversion.

The three database cases come from Caqti's own tags, grouped once in
`Ascetic_unit_of_work.Caqti_error_kind`: driver loading and connecting are
`Connection`; encoding and decoding are `Malformed`; requests and responses
are `Request`. The text of the error travels alongside.

The connection provider separates acquiring a connection from using it:
`with_connection f` is `Error` only when no connection could be obtained,
and `Ok` with whatever `f` returned otherwise. The provider knows nothing
of the caller's error type; the outbox and the inbox lift an acquisition
failure into `Connection` in one place each. This is also the shape of
`Caqti_eio.Pool.use`, so `Caqti_connection_provider.of_pool` wraps a pool
in one line and the exception-based adaptation the README used to
recommend is gone.

## Objections considered

Three ways this could be wrong were looked for before it was accepted.

1. *A variant per port duplicates the type; one shared type would do.* A
   shared type would have to live in `unit_of_work`, whose concern is the
   transaction, and would carry a `Subscriber` case foreign to it; and the
   two ports may diverge, as the Rust outbox and inbox already do. The
   duplication is four constructors; composition across the two ports
   needs no conversion, because `'e` unifies. Kept per port.

2. *Polymorphic variants would compose across layers without lifting, as
   Caqti's own errors do.* They would, and at the cost of open types in
   every signature and the type errors they produce when two sets do not
   unify. The rest of this code base and the application layer it serves
   use nominal variants with explicit mapping at boundaries; the one
   lifting this design needs, of the acquisition error, happens in one
   helper per implementation. Kept nominal.

3. *`Request` conflates a transient deadlock with a permanent bad
   statement, so a supervisor still cannot decide from the constructor
   alone.* True, and no classification available at this layer separates
   them: a connection lost mid-statement also surfaces as a failed request.
   The Rust port has the same limit. `Request` therefore means "look before
   retrying", and the text is kept for the person who looks. A finer
   split, for example constraint violations from `Caqti_error.cause`, can
   be added as a case later without breaking callers that match `_`.

## Consequences

- Breaking change of both ports, of `Caqti_connection_provider.S`, and of
  the subscriber type, which is now `'e subscriber`.
- `Iter.iter` takes a subscriber that returns a result, so declining a
  message rolls the open transaction back and ends the iteration with
  `Subscriber e`, the same contract as `dispatch` and `run`.
- Test doubles of the provider return `Ok (f conn)`.
- `Unit_of_work.S.commit` still returns a string error; it is outside the
  messaging ports and left for a later decision.
