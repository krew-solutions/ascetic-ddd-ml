(** Which errors of the database are of the moment, and which are defects.

    A loop that meets an error of the moment, a lock cycle the server broke, a
    serialization failure, a connection lost, a server shutting down or out of resources,
    waits and goes on: its transaction was rolled back, nothing was recorded, and the work
    comes back next time. Any other error is a defect, of the schema, of a statement, of a
    value, and repeating it would repeat the defect; a loop stops on it. The line is drawn
    by SQLSTATE class (PostgreSQL manual, appendix A), read from the PostgreSQL driver's
    error. An error without a SQLSTATE is of the moment when it is the connection's, and a
    defect when it is the driver's reading of a value, such as a column of an unexpected
    type. *)

val sqlstate : string -> bool
(** Whether a SQLSTATE is of the moment: connection exceptions (class [08]), insufficient
    resources (class [53]), serialization failure and deadlock detected ([40001],
    [40P01]), statement completion unknown ([40003]), and the server going down or not yet
    up ([57P01], [57P02], [57P03]). *)

val of_error : Caqti_error.t -> bool
(** Whether a Caqti error is of the moment: a statement refused with a SQLSTATE of the
    moment, a connection that failed or could not be established. A driver that could not
    be loaded, a value that could not be encoded or decoded, and a statement refused for
    any other reason are defects. *)

val driver_error : Caqti_error.t -> Ascetic_session.Driver_error.t
(** The error as the port carries it: its text, and {!of_error}. *)

val protect :
  raised:(Ascetic_session.Driver_error.t -> 'e) ->
  (unit -> ('a, 'e) result) ->
  ('a, 'e) result
(** Runs a driver call, turning an exception of the PostgreSQL client library into the
    error [raised] gives: the library raises, rather than returns, on a connection whose
    server is gone, which is of the moment; its other exceptions, a value out of range, a
    status it did not expect, are defects. Every other exception, a cancellation in
    particular, goes through. *)
