(** {!Ascetic_specification.Value} as a parameter of Caqti, so that the parameters of a
    compiled {!Ascetic_specification.Pg.query} go to the driver as they are.

    The reference's driver sends no type with a parameter: the server infers each from
    what stands beside it - ["age" >= $1] makes [$1] whatever [age] is - and the value is
    then written in the type inferred. Caqti's PostgreSQL driver sends a type with every
    typed field: an integer as [int8], a point in time as [timestamptz]. Beside a column
    that takes away what the compiler counts on - ["at" = $1::timestamptz] of a column
    without zone is compared in the session's time zone, ["a" << $1::int8] is "operator
    does not exist" - so here every value goes as a text of no declared type, which the
    server reads in the type it inferred: [42] as an integer of the column's width, or a
    float, or a numeric; [2023-11-14T22:13:20Z] as a timestamp with zone or without. The
    compiler says the type in the text where the server has nothing to infer it from.

    What this gives up: the driver does not refuse a value of the wrong kind. A text
    ["40"] where an integer is asked for is read as the integer it spells, where the
    reference's driver refuses it; a text that does not spell one is the server's error,
    as it is there. A text with a NUL in it is refused here: PostgreSQL has no such text,
    and a C string would end at the NUL in silence. *)

(** A parameter tuple and its Caqti type, packed: the values of one query, however many.
*)
type t = T : 'a Caqti_type.t * 'a -> t

(** A text with a NUL (U+0000) in it: no text PostgreSQL has. *)
type error = Nul_in_text [@@deriving show, eq]

val text : Ascetic_specification.Value.t -> (string option, error) result
(** How a value is written for the server: none for the null; a boolean as [true] or
    [false]; an integer as its digits; a float as seventeen significant digits, or [NaN],
    [Infinity], [-Infinity]; a text as it is; a point in time as
    [YYYY-MM-DDTHH:MM:SS.ffffffZ], with [BC] after it before year 1; a span of time as
    microseconds. *)

val of_values : Ascetic_specification.Value.t list -> (t, error) result
(** The values as one parameter tuple, in order: [$1] is the first. *)

(** A request and its parameters, packed: what a connection runs with
    [C.exec request params], [C.collect_list request params] and their kin. *)
type ('b, 'm) request = R : ('a, 'b, 'm) Caqti_request.t * 'a -> ('b, 'm) request

val request :
  ?oneshot:bool ->
  'b Caqti_type.t ->
  'm Caqti_mult.t ->
  string ->
  Ascetic_specification.Value.t list ->
  (('b, 'm) request, error) result
(** [request row mult sql params]: the query [sql], whose parameters are [$1], [$2], ...
    as {!Ascetic_specification.Pg.compile} numbers them, with [params] for them, reading
    rows of type [row] with multiplicity [mult]. [oneshot] is Caqti's: not prepared for
    reuse, which is right for a query whose text depends on the specification. *)
