------------------------------ MODULE Cancel ------------------------------
(***************************************************************************)
(* The cancelling of a subscription of `lib/bus`, as a protocol (ADR-0016): *)
(* a subscription cancelled is a handler that is not running and will not  *)
(* be run.                                                                 *)
(*                                                                         *)
(* A transport calls the handler from fibers of its own, the callers.  A   *)
(* call is admitted, counted in flight, while the handler is attached, and *)
(* counted out when the handler has returned.  Cancelling detaches the     *)
(* handler, once, then waits until nothing is in flight; a handler may     *)
(* cancel its own subscription from inside a call, and then does not wait. *)
(* Several fibers may cancel at once.                                      *)
(*                                                                         *)
(* A step of this model is what the implementation does under one lock, or *)
(* without giving way.  Four constants take one such step apart each, as   *)
(* the implementation would be without the lock or the wait in question,   *)
(* and TLC finds what goes wrong:                                          *)
(*                                                                         *)
(*   AdmitUnderLock     FALSE: whether the handler is attached is read, and *)
(*                      the call counted, in two steps: a call is made     *)
(*                      after a cancel has returned;                       *)
(*   WaitForDetach      FALSE: a cancel that finds the detaching taken by  *)
(*                      another goes on to wait at once, as first written: *)
(*                      it returns while the handler is still attached;    *)
(*   RegisterUnderLock  FALSE: nothing in flight is read, and the waiter   *)
(*                      registered, in two steps: the wake is lost;        *)
(*   InsideMark         FALSE: a handler cancelling itself waits like      *)
(*                      anybody else: for itself, for ever.                *)
(*                                                                         *)
(* A subscription served by a loop of its own, Handling.loop, is the case  *)
(* of one call admitted before anybody can cancel, and a detaching that    *)
(* takes no lock.                                                          *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
  Callers,            \* the transport's fibers that call the handler
  Cancellers,         \* fibers that cancel from outside a call
  Messages,           \* how many calls the transport attempts in all
  AdmitUnderLock,
  WaitForDetach,
  RegisterUnderLock,
  InsideMark

ASSUME Callers \cap Cancellers = {}

Actors == Callers \cup Cancellers

VARIABLES
  attached,  \* the handler is the group's: what a caller reads before a call
  detach,    \* "Attached", "Detaching" by one canceller, "Detached"
  inFlight,  \* calls admitted and not yet counted out
  waiting,   \* actors registered to be woken when nothing is in flight
  woken,     \* actors woken
  taken,     \* calls attempted so far
  call,      \* per caller: "Idle", "Check", "Admitting", "Run", "Leave"
  cancel     \* per actor: "Idle", "Detaching", "AwaitDetach", "Quiesce", "Registering", "Waiting", "Done"

vars == <<attached, detach, inFlight, waiting, woken, taken, call, cancel>>

Init ==
  /\ attached = TRUE
  /\ detach = "Attached"
  /\ inFlight = 0
  /\ waiting = {}
  /\ woken = {}
  /\ taken = 0
  /\ call = [d \in Callers |-> "Idle"]
  /\ cancel = [a \in Actors |-> "Idle"]

(* ------------------------------------------------------------------------ *)
(* A call of the handler                                                      *)

\* The transport has a message for the handler.
Take(d) ==
  /\ call[d] = "Idle" /\ taken < Messages
  /\ taken' = taken + 1
  /\ call' = [call EXCEPT ![d] = "Check"]
  /\ UNCHANGED <<attached, detach, inFlight, waiting, woken, cancel>>

\* Under the lock handlers are detached under: the call is counted only if
\* the handler is still attached.  Without the lock the count comes later.
Check(d) ==
  /\ call[d] = "Check"
  /\ IF ~attached
       THEN call' = [call EXCEPT ![d] = "Idle"] /\ UNCHANGED inFlight
       ELSE IF AdmitUnderLock
              THEN inFlight' = inFlight + 1 /\ call' = [call EXCEPT ![d] = "Run"]
              ELSE call' = [call EXCEPT ![d] = "Admitting"] /\ UNCHANGED inFlight
  /\ UNCHANGED <<attached, detach, waiting, woken, taken, cancel>>

Admit(d) ==
  /\ call[d] = "Admitting"
  /\ inFlight' = inFlight + 1
  /\ call' = [call EXCEPT ![d] = "Run"]
  /\ UNCHANGED <<attached, detach, waiting, woken, taken, cancel>>

\* The handler returns; not from the middle of a cancel of its own.
Return(d) ==
  /\ call[d] = "Run" /\ cancel[d] \in {"Idle", "Done"}
  /\ call' = [call EXCEPT ![d] = "Leave"]
  /\ UNCHANGED <<attached, detach, inFlight, waiting, woken, taken, cancel>>

\* The call is counted out, under the lock of the count; when nothing is left
\* in flight, everybody registered is woken.
Leave(d) ==
  /\ call[d] = "Leave"
  /\ inFlight' = inFlight - 1
  /\ IF inFlight' = 0
       THEN woken' = woken \cup waiting /\ waiting' = {}
       ELSE UNCHANGED <<woken, waiting>>
  /\ call' = [call EXCEPT ![d] = "Idle"]
  /\ cancel' = [cancel EXCEPT ![d] = "Idle"]
  /\ UNCHANGED <<attached, detach, taken>>

(* ------------------------------------------------------------------------ *)
(* A cancel, by a canceller, or by a caller from inside its call              *)

MayCancel(a) == IF a \in Cancellers THEN TRUE ELSE call[a] = "Run"

\* The detaching is taken by one; whoever finds it taken waits for it to be
\* over before going on, or, as first written, does not.
Begin(a) ==
  /\ MayCancel(a) /\ cancel[a] = "Idle"
  /\ IF detach = "Attached"
       THEN detach' = "Detaching" /\ cancel' = [cancel EXCEPT ![a] = "Detaching"]
       ELSE /\ UNCHANGED detach
            /\ cancel' = [cancel EXCEPT ![a] =
                 IF detach = "Detaching" /\ WaitForDetach THEN "AwaitDetach" ELSE "Quiesce"]
  /\ UNCHANGED <<attached, inFlight, waiting, woken, taken, call>>

\* Under the lock calls are admitted under.  Getting the lock is a wait, which
\* is why this is a step of its own.
Detach(a) ==
  /\ cancel[a] = "Detaching"
  /\ attached' = FALSE
  /\ detach' = "Detached"
  /\ cancel' = [cancel EXCEPT ![a] = "Quiesce"]
  /\ UNCHANGED <<inFlight, waiting, woken, taken, call>>

Awaited(a) ==
  /\ cancel[a] = "AwaitDetach" /\ detach = "Detached"
  /\ cancel' = [cancel EXCEPT ![a] = "Quiesce"]
  /\ UNCHANGED <<attached, detach, inFlight, waiting, woken, taken, call>>

\* From inside a call there is nothing to wait for.  From outside, under the
\* lock of the count: nothing in flight, or registered to be woken.
Quiesce(a) ==
  /\ cancel[a] = "Quiesce"
  /\ IF (a \in Callers /\ InsideMark) \/ inFlight = 0
       THEN cancel' = [cancel EXCEPT ![a] = "Done"] /\ UNCHANGED waiting
       ELSE IF RegisterUnderLock
              THEN waiting' = waiting \cup {a} /\ cancel' = [cancel EXCEPT ![a] = "Waiting"]
              ELSE cancel' = [cancel EXCEPT ![a] = "Registering"] /\ UNCHANGED waiting
  /\ UNCHANGED <<attached, detach, inFlight, woken, taken, call>>

Register(a) ==
  /\ cancel[a] = "Registering"
  /\ waiting' = waiting \cup {a}
  /\ cancel' = [cancel EXCEPT ![a] = "Waiting"]
  /\ UNCHANGED <<attached, detach, inFlight, woken, taken, call>>

Wake(a) ==
  /\ cancel[a] = "Waiting" /\ a \in woken
  /\ cancel' = [cancel EXCEPT ![a] = "Done"]
  /\ UNCHANGED <<attached, detach, inFlight, waiting, woken, taken, call>>

(* ------------------------------------------------------------------------ *)

Next ==
  \/ \E d \in Callers : Take(d) \/ Check(d) \/ Admit(d) \/ Return(d) \/ Leave(d)
  \/ \E a \in Actors : Begin(a) \/ Detach(a) \/ Awaited(a) \/ Quiesce(a) \/ Register(a) \/ Wake(a)

\* Everything goes on but the choice to cancel, which nobody has to make.
Fairness ==
  /\ \A d \in Callers :
       WF_vars(Take(d)) /\ WF_vars(Check(d)) /\ WF_vars(Admit(d)) /\ WF_vars(Return(d)) /\ WF_vars(Leave(d))
  /\ \A a \in Actors :
       WF_vars(Detach(a)) /\ WF_vars(Awaited(a)) /\ WF_vars(Quiesce(a)) /\ WF_vars(Register(a)) /\ WF_vars(Wake(a))

Spec == Init /\ [][Next]_vars /\ Fairness

(* ------------------------------------------------------------------------ *)
(* Properties                                                                 *)

TypeOK ==
  /\ attached \in BOOLEAN
  /\ detach \in {"Attached", "Detaching", "Detached"}
  /\ inFlight \in 0..Cardinality(Callers)
  /\ waiting \subseteq Actors /\ woken \subseteq Actors
  /\ taken \in 0..Messages
  /\ call \in [Callers -> {"Idle", "Check", "Admitting", "Run", "Leave"}]
  /\ cancel \in [Actors -> {"Idle", "Detaching", "AwaitDetach", "Quiesce", "Registering", "Waiting", "Done"}]

\* The count is of the calls admitted and not yet counted out.
CountIsOfCalls == inFlight = Cardinality({d \in Callers : call[d] \in {"Run", "Leave"}})

\* A subscription cancelled is a handler that is not running and will not be
\* run: once a cancel from outside has returned, no call is in flight, ever.
Quiet ==
  \A c \in Cancellers :
    cancel[c] = "Done" => inFlight = 0 /\ \A d \in Callers : call[d] # "Run"

\* Whoever is registered to be woken has something to wait for.
NoLostWake == waiting # {} => inFlight > 0

\* A handler returns, one that cancels its own subscription included.
HandlerReturns == \A d \in Callers : (call[d] = "Run") ~> (call[d] # "Run")

\* A cancel returns, since handlers do.
CancelReturns == \A c \in Cancellers : (cancel[c] # "Idle") ~> (cancel[c] = "Done")

=============================================================================
