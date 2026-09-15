(** Two sessions acting as one.

    A use case that must write to two stores at once, the data generator's target database
    and its own bookkeeping, for instance, is written against one session as usual; that
    the session is a pair is a decision of the composition root. A composite is itself a
    session, so it nests like any other, and a scope on it opens a scope on each delegate:

    {v first open, second open, ... work ..., second close, first close v}

    More than two delegates nest: [Make (A) (Make (B) (C))] is a session whose handle is
    [A.t * (B.t * C.t)], taken apart by the pattern [(a, (b, c))]. A repository reaches
    the delegate it needs by position, [fst uow] or [snd uow], after pinning [type uow] to
    the product; the choice is explicit in the code, and the application layer,
    polymorphic in the session type, never sees it.

    {2 What it is not}

    It is not a distributed transaction. The inner delegate commits first; if the outer
    one then fails to commit, the two diverge, and no composition can prevent that. Work
    that must be undone across stores belongs in a saga. What the composite does guarantee
    is one failure path: an error inside the scope, or the inner delegate failing to
    commit, rolls the outer one back. So the delegate whose rollback must undo the other's
    work goes first.

    Each delegate keeps its own guard, so the composite needs none: a second scope on the
    same composite is refused by whichever delegate is asked first. Observers stay with
    the delegates. *)

module Make (A : Ascetic_session.Session.S) (B : Ascetic_session.Session.S) : sig
  type t = A.t * B.t

  include Ascetic_session.Session.S with type t := t
end
