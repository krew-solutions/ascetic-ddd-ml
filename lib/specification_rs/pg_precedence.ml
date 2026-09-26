(* How tightly PostgreSQL's operators bind, and to which side: what decides where the
   compiled text needs parentheses to mean what the tree means.

   The numbers are the rows of the table in the PostgreSQL manual, "Operator Precedence",
   bottom up; only their order matters. *)

(* To which side a run of operators of one precedence groups. *)
type associativity =
  | Left  (** [a - b - c] is [(a - b) - c]. *)
  | Right
      (** [a ^ b ^ c] would be [a ^ (b ^ c)]. No infix operator of a specification groups
          so; it is the side a left-grouping one leaves. *)
  | Non  (** [a = b = c] is not PostgreSQL at all. *)

(* Of what needs no parentheses wherever it stands: a column, a parameter, [EXISTS (...)]. *)
let atom = 255

(* Of [::], the row above every operator: what an operand is parenthesised against before
   its type is said. *)
let cast = 160
let prefix : Operator.prefix -> int = function Neg -> 140 | Not -> 60

let infix : Operator.infix -> int * associativity = function
  | Arithmetic (Mul | Div | Mod) -> (120, Left)
  | Arithmetic (Add | Sub) -> (110, Left)
  (* "Any other operator." *)
  | Arithmetic (Shl | Shr) -> (100, Left)
  | Comparison _ -> (80, Non)
  | Is -> (70, Non)
  | Logical And -> (50, Left)
  | Logical Or -> (40, Left)

let postfix : Operator.postfix -> int = function Is_null | Is_not_null -> 70

(* Whether regrouping a run of [op] leaves its value what it was, so that
   [a AND (b AND c)] can be written without the parentheses. True of the logical
   connectives and of nothing else: integer [+] can overflow one way and not the other,
   float [+] rounds differently. *)
let regroups : Operator.infix -> bool = function Logical _ -> true | _ -> false
