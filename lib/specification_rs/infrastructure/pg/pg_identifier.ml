(* A name, as the text of a query has it: between double quotes.

   A word PostgreSQL knows is read as what PostgreSQL knows, and a member of an aggregate
   may be called anything: [user] without quotes is the session's user, so [user = $1]
   parses and selects other rows than were asked for, and [order] does not parse. Which
   words these are depends on the server's version, which a library does not know; so no
   name is looked up in a list, and every name is quoted.

   Between quotes a name is the column's name to the letter: ["createdAt"] is the column
   created as ["createdAt"], which [createdAt] without quotes is not - PostgreSQL folds
   that to [createdat]. What a member of the domain is called in the storage is for a
   [Mapping] to say.

   Two things keep SQL of a tree's own out of the text, and neither rests on the other: a
   name is of ASCII letters, digits and [_] or it is refused, and a double quote inside a
   name is doubled, as PostgreSQL reads it. *)

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'

(* [name], if it is of ASCII letters, digits and [_] and does not start with a digit. *)
let plain name =
  let valid =
    String.length name > 0
    && (is_alpha name.[0] || name.[0] = '_')
    && String.for_all (fun c -> is_alpha c || is_digit c || c = '_') name
  in
  if valid then Ok name else Error (Pg_error.Invalid_identifier name)

(* [name] between double quotes, a double quote of its own written twice. *)
let quoted name =
  let doubled = String.concat "\"\"" (String.split_on_char '"' name) in
  "\"" ^ doubled ^ "\""

(* [name], checked and quoted. *)
let identifier name = Result.map quoted (plain name)
let ( let* ) = Result.bind

let rec traverse f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = traverse f xs in
      Ok (y :: ys)

(* A table's name, which may carry its schema: [public.items] is ["public"."items"]. *)
let qualified name =
  Result.map (String.concat ".") (traverse identifier (String.split_on_char '.' name))
