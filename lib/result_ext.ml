(** Railway-Oriented Programming extensions for [Result]. *)

(* Binding operators *)

let ( let* ) = Result.bind

let ( let+ ) r f = Result.map f r

let ( and* ) r1 r2 =
  match r1, r2 with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, _ | _, Error e -> Error e

let ( and+ ) = ( and* )

(* Combinators *)

let map_error f = function Ok x -> Ok x | Error e -> Error (f e)

let flat_map f r = Result.bind r f

let traverse f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> (
        match f x with Ok y -> go (y :: acc) rest | Error _ as e -> e)
  in
  go [] xs

let sequence rs = traverse Fun.id rs

let or_else r f = match r with Ok _ as ok -> ok | Error _ -> f ()

let tap f r =
  (match r with Ok x -> f x | Error _ -> ());
  r

let tap_error f r =
  (match r with Ok _ -> () | Error e -> f e);
  r

let to_option = function Ok x -> Some x | Error _ -> None

let of_option ~error = function Some x -> Ok x | None -> Error error

let guard condition ~error = if condition then Ok () else Error error
