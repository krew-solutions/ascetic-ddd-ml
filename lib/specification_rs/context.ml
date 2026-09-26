type error =
  | Missing of string
  | Not_a_value of string
  | Not_an_object of string
  | Not_a_collection of string
[@@deriving show { with_path = false }, eq]

let error_to_string = function
  | Missing name -> Printf.sprintf "key '%s' not found" name
  | Not_a_value name -> Printf.sprintf "'%s' is not a value" name
  | Not_an_object name -> Printf.sprintf "'%s' is not an object" name
  | Not_a_collection name -> Printf.sprintf "'%s' is not a collection" name

type 'v t = {
  field : string -> ('v, error) result;
  object_ : string -> ('v t, error) result;
  collection : string -> ('v t list, error) result;
}
