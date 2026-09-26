module Foreign_key = struct
  type t = {
    name : string option;
    table : string;
    columns : string list;
    referenced_table : string;
    referenced_columns : string list;
  }
  [@@deriving show { with_path = false }, eq]

  let make table column referenced_table referenced_column =
    {
      name = None;
      table;
      columns = [ column ];
      referenced_table;
      referenced_columns = [ referenced_column ];
    }

  let and_ column referenced_column key =
    {
      key with
      columns = key.columns @ [ column ];
      referenced_columns = key.referenced_columns @ [ referenced_column ];
    }

  let named name key = { key with name = Some name }

  let name key =
    match key.name with
    | Some name -> name
    | None ->
        let table =
          match List.rev (String.split_on_char '.' key.table) with
          | last :: _ -> last
          | [] -> key.table
        in
        Printf.sprintf "%s_%s_fkey" table (String.concat "_" key.columns)

  let table key = key.table
  let columns key = key.columns
  let referenced_table key = key.referenced_table
  let referenced_columns key = key.referenced_columns
end

type t = {
  table : string;
  alias : string option;
  keys : Foreign_key.t list;
  composites : (string * string) list;
}
[@@deriving show { with_path = false }, eq]

let make table = { table; alias = None; keys = []; composites = [] }
let alias alias schema = { schema with alias = Some alias }
let key key schema = { schema with keys = schema.keys @ [ key ] }

let foreign_key table column referenced_table referenced_column schema =
  key (Foreign_key.make table column referenced_table referenced_column) schema

let composite table column schema =
  { schema with composites = schema.composites @ [ (table, column) ] }

let is_composite schema table column =
  List.exists
    (fun (of_, name) -> String.equal of_ table && String.equal name column)
    schema.composites

let key_named schema name =
  List.find_opt (fun key -> String.equal (Foreign_key.name key) name) schema.keys

let keys_referencing schema table referenced_table =
  List.filter
    (fun (key : Foreign_key.t) ->
      String.equal key.table table && String.equal key.referenced_table referenced_table)
    schema.keys

let keys_on schema table column =
  List.filter
    (fun (key : Foreign_key.t) ->
      String.equal key.table table && List.mem column key.columns)
    schema.keys

let table schema = schema.table
let row schema = Option.value schema.alias ~default:schema.table
