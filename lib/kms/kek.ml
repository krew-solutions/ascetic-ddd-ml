type t = {
  tenant_id : string;
  context : string option;
  version : Key_version.t;
  algorithm : Algorithm.t;
  cipher : Cipher.t;
  wrapped : Wrapped_key.t;
}

let ( let* ) = Result.bind

let binding ~tenant_id context =
  match context with
  | None -> tenant_id
  | Some context ->
      let length = Bytes.create 4 in
      Bytes.set_int32_be length 0 (Int32.of_int (String.length tenant_id));
      Bytes.to_string length ^ tenant_id ^ context

let of_key ~tenant_id ~context ~version ~algorithm key wrapped =
  let* cipher = Algorithm.cipher algorithm key ~aad:(binding ~tenant_id context) in
  Ok { tenant_id; context; version; algorithm; cipher; wrapped }

let generate ~context ~tenant_id ~version ~algorithm ~wrap =
  let* key = Algorithm.generate_key algorithm in
  let* wrapped = wrap key in
  of_key ~tenant_id ~context ~version ~algorithm key wrapped

let load ~context ~tenant_id ~version ~algorithm ~unwrap wrapped =
  let* key = unwrap wrapped in
  of_key ~tenant_id ~context ~version ~algorithm key wrapped

let tenant_id t = t.tenant_id
let context t = t.context
let version t = t.version
let algorithm t = t.algorithm
let wrapped t = t.wrapped
let wrap t dek = Wrapped_key.wrap t.version t.cipher dek
let unwrap t wrapped = Wrapped_key.unwrap wrapped t.version t.cipher

let rewrap t ~from wrapped =
  let* dek = unwrap from wrapped in
  wrap t dek

let generate_dek t =
  let* dek = t.cipher.generate_key () in
  let* wrapped = wrap t dek in
  Ok (dek, wrapped)

let pp ppf t =
  match t.context with
  | None ->
      Format.fprintf ppf "Kek(tenant `%s`, version %a, %a)" t.tenant_id Key_version.pp
        t.version Algorithm.pp t.algorithm
  | Some context ->
      Format.fprintf ppf "Kek(tenant `%s`, context `%s`, version %a, %a)" t.tenant_id
        context Key_version.pp t.version Algorithm.pp t.algorithm
