type t = {
  tenant_id : string;
  version : Key_version.t;
  algorithm : Algorithm.t;
  cipher : Cipher.t;
  wrapped : Wrapped_key.t;
}

let ( let* ) = Result.bind

let of_key ~tenant_id ~version ~algorithm key wrapped =
  let* cipher = Algorithm.cipher algorithm key ~aad:tenant_id in
  Ok { tenant_id; version; algorithm; cipher; wrapped }

let generate ~tenant_id ~version ~algorithm ~wrap =
  let* key = Algorithm.generate_key algorithm in
  let* wrapped = wrap key in
  of_key ~tenant_id ~version ~algorithm key wrapped

let load ~tenant_id ~version ~algorithm ~unwrap wrapped =
  let* key = unwrap wrapped in
  of_key ~tenant_id ~version ~algorithm key wrapped

let tenant_id t = t.tenant_id
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
  Format.fprintf ppf "Kek(tenant `%s`, version %a, %a)" t.tenant_id Key_version.pp
    t.version Algorithm.pp t.algorithm
