type t = { key : Key.t; algorithm : Algorithm.t }

let ( let* ) = Result.bind
let version = Key_version.first

(* The master key as the tenant sees it: bound to the tenant as associated
   data. *)
let cipher_for t ~tenant_id = Algorithm.cipher t.algorithm t.key ~aad:tenant_id

let make key algorithm =
  let* _ = Algorithm.cipher algorithm key ~aad:"" in
  Ok { key; algorithm }

let algorithm t = t.algorithm

let wrap t ~tenant_id key =
  let* cipher = cipher_for t ~tenant_id in
  Wrapped_key.wrap version cipher key

let unwrap t ~tenant_id wrapped =
  let* cipher = cipher_for t ~tenant_id in
  Wrapped_key.unwrap wrapped version cipher

let make_kek ?context t ~tenant_id ~version =
  Kek.generate ~context ~tenant_id ~version ~algorithm:t.algorithm
    ~wrap:(wrap t ~tenant_id)

let generate_kek ?context t ~tenant_id =
  make_kek ?context t ~tenant_id ~version:Key_version.first

let load_kek ?context t ~tenant_id ~version ~algorithm wrapped =
  Kek.load ~context ~tenant_id ~version ~algorithm ~unwrap:(unwrap t ~tenant_id) wrapped

let rotate_kek t kek =
  let* version = Key_version.next (Kek.version kek) in
  make_kek ?context:(Kek.context kek) t ~tenant_id:(Kek.tenant_id kek) ~version

let pp ppf t = Format.fprintf ppf "Master_key(%a)" Algorithm.pp t.algorithm
