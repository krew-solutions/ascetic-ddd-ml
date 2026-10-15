type t = {
  tenant_id : string;
  purpose : Key_label.t;
  version : Key_version.t;
  key : Key.t;
}

let load ~tenant_id ~purpose ~version key = { tenant_id; purpose; version; key }

let generate ~tenant_id ~purpose ~version =
  Result.map (load ~tenant_id ~purpose ~version) (Key.random 32)

let tenant_id t = t.tenant_id
let purpose t = t.purpose
let version t = t.version
let key t = t.key

let name_of ~tenant_id purpose =
  Printf.sprintf "%s.mac.%s" tenant_id (Key_label.to_string purpose)

let name t = name_of ~tenant_id:t.tenant_id t.purpose

let mac t message =
  let tag =
    Digestif.SHA256.(to_raw_string (hmac_string ~key:(Key.to_string t.key) message))
  in
  Mac.make ~key_version:t.version tag

let pp ppf t =
  Format.fprintf ppf "Mac_key(tenant `%s`, purpose `%a`, version %a)" t.tenant_id
    Key_label.pp t.purpose Key_version.pp t.version
