type id = Canonical.json
type t = { tenant_id : string; kind : string; id : id }

let make ~tenant_id ~kind id = { tenant_id; kind; id }
let tenant_id t = t.tenant_id
let kind t = t.kind
let id t = t.id

let canonical t =
  Canonical.to_string (`List [ `String t.tenant_id; `String t.kind; t.id ])

let id_text t = Canonical.to_string t.id
let equal a b = String.equal (canonical a) (canonical b)
let pp ppf t = Format.pp_print_string ppf (canonical t)
