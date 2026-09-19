module Transport = Ascetic_kms_vault.Vault_transport

type client = Cohttp_eio.Client.t

let max_answer = 1 lsl 20

let send client (request : Transport.request) =
  match
    Eio.Switch.run @@ fun sw ->
    let headers =
      Http.Header.of_list
        (("X-Vault-Token", request.token)
        ::
        (if Option.is_some request.body then [ ("Content-Type", "application/json") ]
         else []))
    in
    let body =
      Option.map
        (fun json -> Cohttp_eio.Body.of_string (Yojson.Safe.to_string json))
        request.body
    in
    let response, answer =
      Cohttp_eio.Client.call ~sw client ~headers ?body
        (Http.Method.of_string request.meth)
        (Uri.of_string request.url)
    in
    let status = Http.Status.to_int (Http.Response.status response) in
    let text = Eio.Buf_read.(parse_exn take_all) answer ~max_size:max_answer in
    (status, String.trim text)
  with
  | status, "" -> Ok { Transport.status; body = None }
  | status, text -> (
      match Yojson.Safe.from_string text with
      | json -> Ok { Transport.status; body = Some json }
      | exception Yojson.Json_error reason ->
          Error (Printf.sprintf "the answer is not JSON: %s" reason))
  (* A cancellation is not the transport's to report. *)
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn -> Error (Printexc.to_string exn)
