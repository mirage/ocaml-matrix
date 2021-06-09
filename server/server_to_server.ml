open Lwt
open Cohttp
open Cohttp_lwt_unix
open Server_helpers

let rooter uri meth _hds request =
  let path = Uri.path uri in
  let query_args = Uri.query uri in
  match meth with
  | `OPTIONS -> (
    match Routing.option path Federation_routes.routes with
    | Some meths ->
      Logs.info (fun m ->
          m "%s '%s' : %a"
            (Code.string_of_method meth)
            (Uri.to_string uri) response_style `OK);
      let headers = make_headers ~meths () in
      Server.respond_string ~headers ~status:`OK ~body:"" ()
    | _ ->
      Logs.err (fun m ->
          m "Uri '%s' with method '%s': Not found" (Uri.to_string uri)
            (Code.string_of_method meth));
      Server.respond_not_found ())
  | _ -> (
    try
      match Routing.parse path meth Federation_routes.routes with
      | Some (_needs_auth, response) ->
        response request query_args None >>= fun (status, body, body_type) ->
        Logs.info (fun m ->
            m "%s '%s' : %a"
              (Code.string_of_method meth)
              (Uri.to_string uri) response_style status);
        let headers = make_headers ?body_type () in
        Server.respond_string ~headers ~status ~body ()
      | _ ->
        Logs.err (fun m ->
            m "Uri '%s' with method '%s': Not found" (Uri.to_string uri)
              (Code.string_of_method meth));
        Server.respond_not_found ()
    with e ->
      Logs.err (fun m ->
          m "Exception at uri '%s' with method '%s': %a" (Uri.to_string uri)
            (Code.string_of_method meth)
            Json_encoding.print_error e);
      Server.respond_not_found ())

let server =
  let callback _conn req body =
    let uri = req |> Request.uri in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    body |> Cohttp_lwt.Body.to_string >>= fun request ->
    rooter uri meth headers request in
  let crt_file_path = Unix.getenv "PWD" ^ "/server.crt" in
  let key_file_path = Unix.getenv "PWD" ^ "/server.key" in
  Server.create
    ~mode:
      (`TLS
        ( `Crt_file_path crt_file_path,
          `Key_file_path key_file_path,
          `No_password,
          `Port 8448 ))
    (Server.make ~callback ())
