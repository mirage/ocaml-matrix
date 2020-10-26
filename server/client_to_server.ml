open Lwt
open Cohttp
open Cohttp_lwt_unix
open Store
open Server_helpers

let is_logged = function
  | None -> Lwt.return_ok false
  | Some token ->
    Store.exists store Key.(v "tokens" / token) >>=
    (function
      | Error _ -> Lwt.return_error ()
      | Ok None -> Lwt.return_ok false
      | Ok (Some _) -> Lwt.return_ok true)

let get_auth_token headers uri =
  Option.fold
    ~some:
      (fun x ->
        match String.split_on_char ' ' x with
        | [b; t] when b = "Bearer" -> Some t
        | _ -> None)
    ~none:(Uri.get_query_param uri "access_token")
    (Header.get headers "Authorization")

let rooter uri meth hds request =
  let path = Uri.path uri in
  let query_args = Uri.query uri in
  let auth_token = get_auth_token hds uri in
  match meth with
  | `OPTIONS ->
    (match Routing.option path Routes.routes with
    | Some meths ->
      Logs.info (fun m -> m "%s '%s' : %a" (Code.string_of_method meth) (Uri.to_string uri) response_style `OK);
      let headers = make_headers ~meths () in
      Server.respond_string ~headers ~status:`OK ~body:"" ()
    | _ ->
        Logs.err (fun m -> m "Uri '%s' with method '%s': Not found" (Uri.to_string uri) (Code.string_of_method meth));
        Server.respond_not_found ())
  | _ ->
    try
      match Routing.parse path meth Routes.routes with
      | Some (need_auth, response) ->
        if need_auth then
          is_logged auth_token >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, Endpoint.error "M_UNKNOWN" "Internal storage failure", None)
            | Ok false -> Lwt.return (`Forbidden, Endpoint.error "M_FORBIDDEN" "", None)
            | Ok true -> response request query_args auth_token) >>=
          (fun (status, body, body_type) ->
            Logs.info (fun m -> m "%s '%s' : %a" (Code.string_of_method meth) (Uri.to_string uri) response_style status);
            let headers = make_headers ?body_type () in
            Server.respond_string ~headers ~status ~body ())
        else
          response request query_args None >>=
            (fun (status, body, body_type) ->
              Logs.info (fun m -> m "%s '%s' : %a" (Code.string_of_method meth) (Uri.to_string uri) response_style status);
              let headers = make_headers ?body_type () in
              Server.respond_string ~headers ~status ~body ())
      | _ ->
        Logs.err (fun m -> m "Uri '%s' with method '%s': Not found" (Uri.to_string uri) (Code.string_of_method meth));
        Server.respond_not_found ()
    with
      | e ->
        Logs.err (fun m -> m "Exception at uri '%s' with method '%s': %a" (Uri.to_string uri) (Code.string_of_method meth) Json_encoding.print_error e);
        Server.respond_not_found ()

let server =
  let callback _conn req body =
    let uri = req |> Request.uri in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    body |> Cohttp_lwt.Body.to_string >>= (fun request ->
      rooter uri meth headers request)
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
