open Lwt
open Cmdliner
open Cohttp
open Cohttp_lwt_unix
open Store

let make_headers ?(meths=[`GET; `POST; `PUT; `DELETE; `OPTIONS]) () =
  let headers = Header.init () in
  let headers = Header.add headers "Content-Type" "application/json" in
  let headers = Header.add headers "Access-Control-Allow-Origin" "*" in
  let meths = List.map Code.string_of_method meths in
  let meths = Fmt.to_to_string (Fmt.list ~sep:(Fmt.any ",") (Fmt.string)) meths in
  let headers = Header.add headers "Access-Control-Allow-Methods" meths in
  let headers = Header.add headers "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept, Authorization" in
  headers

let get_auth_token headers uri =
  Option.fold
    ~some:
      (fun x ->
        match String.split_on_char ' ' x with
        | [b; t] when b = "Bearer" -> Some t
        | _ -> None)
    ~none:(Uri.get_query_param uri "access_token")
    (Header.get headers "Authorization")

let is_logged = function
  | None -> Lwt.return_ok false
  | Some token ->
    Store.exists store Key.(v "tokens" / token) >>=
    (function
      | Error _ -> Lwt.return_error ()
      | Ok None -> Lwt.return_ok false
      | Ok (Some _) -> Lwt.return_ok true)

let response_style ppf status =
  match status with
  | `Code _ -> Fmt.pf ppf "%s" (Cohttp.Code.string_of_status status)
  | #Code.informational_status -> Fmt.pf ppf "%a" Fmt.(styled `Blue (styled `Bold string)) (Cohttp.Code.string_of_status status)
  | #Code.success_status -> Fmt.pf ppf "%a" Fmt.(styled `Green (styled `Bold string)) (Cohttp.Code.string_of_status status)
  | #Code.redirection_status -> Fmt.pf ppf "%a" Fmt.(styled `Cyan (styled `Bold string)) (Cohttp.Code.string_of_status status)
  | #Code.client_error_status -> Fmt.pf ppf "%a" Fmt.(styled `Red (styled `Bold string)) (Cohttp.Code.string_of_status status)
  | #Code.server_error_status -> Fmt.pf ppf "%a" Fmt.(styled `Magenta (styled `Bold string)) (Cohttp.Code.string_of_status status)

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
            | Error _ -> Lwt.return (`Internal_server_error, Endpoint.error "M_UNKNOWN" "Internal storage failure")
            | Ok false -> Lwt.return (`Forbidden, Endpoint.error "M_FORBIDDEN" "")
            | Ok true -> response request query_args auth_token) >>=
          (fun (status, body) ->
            Logs.info (fun m -> m "%s '%s' : %a" (Code.string_of_method meth) (Uri.to_string uri) response_style status);
            let headers = make_headers () in
            Server.respond_string ~headers ~status ~body ())
        else
          response request query_args None >>=
            (fun (status, body) ->
              Logs.info (fun m -> m "%s '%s' : %a" (Code.string_of_method meth) (Uri.to_string uri) response_style status);
              let headers = make_headers () in
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
  (* let crt_file_path =
    Unix.getenv "PWD" ^ "/server.crt"
  in
  let key_file_path =
    Unix.getenv "PWD" ^ "/server.key"
  in
  Server.create ~mode:(`TLS (`Crt_file_path crt_file_path, `Key_file_path key_file_path, `No_password, `Port 8000)) (Server.make ~callback ()) *)
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let main () =
  ignore (Lwt_main.run server)

let setup level full total log_file =
  (* let style_renderer = `None in *)
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level level;
  (match log_file with
  | None ->
    Logs.set_reporter (Logs_fmt.reporter ())
  | Some log_file ->
    let dst_channel = open_out log_file in
    let dst = Format.formatter_of_out_channel dst_channel in
    Logs.set_reporter (Logs_fmt.reporter ~dst ()));
  if not total then
    (List.filter
      (fun src ->
        let src = Logs.Src.name src in
        let src_hd = String.split_on_char '.' src |> List.hd in
        List.exists (fun s -> String.equal src_hd s) ["irmin"; "git"; "git-unix"; "decompress"]) (Logs.Src.list ()) |>
    List.iter (fun src -> Logs.Src.set_level src (Some Logs.Info));
    if not full then
      List.filter
        (fun src ->
          let src = Logs.Src.name src in
          let src_hd = String.split_on_char '.' src |> List.hd in
          List.exists (fun s -> not (String.equal src_hd s)) ["application"]) (Logs.Src.list ()) |>
      List.iter (fun src -> Logs.Src.set_level src (Some Logs.Info));)

let full =
  Arg.(Arg.value & (Arg.flag & info ["f"; "full"] ~docv:"full log (except storage)"))

let total =
  Arg.(Arg.value & (Arg.flag & info ["t"; "total"] ~docv:"total log"))

let log_file =
  Arg.(value & (opt (some string) None & info ["l"; "log-file"] ~docv:"image width"))

let () =
  let info =
    let doc = "poc of a matrix server" in
    Term.info "server" ~version:"%%VERSION%%" ~doc
  in
  Term.exit @@ Term.eval (Term.(const main $ Term.(const setup $ Logs_cli.level () $ full $ total $ log_file)), info)
