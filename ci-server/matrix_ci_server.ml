open Cmdliner

let main server_name () =
  Dream.run ~interface:"0.0.0.0" ~port:8008
  @@ Dream.logger
  @@ Client_routes.router server_name
  @@ Dream.not_found

let setup level =
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let server_name =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"server_name" ~doc:"the name of the server")

let () =
  let info =
    let doc = "poc of a matrix server" in
    Term.info "server" ~version:"%%VERSION%%" ~doc in
  Term.exit
  @@ Term.eval (Term.(const main $ server_name $  Term.(const setup $ Logs_cli.level ())), info)
