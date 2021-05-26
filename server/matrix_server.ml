open Cmdliner

let main () =
  Logs.debug (fun m -> m "Server launched%!")
  ; ignore
      (Lwt_main.run
         (Lwt.join [Client_to_server.server; Server_to_server.server]))

let setup level full total log_file =
  (* let style_renderer = `None in *)
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ()
  ; Logs.set_level level
  ; (match log_file with
    | None -> Logs.set_reporter (Logs_fmt.reporter ())
    | Some log_file ->
      let dst_channel = open_out log_file in
      let dst = Format.formatter_of_out_channel dst_channel in
      Logs.set_reporter (Logs_fmt.reporter ~dst ()))
  ; if not total then (
      List.filter
        (fun src ->
          let src = Logs.Src.name src in
          let src_hd = String.split_on_char '.' src |> List.hd in
          List.exists
            (fun s -> String.equal src_hd s)
            ["irmin"; "git"; "git-unix"; "decompress"])
        (Logs.Src.list ())
      |> List.iter (fun src -> Logs.Src.set_level src (Some Logs.Info))
      ; if not full then
          List.filter
            (fun src ->
              let src = Logs.Src.name src in
              let src_hd = String.split_on_char '.' src |> List.hd in
              List.exists (fun s -> not (String.equal src_hd s)) ["application"])
            (Logs.Src.list ())
          |> List.iter (fun src -> Logs.Src.set_level src (Some Logs.Info)))

let full =
  Arg.(
    Arg.value & Arg.flag & info ["f"; "full"] ~docv:"full log (except storage)")

let total = Arg.(Arg.value & Arg.flag & info ["t"; "total"] ~docv:"total log")

let log_file =
  Arg.(
    value & opt (some string) None & info ["l"; "log-file"] ~docv:"image width")

let () =
  let info =
    let doc = "poc of a matrix server" in
    Term.info "server" ~version:"%%VERSION%%" ~doc in
  Term.exit
  @@ Term.eval
       ( Term.(
           const main
           $ Term.(const setup $ Logs_cli.level () $ full $ total $ log_file))
       , info )
