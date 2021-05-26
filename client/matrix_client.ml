open Cmdliner
open Commands

let main () =
  (* let open Client in
     let _ = login gwen in
     let _ = public_rooms None in
     let _ = public_rooms (Some "ocaml-matrix") in *)
  let open Server in
  (* let keys = get_server_keys () in
     let _key = List.hd @@ Matrix_stos.Key.Server_key.get_verify_keys keys in *)
  let rooms = public_rooms "ed25519:foo_bar" in
  let room_id = Matrix_stos.Public_rooms.Get_public_rooms.Response.(get_chunk rooms |> List.hd |> Public_rooms_chunk.get_room_id) in
  let _ = join_room "ed25519:foo_bar" room_id in
  ()

let setup level =
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ()
  ; Logs.set_level level
  ; Logs.set_reporter (Logs.format_reporter ())

let () =
  let info =
    let doc = "poc of a matrix client" in
    Term.info "server" ~version:"%%VERSION%%" ~doc in
  Term.exit
  @@ Term.eval (Term.(const main $ Term.(const setup $ Logs_cli.level ())), info)
