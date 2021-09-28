open Cmdliner

let client_server info =
  let interface = "0.0.0.0" in
  let scheme = "http" in
  let port = 8008 in
  Dream.log "Running on %s:%i (%s://localhost:%i)" interface port scheme port;
  Dream.log "Type Ctrl+C to stop";
  Dream.serve ~interface ~port
  @@ Dream.logger
  @@ Client_routes.router info
  @@ Dream.not_found

(* Fix the certificate and key paths for some command line args *)
let federation_server info =
  let interface = "0.0.0.0" in
  let scheme = "https" in
  let port = 8448 in
  let certificate_file = Unix.getenv "PWD" ^ "/server.crt" in
  let key_file = Unix.getenv "PWD" ^ "/server.key" in
  Dream.log "Running on %s:%i (%s://localhost:%i)" interface port scheme port;
  Dream.log "Type Ctrl+C to stop";
  Dream.serve ~interface ~port ~https:true ~certificate_file ~key_file
  @@ Dream.logger
  @@ Federation_routes.router info
  @@ Dream.not_found

(* Rework the function for something cleaner (especially the rais part) *)
let read_key file =
  let ic = open_in file in
  let len = in_channel_length ic in
  let bytes = Bytes.create len in
  really_input ic bytes 0 len;
  match
    Rresult.R.error_msg_to_invalid_arg
      (X509.Private_key.decode_pem (Cstruct.of_bytes bytes))
  with
  | `ED25519 key -> key, Mirage_crypto_ec.Ed25519.pub_of_priv key
  | _ -> raise @@ Invalid_argument "Not an ED25519 key"

let main server_name (key_name, key_path) () =
  let priv_key, pub_key = read_key key_path in
  let info = Common_routes.{server_name; key_name; priv_key; pub_key} in
  Lwt_main.run (Lwt.join [client_server info; federation_server info])

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

let server_key =
  Arg.(
    required
    & pos 1 (some @@ pair string string) None
    & info [] ~docv:"server_key"
        ~doc:"the key name and it's path, separated by a ','")

let () =
  let info =
    let doc = "poc of a matrix server" in
    Term.info "server" ~version:"%%VERSION%%" ~doc in
  Term.exit
  @@ Term.eval
       ( Term.(
           const main
           $ server_name
           $ server_key
           $ Term.(const setup $ Logs_cli.level ())),
         info )
