open Cmdliner
open Matrix_ci_server
module Stack = Tcpip_stack_socket.V4V6
module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
module Client_routes = Client_routes.Make (Pclock) (Time) (Stack)
module Federation_routes = Federation_routes.Make (Pclock) (Time) (Stack)
module Nss = Ca_certs_nss.Make (Pclock)

let client_server port stack info =
  let router = Dream.logger @@ Client_routes.router info @@ Dream.not_found in
  Dream.http ~port (Stack.tcp stack) router

let federation_server port stack info =
  let router =
    Dream.logger @@ Federation_routes.router info @@ Dream.not_found in
  Dream.https ~port (Stack.tcp stack) router

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

module TLS = struct
  include Tls_mirage.Make (Tcpip_stack_socket.V4V6.TCP)
  open Lwt.Infix

  let ( >>? ) = Lwt_result.bind

  type endpoint =
    Tcpip_stack_socket.V4V6.TCP.t
    * Tls.Config.client
    * [ `host ] Domain_name.t option
    * Ipaddr.t
    * int

  let connect (stack, tls, host, ipaddr, port) =
    let open Tcpip_stack_socket.V4V6 in
    TCP.create_connection stack (ipaddr, port)
    >|= Rresult.R.reword_error (fun err -> `Read err)
    >>? fun flow ->
    client_of_flow tls ?host flow >>= fun flow -> Lwt.return flow
end

let tls_edn, tls_protocol = Mimic.register ~priority:10 ~name:"tls" (module TLS)
let witness_stack = Mimic.make ~name:"stack"

let fill ctx federation_port =
  let open Mimic in
  let k0 stack scheme port domain_name =
    match scheme with
    | `HTTP -> Lwt.return_none
    | `HTTPS -> (
      match Unix.gethostbyname (Domain_name.to_string domain_name) with
      | {Unix.h_addr_list; _} when Array.length h_addr_list > 0 ->
        let ipaddr = Ipaddr_unix.of_inet_addr h_addr_list.(0) in
        let authenticator = Result.get_ok (Nss.authenticator ()) in
        let cfg = Tls.Config.client ~authenticator () in
        Lwt.return_some (Stack.tcp stack, cfg, Some domain_name, ipaddr, port)
      | _ -> Lwt.return_none) in
  let k1 stack scheme port ipaddr =
    match scheme with
    | `HTTP -> Lwt.return_none
    | `HTTPS ->
      let authenticator = Result.get_ok (Nss.authenticator ()) in
      let cfg = Tls.Config.client ~authenticator () in
      Lwt.return_some (Stack.tcp stack, cfg, None, ipaddr, port) in
  let ctx =
    Mimic.fold tls_edn
      Fun.
        [
          req witness_stack; req Paf_cohttp.scheme;
          dft Paf_cohttp.port federation_port; req Paf_cohttp.domain_name;
        ]
      ~k:k0 ctx in
  let ctx =
    Mimic.fold tls_edn
      Fun.
        [
          req witness_stack; req Paf_cohttp.scheme;
          dft Paf_cohttp.port federation_port; req Paf_cohttp.ipaddr;
        ]
      ~k:k1 ctx in
  ctx

let sleep v = Lwt_unix.sleep (Int64.to_float v)

let stack_of_addr addr =
  let interface = Ipaddr.V4.Prefix.of_string_exn addr in
  let%lwt udp_socket =
    Udpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false interface None
  in
  let%lwt tcp_socket =
    Tcpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false interface None
  in
  Stack.connect udp_socket tcp_socket

let fill_http ctx federation_port stack =
  let ctx = Mimic.add witness_stack stack ctx in
  let ctx = fill Mimic.(add Paf_cohttp.sleep sleep ctx) federation_port in
  ctx

let main
    server_name
    (key_name, key_path)
    addr
    client_port
    federation_port
    store_path
    () =
  let priv_key, pub_key = read_key key_path in
  Lwt_main.run
    (let%lwt stack = stack_of_addr addr in
     let ctx = Mimic.empty in
     let ctx = fill_http ctx federation_port stack in
     let config = Irmin_git.config store_path in
     let%lwt repo = Store.Store.Repo.v config in
     let%lwt store = Store.Store.master repo in
     let info =
       Common_routes.{server_name; key_name; priv_key; pub_key; ctx; store}
     in
     Lwt.join
       [
         client_server client_port stack info;
         federation_server federation_port stack info;
       ])

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

let addr =
  Arg.(
    value
    & opt string "0.0.0.0/0"
    & info ["addr"] ~docv:"ip_address/mask" ~doc:"the ip address and it's mask")

let client_port =
  Arg.(
    value
    & opt int 8008
    & info ["client_port"] ~docv:"client_port"
        ~doc:"the port of the client API, default to 8008")

let federation_port =
  Arg.(
    value
    & opt int 8448
    & info ["federation_port"] ~docv:"federation_port"
        ~doc:"the port of the federation API, default to 8448")

let store_path =
  Arg.(
    value
    & opt string "/tmp/ocaml-matrix"
    & info ["store_path"] ~docv:"store_path"
        ~doc:"the path to the irmin git store, default to `/tmp/ocaml-matrix`")

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
           $ addr
           $ client_port
           $ federation_port
           $ store_path
           $ Term.(const setup $ Logs_cli.level ())),
         info )
