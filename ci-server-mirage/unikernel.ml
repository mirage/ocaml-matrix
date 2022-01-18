open Rresult
open Lwt.Infix
open Matrix_ci_server

let ( <.> ) f g = fun x -> f (g x)

module type DNS = sig
  type t

  val gethostbyname : t -> [ `host ] Domain_name.t ->
    (Ipaddr.V4.t, [> `Msg of string ]) result Lwt.t
end

module Make
  (_ : Mirage_console.S)
  (Random : Mirage_random.S)
  (Time : Mirage_time.S)
  (Mclock : Mirage_clock.MCLOCK)
  (Pclock : Mirage_clock.PCLOCK)
  (Stack : Tcpip.Stack.V4V6)
  (Dns : DNS) (* XXX(dinosaure): ask @hannesm to provide a signature. *)
  (Paf : Dream_paf_mirage.S with type stack = Stack.TCP.t)
  (_ : sig end) = struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
  module Client_routes = Client_routes.Make (Pclock) (Time) (Stack)
  module Federation_routes = Federation_routes.Make (Pclock) (Time) (Stack)

  (* Setup mimic context for https calls to other servers *)
  module Client = Matrix_ci_server.Paf_cohttp
  module Nss = Ca_certs_nss.Make(Pclock)

  let authenticator = Result.get_ok (Nss.authenticator ())
  let default_tls_cfg = Tls.Config.client ~authenticator ()

  let stack = Mimic.make ~name:"stack"
  let tls = Mimic.make ~name:"tls"

  let with_stack v ctx = Mimic.add stack (Stack.tcp v) ctx

  let with_tls port ctx =
    let k scheme domain_name cfg stack ipaddr port = match scheme with
      | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port) | _ -> Lwt.return_none in
    Mimic.(fold Paf.tls_edn Fun.[ req Client.scheme
                                ; opt Client.domain_name
                                ; dft tls default_tls_cfg
                                ; req stack
                                ; req Client.ipaddr
                                ; dft Client.port port ] ~k ctx)

  let dns = Mimic.make ~name:"dns"

  let with_dns v ctx = Mimic.add dns v ctx
  let with_sleep ctx = Mimic.add Paf_cohttp.sleep Time.sleep_ns ctx

  let with_resolv ctx =
    let k dns domain_name =
      if Domain_name.to_string domain_name = "my.domain.name"
      then Lwt.return_some (Ipaddr.V4 Ipaddr.V4.localhost)
      else
      Dns.gethostbyname dns domain_name >>= function
      | Ok ipv4 -> Lwt.return_some (Ipaddr.V4 ipv4)
      | _ -> Lwt.return_none in
    Mimic.(fold Client.ipaddr Fun.[ req dns; req Client.domain_name ] ~k ctx)

  let http_ctx stack port dns =
    Mimic.empty
    |> with_sleep
    |> with_tls port    (* domain_name -> tls -> stack -> ipaddr -> port => (domain_name * tls * stack * ipaddr * port) *)
    |> with_resolv      (* domain_name => ipaddr *)
    |> with_stack stack (* stack *)
    |> with_dns dns     (* dns *)
  (* done with the setup *)

  let process_key priv_key =
    match Base64.decode priv_key with
    | Error (`Msg s) -> raise @@ Invalid_argument s
    | Ok priv_key -> (
    let priv_key = Cstruct.of_string priv_key in
    match
      Rresult.R.error_msg_to_invalid_arg
        (X509.Private_key.of_cstruct priv_key `ED25519)
    with
    | `ED25519 key -> key,
      Mirage_crypto_ec.Ed25519.pub_of_priv key
    | _ -> raise @@ Invalid_argument "Not an ED25519 key")

  let info ctx priv_key =
    Store.connect ctx (Key_gen.remote ()) >>= fun (store, remote) ->
    Store.pull store remote >>= fun () ->
    let priv_key, pub_key = process_key priv_key in
    Lwt.return
    Matrix_ci_server.Common_routes.{
      server_name = "matrix.egar.im"
    ; key_name = "foo_bar"
    ; priv_key
    ; pub_key
    ; ctx
    ; store
    ; remote }

  let client_server port stack info =
    let router = Dream.logger @@ Client_routes.router info @@ Dream.not_found in
    Dream.http ~port (Stack.tcp stack) router

  let federation_server port stack info =
    let router =
      Dream.logger @@ Federation_routes.router info @@ Dream.not_found in
    Dream.https ~port (Stack.tcp stack) router

  let start _console _ _ _ _ stackv4v6 dns _ ctx =
    let ctx = Mimic.merge ctx (http_ctx stackv4v6 (Key_gen.federation_port ()) dns) in
    info ctx @@ Key_gen.priv_key () >>= fun info ->
      Lwt.join
      [
        client_server (Key_gen.client_port ()) stackv4v6 info;
        federation_server (Key_gen.federation_port ()) stackv4v6 info;
      ]
end
