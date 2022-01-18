open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git
   unikernel/empty-commit/config.ml
   commit #45d90b8792ab8f3866751f462619c7dd7860e5d5 *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v; !v

let mimic_conf () =
  let packages = [package "mimic"] in
  impl
  @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())

       method! connect _ _modname =
         function
         | [a; b] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [x] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

(* TODO(dinosaure): [timeout] and [timer interval]. *)
let mimic_happy_eyeballs =
  let packages = [package "git-mirage" ~sublibs:["happy-eyeballs"]] in
  impl
  @@ object
       inherit base_configurable
       method ty = random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic
       method module_name = "Git_mirage_happy_eyeballs.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_happy_eyeballs"

       method! connect _ modname =
         function
         | [_random; _time; _mclock; _pclock; stackv4v6] ->
           Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end

let mimic_tcp =
  let packages = [package "git-mirage" ~sublibs:["tcp"]] in
  impl
  @@ object
       inherit base_configurable
       method ty = tcpv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_tcp"

       method! connect _ modname =
         function
         | [_tcpv4v6; ctx] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname ctx
         | _ -> assert false
     end

let mimic_http ?tls_key_fingerprint ?tls_cert_fingerprint headers =
  let packages = [package "git-mirage" ~sublibs:["http"]] in
  impl
  @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> tcpv4v6 @-> mimic @-> mimic

       method! keys =
         match tls_key_fingerprint, tls_cert_fingerprint with
         | Some tls_key_fingerprint, None ->
           let keys =
             match headers with
             | Some headers -> [Key.abstract headers]
             | None -> [] in
           [Key.abstract tls_key_fingerprint] @ keys
         | None, Some tls_cert_fingerprint ->
           let keys =
             match headers with
             | Some headers -> [Key.abstract headers]
             | None -> [] in
           [Key.abstract tls_cert_fingerprint] @ keys
         | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
           let keys =
             match headers with
             | Some headers -> [Key.abstract headers]
             | None -> [] in
           [Key.abstract tls_key_fingerprint; Key.abstract tls_cert_fingerprint]
           @ keys
         | None, None -> (
           match headers with
           | Some headers -> [Key.abstract headers]
           | None -> [])

       method module_name = "Git_mirage_http.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_http"

       method! connect _ modname =
         function
         | [_time; _pclock; _tcpv4v6; ctx] -> (
           let serialize_headers ppf = function
             | None -> ()
             | Some headers ->
               Fmt.pf ppf "?headers:%a" Key.serialize_call
                 (Key.abstract headers) in
           match tls_key_fingerprint, tls_cert_fingerprint with
           | Some tls_key_fingerprint, None ->
             Fmt.str
               {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_key_fingerprint:%a%a|ocaml}
               modname ctx modname Key.serialize_call
               (Key.abstract tls_key_fingerprint)
               Fmt.(const string " " ++ serialize_headers)
               headers
           | None, Some tls_cert_fingerprint ->
             Fmt.str
               {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname Key.serialize_call
               (Key.abstract tls_cert_fingerprint)
               Fmt.(const string " " ++ serialize_headers)
               headers
           | None, None ->
             Fmt.str
               {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers%a|ocaml}
               modname ctx modname
               Fmt.(const string " " ++ serialize_headers)
               headers
           | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
             Fmt.str
               {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers
                              ?tls_key_fingerprint:%a ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname Key.serialize_call
               (Key.abstract tls_key_fingerprint)
               Key.serialize_call
               (Key.abstract tls_cert_fingerprint)
               Fmt.(const string " " ++ serialize_headers)
               headers)
         | _ -> assert false
     end

let tcpv4v6_of_stackv4v6 =
  impl
  @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> tcpv4v6
       method module_name = "Git_mirage_happy_eyeballs.TCPV4V6"

       method! packages =
         Key.pure [package "git-mirage" ~sublibs:["happy-eyeballs"]]

       method name = "tcpv4v6"

       method! connect _ modname =
         function
         | [stackv4v6] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end
(* --- end of copied code --- *)

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname." ["hostname"] in
  Key.(create "hostname" Arg.(opt string "localhost" doc))

let client_port =
  let doc =
    Key.Arg.info ~doc:"Listening port for the client API." ["client_port"] in
  Key.(create "client_port" Arg.(opt int 8008 doc))

let federation_port =
  let doc =
    Key.Arg.info ~doc:"Listening port for the federation API."
      ["federation_port"] in
  Key.(create "federation_port" Arg.(opt int 8448 doc))

let priv_key =
  let doc = Key.Arg.info ~doc:"Ed25519 private key." ["priv_key"] in
  Key.(create "priv_key" Arg.(required string doc))

let tls_key_fingerprint =
  let doc =
    Key.Arg.info ~doc:"The fingerprint of the TLS key." ["tls-key-fingerprint"]
  in
  Key.(create "tls_key_fingerprint" Arg.(opt (some string) None doc))

let tls_cert_fingerprint =
  let doc =
    Key.Arg.info ~doc:"The fingerprint of the TLS certificate."
      ["tls-cert-fingerprint"] in
  Key.(create "tls_cert_fingerprint" Arg.(opt (some string) None doc))

let remote =
  let doc = Key.Arg.info ~doc:"Remote git repository." ["r"; "remote"] in
  Key.(
    create "remote" Arg.(opt string "git://127.0.0.1/ocaml-matrix-store" doc))

type dns = Dns

let dns = typ Dns

let dns_conf () =
  let packages = [package "dns-client" ~min:"6.1.0" ~sublibs:["mirage"]] in
  impl
  @@ object
       inherit base_configurable
       method ty = random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> dns
       method module_name = "Dns_client_mirage.Make"
       method! packages = Key.pure packages
       method name = "dns"

       method! connect _ modname =
         function
         | [_random; _time; _mclock; _pclock; stackv4v6] ->
           Fmt.str {ocaml|Lwt.return (%s.create %s)|ocaml} modname stackv4v6
         | _ -> assert false
     end

type paf = Dream_paf

let paf = typ Dream_paf

let paf_conf () =
  let packages = [package "paf" ~sublibs:["mirage"]] in
  impl
  @@ object
       inherit base_configurable
       method ty = time @-> stackv4v6 @-> paf
       method module_name = "Dream_paf_mirage.Make"
       method! packages = Key.pure packages
       method name = "paf"
     end

let dream =
  foreign "Unikernel.Make"
    ~packages:
      [
        package "logs"; package ~min:"2.10.0" "irmin-mirage";
        package ~min:"2.10.0" "irmin-mirage-git";
        package ~min:"3.7.0" "git-mirage"; package ~min:"7.0.0" "tcpip";
        package "dream-mirage" ~pin:"git+https://github.com/clecat/dream.git";
        package "matrix-ci-server";
      ]
    ~keys:
      Key.
        [
          abstract hostname; abstract client_port; abstract federation_port;
          abstract priv_key; abstract remote;
        ]
    (console
    @-> random
    @-> time
    @-> mclock
    @-> pclock
    @-> stackv4v6
    @-> dns
    @-> paf
    @-> mimic
    @-> job)

let mimic_impl random stackv4v6 mclock pclock time =
  let tcpv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6 in
  let mhappy_eyeballs =
    mimic_happy_eyeballs $ random $ time $ mclock $ pclock $ stackv4v6 in
  let mtcp = mimic_tcp $ tcpv4v6 $ mhappy_eyeballs in
  let mhttp =
    mimic_http ~tls_key_fingerprint ~tls_cert_fingerprint None
    $ time
    $ pclock
    $ tcpv4v6
    $ mhappy_eyeballs in
  merge mhttp mtcp

let random = default_random
let console = default_console
let time = default_time
let pclock = default_posix_clock
let mclock = default_monotonic_clock
let stackv4v6 = generic_stackv4v6 default_network

let dns random time mclock pclock stackv4v6 =
  dns_conf () $ random $ time $ mclock $ pclock $ stackv4v6

let dns = dns random time mclock pclock stackv4v6
let paf time stackv4v6 = paf_conf () $ time $ stackv4v6

let mimic_impl =
  mimic_impl default_random stackv4v6 default_monotonic_clock
    default_posix_clock default_time

let () =
  register "dream"
    [
      dream
      $ console
      $ random
      $ time
      $ mclock
      $ pclock
      $ stackv4v6
      $ dns
      $ paf time stackv4v6
      $ mimic_impl;
    ]
