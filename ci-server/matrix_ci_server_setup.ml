open Cmdliner

module Content_string = struct
  include Irmin.Contents.String

  (* We should add here a merge function which will allow to properly handle
     concurrent merges *)
  let merge =
    let dt = Irmin.Type.(option string) in
    let equal = Irmin.Type.(unstage (equal dt)) in
    let default = Irmin.Merge.default dt in
    let f ~old x y =
      if equal x y then Irmin.Merge.ok x else Irmin.Merge.f default ~old x y
    in
    Irmin.Merge.v dt f
end

module Stack = Tcpip_stack_socket.V4V6
module Git_happy_eyeballs =
  Git_mirage_happy_eyeballs.Make (Mirage_crypto_rng) (Time) (Mclock) (Pclock)
    (Stack)
module Git_tcp = Git_mirage_tcp.Make (Stack.TCP) (Git_happy_eyeballs)
module Store = Irmin_mirage_git.Mem.KV (Content_string)
module Sync = Irmin.Sync (Store)
open Lwt.Infix

let connect ctx remote =
  let config = Irmin_mem.config () in
  Store.Repo.v config >>= Store.master >|= fun repo ->
  repo, Store.remote ~ctx remote

let pull repo upstream =
  Logs.info (fun m -> m "pulling from remote!");
  Sync.pull ~depth:1 repo upstream `Set >|= function
  | Ok `Empty -> Logs.warn (fun m -> m "pulled empty repository")
  | Ok (`Head _ as s) ->
    Logs.info (fun m -> m "ok, pulled %a!" Sync.pp_status s)
  | Error (`Msg e) -> Logs.warn (fun m -> m "pull error %s" e)
  | Error (`Conflict msg) -> Logs.warn (fun m -> m "pull conflict %s" msg)

let push store upstream =
  Logs.info (fun m -> m "pushing to remote!");
  Sync.push store upstream

let setup =
  let setup level =
    let style_renderer = `Ansi_tty in
    Fmt_tty.setup_std_outputs ~style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let open Term in
  const setup $ Logs_cli.level ()

let root_cmd =
  let open Term in
  let info = info "server_utility" in
  let term = ret (const (fun () -> `Help (`Pager, None)) $ setup) in
  term, info

let stack () =
  let interface = Ipaddr.V4.Prefix.global in
  let%lwt udp_socket =
    Udpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false interface None
  in
  let%lwt tcp_socket =
    Tcpv4v6_socket.connect ~ipv4_only:false ~ipv6_only:false interface None
  in
  Stack.connect udp_socket tcp_socket

let fill_git ctx stack =
  let ctx = ctx in
  let happy_eyeballs = Happy_eyeballs.create 1000000L in
  let%lwt happy_ctx = Git_happy_eyeballs.connect ~happy_eyeballs stack in
  let ctx = Mimic.merge ctx happy_ctx in
  Git_tcp.connect ctx

module User = struct
  let f store user_id server_name password () =
    let%lwt stack = stack () in
    let ctx = Mimic.empty in
    let%lwt ctx = fill_git ctx stack in
    let%lwt store, remote = connect ctx store in
    let%lwt () = pull store remote in
    (* Verify if the user already exists *)
    let user_id = "@" ^ user_id ^ ":" ^ server_name in
    let%lwt s_user = Store.find_tree store (Store.Key.v ["users"; user_id]) in
    match s_user with
    | Some _ ->
      Logs.err (fun m -> m "user id %s already exists" user_id);
      Lwt.return 1
    | None -> (
      Mirage_crypto_rng_unix.initialize ();
      let salt = Cstruct.to_string @@ Mirage_crypto_rng.generate 32 in
      let digest = Digestif.BLAKE2B.hmac_string ~key:salt password in
      let hashed = Digestif.BLAKE2B.to_hex digest in
      let%lwt tree = Store.tree store in
      Store.Tree.clear tree;
      let%lwt tree =
        Store.Tree.add tree (Store.Key.v ["users"; user_id; "username"]) user_id
      in
      let%lwt tree =
        Store.Tree.add tree (Store.Key.v ["users"; user_id; "salt"]) salt in
      let%lwt tree =
        Store.Tree.add tree (Store.Key.v ["users"; user_id; "password"]) hashed
      in
      let%lwt return =
        Store.set_tree
          ~info:(fun () ->
            Irmin.Info.v
              ~date:(Int64.of_int @@ fst (Pclock.now_d_ps ()))
              ~author:"matrix-ci-server-setup" "add user")
          store (Store.Key.v []) tree in
      match return with
      | Ok () -> (
        let%lwt return = push store remote in
        match return with
        | Ok _ ->
          Lwt.return 0
          | Error write_error ->
          Logs.err (fun m ->
            m "Push error: %a" Sync.pp_push_error write_error);
        Lwt.return 1
        )
      | Error write_error ->
        Logs.err (fun m ->
            m "Write error: %a" (Irmin.Type.pp Store.write_error_t) write_error);
        Lwt.return 1)

  let store =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"store" ~doc:"path to the store")

  let user_id =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"user" ~doc:"user id to create")

  let server_name =
    Arg.(
      required
      & pos 2 (some string) None
      & info [] ~docv:"server_name" ~doc:"server name of the user")

  let pwd =
    Arg.(
      required
      & pos 3 (some string) None
      & info [] ~docv:"password" ~doc:"user password")

  let cmd =
    let info =
      let doc = "Creates a user." in
      Term.info "user" ~doc in
    let term =
      Term.(app (const Lwt_main.run) (const f $ store $ user_id $ server_name $ pwd $ setup))
    in
    term, info
end

let () =
  let open Term in
  exit_status @@ eval_choice root_cmd [User.cmd]
