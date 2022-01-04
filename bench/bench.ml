let user = "bonjour"
let pwd = "bonjour"

open Shexp_process.Infix

let ( let* ) = ( >>= )
let ( let+ ) = ( >>| )

let setup_server () =
  let open Shexp_process in
  run_exit_code "rm" ["-rf"; "/tmp/ocaml-matrix/"]
  |> map ~f:ignore
  >> mkdir "/tmp/ocaml-matrix/"
  >> chdir "/tmp/ocaml-matrix"
       (run "git" ["init"; "--initial-branch=master"]
       >> run "touch" ["init"]
       >> run "git" ["add"; "init"]
       >> run "git" ["commit"; "-m"; "init"]
       >> run "openssl" ["genpkey"; "-algorithm"; "Ed25519"; "-out"; "pkey"])
  >> run "matrix-ci-server-setup" ["user"; "/tmp/ocaml-matrix/"; user; pwd]
  >> spawn "matrix-ci-server-bin"
       ["matrix.localhost"; "foo,/tmp/ocaml-matrix/pkey"]
  |> map ~f:Background_command.pid

module Client = Matrix_current.Raw

let bench client job =
  let open Lwt.Syntax in
  let settings =
    {Client.name= "Test room"; topic= "."; power_level_content_override= None}
  in
  let+ result = Client.get_room ~job ~alias:"test_room" ~settings client in
  match result with
  | Ok v -> Printf.printf "OK: %s\n" v
  | Error v -> Printf.printf "Error: %s\n" v

let () =
  let server_pid = Shexp_process.eval (setup_server ()) in
  Unix.sleepf 0.2;
  let server = Client.Server.v `Http "localhost" 8008 in
  let client = Client.v ~server ~device:None ~user ~pwd ~max_connections:64 () in
  let switch = Current.Switch.create ~label:"job" () in
  let config = Current.Config.v () in
  let job = Current.Job.create ~switch ~label:"job" ~config () in
  Fun.protect ~finally:(fun () ->
      Shexp_process.(eval (run "kill" ["-9"; Int.to_string server_pid])))
  @@ fun () -> Lwt_main.run (bench client job)
