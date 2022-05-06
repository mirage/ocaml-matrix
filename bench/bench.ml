let user = "bonjour"
let pwd = "bonjour"
let host = "localhost"
let userid = Fmt.str "@%s:%s" user host

open Shexp_process.Infix

let ( let* ) = ( >>= )
let ( let+ ) = ( >>| )

let setup_server () =
  let open Shexp_process in
  run_exit_code "rm" ["-rf"; "/tmp/ocaml-matrix/"]
  |> map ~f:ignore
  >> mkdir "/tmp/ocaml-matrix/"
  >> chdir "/tmp/ocaml-matrix"
       (run "git" ["init"; "--initial-branch=main"]
       >> run "touch" ["init"]
       >> run "git" ["add"; "init"]
       >> run "git" ["commit"; "-m"; "init"]
       >> run "openssl" ["genpkey"; "-algorithm"; "Ed25519"; "-out"; "pkey"])
  >> run "matrix-ci-server-setup" ["user"; "/tmp/ocaml-matrix/"; userid; pwd]
  >> spawn "matrix-ci-server-bin" [host; "foo,/tmp/ocaml-matrix/pkey"]
  |> map ~f:Background_command.pid

module Client = Matrix_current.Raw

let message =
  Matrix_common.Events.Event_content.Message.(Text (Text.make ~body:"hello" ()))

type test = {n_messages: int; max_connections: int; n_rooms: int}

let v m c r = {n_messages= m; max_connections= c; n_rooms= r}
let existing_clients = Hashtbl.create 3
let server = Client.Server.v `Http "localhost" 8008

let client max_connections =
  try Hashtbl.find existing_clients max_connections
  with Not_found ->
    let client =
      Client.v ~server
        ~device:(Some ("dev_" ^ Int.to_string max_connections))
        ~user ~pwd ~max_connections () in
    Hashtbl.add existing_clients max_connections client;
    client

let all_tests =
  [
    v 128 1 1; v 128 4 1; v 128 16 1; v 32 1 4; v 32 4 4; v 32 16 4; v 8 1 16;
    v 8 4 16; v 8 16 16;
  ]

let test_id {n_messages; max_connections; n_rooms} =
  Fmt.str "%d.%d.%d" n_messages max_connections n_rooms

let bench_test job ({n_messages; max_connections; n_rooms} as test) =
  Current.Job.log job "TEST: %d/%d/%d" n_messages max_connections n_rooms;
  let open Lwt.Syntax in
  let settings =
    {Client.name= "Test room"; topic= "."; power_level_content_override= None}
  in
  let client = client max_connections in
  let* rooms_id =
    List.init n_rooms Fun.id
    |> Lwt_list.map_s (fun id ->
           Client.get_room ~job
             ~alias:(Fmt.str "test_%s_room_%d" (test_id test) id)
             ~settings client
           |> Lwt.map Result.get_ok) in
  let t0 = Mtime_clock.counter () in
  let+ () =
    rooms_id
    |> Lwt_list.iter_p (fun room_id ->
           List.init n_messages ignore
           |> Lwt_list.iter_p (fun () ->
                  Client.post ~job ~room_id client message
                  |> Lwt.map Result.get_ok)) in
  let duration = Mtime_clock.count t0 in
  test, duration

let bench () =
  let switch = Current.Switch.create ~label:"job" () in
  let config = Current.Config.v () in
  let job = Current.Job.create ~switch ~label:"job" ~config () in
  Lwt_list.map_s (bench_test job) all_tests

let () =
  let server_pid = Shexp_process.eval (setup_server ()) in
  Unix.sleepf 0.2;
  Fun.protect ~finally:(fun () ->
      Shexp_process.(eval (run "kill" ["-9"; Int.to_string server_pid])))
  @@ fun () ->
  let durations = Lwt_main.run (bench ()) in
  Printf.printf "For all tests: 128 messages:\n";
  List.iter
    (fun (test, duration) ->
      Fmt.pr "%d rooms %d connections: %a\n" test.n_rooms test.max_connections
        Mtime.Span.pp duration)
    durations
