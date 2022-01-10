module Client = Matrix_current.Raw

type remote = {user: string; password: string; server: string}
[@@deriving yojson]

let ( let** ) = Lwt_result.bind

let main {user; password; server} room message =
  let switch = Current.Switch.create ~label:"job" () in
  let config = Current.Config.v () in
  let job = Current.Job.create ~switch ~label:"job" ~config () in
  let server =
    let uri = Uri.of_string server in
    let scheme =
      match Uri.scheme uri with
      | None | Some "https" -> `Https
      | Some "http" -> `Http
      | _ ->
        failwith "remote: scheme is not supported. Specify either http or https"
    in
    let remote =
      match Uri.host uri with
      | None -> (
        match Uri.path uri with
        | "" -> failwith "remote: host not specified"
        | v -> v)
      | Some remote -> remote in
    let port =
      match Uri.port uri, scheme with
      | None, `Http -> 8080
      | None, `Https -> 443
      | Some port, _ -> port in
    Printf.printf "%s: sending message to room %S via server %s." user room
      remote;
    Client.Server.v scheme remote port in
  let client =
    Client.v ~server ~device:(Some "matrix-ci-bot") ~user ~pwd:password () in
  let settings =
    {
      Client.name= room;
      topic= "managed by matrix_current";
      power_level_content_override= None;
    } in
  let** room_id = Client.get_room ~job ~alias:room ~settings client in
  let message =
    Matrix_common.Events.Event_content.Message.(
      Text (Text.make ~body:message ())) in
  Client.post ~job ~room_id client message

let run a b c =
  match Lwt_main.run (main a b c) with
  | Ok () -> Ok ()
  | Error msg ->
    Printf.printf "Failure was encountered:\n%s\n" msg;
    Error 1

open Cmdliner

let remote =
  let file =
    Arg.required
    @@ Arg.(opt (some string)) None
    @@ Arg.info ~doc:"remote file" ~docv:"REMOTE" ["remote"] in
  Term.(
    const (fun file ->
        Yojson.Safe.from_file file
        |> remote_of_yojson
        |> Result.map_error (fun s -> `Msg s))
    $ file)
  |> Term.term_result

let room =
  Arg.required
  @@ Arg.(opt (some string)) None
  @@ Arg.info ~doc:"room" ~docv:"ROOM" ["room"]

let message =
  Arg.required
  @@ Arg.(opt (some string)) None
  @@ Arg.info ~doc:"message" ~docv:"MESSAGE" ["message"]

let main = Term.(const run $ remote $ room $ message), Term.info "matrix-client"
let () = Term.(exit @@ eval main)
