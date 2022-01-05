open Current.Syntax

let src = Logs.Src.create "matrix.current_matrix" ~doc:"OCurrent matrix plugin"

module Log = (val Logs.src_log src : Logs.LOG)

type context = Post.t

let context ?max_connections ~host ~port ~scheme ~user ~pwd ~device () =
  let server = Http.Server.v scheme host port in
  Client.v ?max_connections ~server ~user ~pwd ~device ()

module Cmdliner = struct
  open Cmdliner

  let named f = Cmdliner.Term.(app (const f))

  let host =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"Matrix server host" ~docv:"MATRIX_HOST" ["matrix-host"]
    |> named (fun x -> `Host x)

  let port =
    Arg.required
    @@ Arg.opt Arg.(some int) (Some 8448)
    @@ Arg.info ~doc:"Matrix server port" ~docv:"MATRIX_PORT" ["matrix-port"]
    |> named (fun x -> `Port x)

  let scheme =
    Arg.required
    @@ Arg.opt Arg.(some (enum ["http", `Http; "https", `Https])) (Some `Https)
    @@ Arg.info ~doc:"Matrix server scheme" ~docv:"MATRIX_SCHEME"
         ["matrix-scheme"]
    |> named (fun x -> `Scheme x)

  let user =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"Matrix bot user" ~docv:"MATRIX_USER" ["matrix-user"]
    |> named (fun x -> `User x)

  let load_file path =
    try
      let ch = open_in path in
      let len = in_channel_length ch in
      let data = really_input_string ch len in
      close_in ch; data
    with ex ->
      if Sys.file_exists path then
        failwith @@ Fmt.str "Error loading %S: %a" path Fmt.exn ex
      else failwith @@ Fmt.str "File %S does not exist" path

  let passfile =
    Arg.value
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"Matrix bot password file" ~docv:"MATRIX_PASSWORD_FILE"
         ["matrix-passfile"]
    |> named (fun x -> `Passfile x)

  let max_connections =
    Arg.value
    @@ Arg.opt Arg.(some int) None
    @@ Arg.info ~doc:"Maximal number of concurrent connections to the server"
         ~docv:"MATRIX_MAX_CONNECTIONS" ["matrix-max-connections"]
    |> named (fun x -> `Max_connections x)

  let v
      (`Host host)
      (`Port port)
      (`Scheme scheme)
      (`User user)
      (`Passfile passfile)
      (`Max_connections max_connections) =
    try
      let server = Http.Server.v scheme (Option.get host) port in
      Some
        (Client.v ?max_connections ~server ~device:None ~user:(Option.get user)
           ~pwd:(load_file (Option.get passfile) |> String.trim)
           ())
    with Invalid_argument _ -> None

  let v =
    Term.(const v $ host $ port $ scheme $ user $ passfile $ max_connections)
end

let cmdliner = Cmdliner.v

module Room = struct
  type t = string

  module RC = Current_cache.Output (Room)

  let make
      ctx
      ~alias
      ?(name = alias)
      ?(topic = Current.return "")
      ?power_level_content_override
      () =
    Current.component "matrix room"
    |> let> name = name
       and> topic = topic
       and> alias = alias
       and> power_level_content_override =
         Current.option_seq power_level_content_override in
       RC.set ctx alias {name; topic; power_level_content_override}
end

module PC = Current_cache.Output (Post)

type message = Matrix_common.Events.Event_content.Message.t

let post ctx ~key ~room message =
  Current.component "matrix post"
  |> let> message = message and> room = room and> key = key in
     PC.set ctx {key; room_id= room} message
