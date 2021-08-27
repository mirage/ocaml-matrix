open Current.Syntax

let src = Logs.Src.create "matrix.current_matrix" ~doc:"OCurrent matrix plugin"

module Log = (val Logs.src_log src : Logs.LOG)

type context = Post.t

let context ~host ~port ~scheme ~user ~pwd ~device =
  let server = Http.Server.v scheme host port in
  Client.v ~server ~user ~pwd ~device

module Room = struct
  type t = string

  module RC = Current_cache.Output (Room)

  let make
      ctx ~alias ?(name = Current.return alias) ?(topic = Current.return "") ()
      =
    Current.component "Matrix room"
    |> let> name = name and> topic = topic in
       RC.set ctx alias {name; topic}
end

module PC = Current_cache.Output (Post)

let post ctx ~key ~room message =
  Current.component "matrix-post"
  |> let> message = message and> room = room in
     PC.set ctx {key; room_id= room} message
