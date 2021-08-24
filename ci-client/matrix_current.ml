open Current.Syntax

let src = Logs.Src.create "matrix.current_matrix" ~doc:"OCurrent matrix plugin"

module Log = (val Logs.src_log src : Logs.LOG)
module PC = Current_cache.Output (Post)

type context = Post.t

let context ~host ~port ~scheme ~user ~pwd ~device =
  let server = Http.Server.v scheme host port in
  Client.{server; user; pwd; device}

let post ctx ~key message =
  Current.component "matrix-post"
  |> let> message = message in
     PC.set ctx key message
