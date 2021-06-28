open Current.Syntax

let src = Logs.Src.create "matrix.current_matrix" ~doc:"OCurrent matrix plugin"
module Log = (val Logs.src_log src : Logs.LOG)

module PC = Current_cache.Output(Post)

type context = Post.t
let context ~host ~port ~user ~pwd ~device = Client.{host; port; user; pwd; device}

let post ctx ~key message =
  Current.component "matrix-post" |>
  let> message = message in
  PC.set ctx key message
