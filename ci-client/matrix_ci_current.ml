open Current.Syntax

module PC = Current_cache.Output(Post)

type context = Post.t
let context ~host ~port ~user ~pwd ~device = Matrix_ci_client.{host; port; user; pwd; device}

let post ctx ~key message =
  Current.component "matrix-post" |>
  let> message = message in
  PC.set ctx key message
