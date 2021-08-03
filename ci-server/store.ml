open Lwt.Infix
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let store =
  Lwt_main.run
    (let config = Irmin_git.config "/tmp/ocaml-matrix" in
     Store.Repo.v config >>= Store.master)
