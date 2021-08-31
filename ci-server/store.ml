open Lwt.Infix

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

module Store = Irmin_unix.Git.FS.KV (Content_string)

let store =
  Lwt_main.run
    (let config = Irmin_git.config "/tmp/ocaml-matrix" in
     Store.Repo.v config >>= Store.master)
