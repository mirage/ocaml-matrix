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

module Store = Irmin_mirage_git.Mem.KV (Content_string)
module Sync = Irmin.Sync (Store)
open Lwt.Infix

let connect ctx remote =
  let config = Irmin_mem.config () in
  Store.Repo.v config >>= Store.master >|= fun repo ->
  repo, Store.remote ~ctx remote

let pull repo upstream =
  Logs.info (fun m -> m "pulling from remote!");
  Sync.pull ~depth:1 repo upstream `Set >|= function
  | Ok `Empty -> Logs.warn (fun m -> m "pulled empty repository")
  | Ok (`Head _ as s) ->
    Logs.info (fun m -> m "ok, pulled %a!" Sync.pp_status s)
  | Error (`Msg e) -> Logs.warn (fun m -> m "pull error %s" e)
  | Error (`Conflict msg) -> Logs.warn (fun m -> m "pull conflict %s" msg)

let push store upstream =
  Logs.info (fun m -> m "pushing to remote!");
  Sync.push store upstream
