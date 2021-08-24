open Lwt.Infix

type t = Client.t

let id = "matrix-room"

module Key = Current.String

module Value = struct
  type t = {name: string; topic: string}

  let digest {name; topic} = name ^ ":" ^ topic
end

module Outcome = Current.String

let publish t job alias Value.{name; topic} =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Job.log job "obtaining room %S" name;
  Client.get_room ~job ~alias ~name ~topic t >>= function
  | Ok v -> Lwt.return @@ Ok v
  | Error s ->
    let msg = Fmt.str "Matrix room failed: %s" s in
    Lwt.return @@ Error (`Msg msg)

let pp f (alias, _) = Fmt.pf f "Room %S" alias
let auto_cancel = false
