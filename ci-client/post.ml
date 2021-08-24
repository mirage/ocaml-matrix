open Lwt.Infix

type t = Client.t

let id = "matrix-post"

module Key = struct
  type t = {key: string; room_id: string}

  let digest {key; room_id} = key ^ "@" ^ room_id
end

module Value = Current.String
module Outcome = Current.Unit

let publish t job Key.{room_id; _} message =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Job.log job "publishing message %S" message;
  Client.post ~job ~room_id t message >>= function
  | Ok () -> Lwt.return @@ Ok ()
  | Error s ->
    let msg = Fmt.str "Matrix post failed: %s" s in
    Lwt.return @@ Error (`Msg msg)

let pp f ({Key.key; room_id}, value) =
  Fmt.pf f "Post %s@%s: %s" key room_id value

let auto_cancel = false
