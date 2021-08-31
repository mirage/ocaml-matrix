open Lwt.Infix

type t = Client.t

let id = "matrix-post"

module Key = struct
  type t = {key: string; room_id: string}

  let digest {key; room_id} = key ^ "@" ^ room_id
end

module Value = struct
  type t = Matrix_common.Events.Event_content.Message.t

  let digest v =
    Json_encoding.construct Matrix_common.Events.Event_content.Message.encoding
      v
    |> Ezjsonm.value_to_string
    |> Digest.string
    |> Digest.to_hex
end

module Outcome = Current.Unit

let publish t job Key.{room_id; _} message =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Job.log job "Publishing message";
  Client.post ~job ~room_id t message >>= function
  | Ok () -> Lwt.return @@ Ok ()
  | Error s ->
    let msg = Fmt.str "Matrix post failed: %s" s in
    Lwt.return @@ Error (`Msg msg)

let pp f ({Key.key; room_id}, _) = Fmt.pf f "Post %s@%s" key room_id
let auto_cancel = false
