open Lwt.Infix

type t = Client.t

let id = "matrix-room"

module Key = Current.String

module Value = struct
  type t = {
    name: string;
    topic: string;
    power_level_content_override:
      Matrix_common.Events.Event_content.Power_levels.t option;
  }

  let digest {name; topic; power_level_content_override} =
    let power_level_string =
      match power_level_content_override with
      | Some power_level_content_override ->
        let power_level_json =
          Json_encoding.construct
            Matrix_common.Events.Event_content.Power_levels.encoding
            power_level_content_override in
        Ezjsonm.value_to_string power_level_json
      | None -> "<>" in
    name ^ ":" ^ topic ^ ":" ^ power_level_string
    |> Digest.string
    |> Digest.to_hex
end

module Outcome = Current.String

let publish t job alias Value.{name; topic; power_level_content_override} =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  Current.Job.log job "obtaining room %S" name;
  let settings = {Client.name; topic; power_level_content_override} in
  Client.get_room ~job ~alias ~settings t >>= function
  | Ok v -> Lwt.return @@ Ok v
  | Error s ->
    let msg = Fmt.str "Matrix room failed: %s" s in
    Lwt.return @@ Error (`Msg msg)

let pp f (alias, _) = Fmt.pf f "Room %S" alias
let auto_cancel = false
