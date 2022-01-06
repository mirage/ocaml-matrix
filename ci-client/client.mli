type t

module Server = Http.Server

val v :
  ?max_connections:int ->
  server:Server.t ->
  device:string option ->
  user:string ->
  pwd:string ->
  unit ->
  t

val post :
  job:Current.Job.t ->
  room_id:string ->
  t ->
  Matrix_common.Events.Event_content.Message.t ->
  (unit, string) result Lwt.t

type settings = {
  name: string;
  topic: string;
  power_level_content_override:
    Matrix_common.Events.Event_content.Power_levels.t option;
}

val get_room :
  job:Current.Job.t ->
  alias:string ->
  settings:settings ->
  t ->
  (string, string) result Lwt.t
