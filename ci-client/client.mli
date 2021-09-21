type t

val v :
  ?max_connections:int ->
  server:Http.Server.t ->
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

val get_room :
  job:Current.Job.t ->
  alias:string ->
  name:string ->
  topic:string ->
  t ->
  (string, string) result Lwt.t
