type t = {
  server: Http.Server.t;
  device: string option;
  user: string;
  pwd: string;
}

val post :
  job:Current.Job.t ->
  room_id:string ->
  t ->
  string ->
  (unit, string) result Lwt.t

val get_room :
  job:Current.Job.t ->
  alias:string ->
  name:string ->
  topic:string ->
  t ->
  (string, string) result Lwt.t
