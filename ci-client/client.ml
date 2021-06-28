open Lwt
open Matrix_ctos

type t = {
  host: string;
  port: int;
  device: string option;
  user: string;
  pwd: string;
}

let make_login device_id user password =
  let identifier = Identifier.User (Identifier.User.make ~user ()) in
  let auth =
    Authentication.Password
      (V2 (Authentication.Password.V2.make ~identifier ~password ())) in
  Login.Post.Request.make ~auth ?device_id ()

let login job host port login =
  Current.Job.log job "Login to host %s and port %d" host port;
  let open Login.Post in
  Http.post host port "_matrix/client/r0/login" None login Request.encoding
    Response.encoding None

let logout job host port auth_token =
  Current.Job.log job "Logout from server";
  let open Logout.Logout in
  Http.post host port "_matrix/client/r0/logout" None (Request.make ())
    Request.encoding Response.encoding auth_token

let resolve_alias job host port room_alias =
  Current.Job.log job "Resolving alias `%s` for room name" room_alias;
  let open Room.Resolve_alias in
  Http.get host port
    (Fmt.str "/_matrix/client/r0/directory/room/%s" room_alias)
    None Response.encoding None

let send_message job host port auth_token txn_id message room_id =
  Current.Job.log job "Sending message to room `%s`" room_id;
  let open Room_event.Put.Message_event in
  Http.put host port
    (Fmt.str "/_matrix/client/r0/rooms/%s/send/%s/%s" room_id "m.room.message"
       txn_id)
    None message Request.encoding Response.encoding auth_token
  >>= fun _ -> return_unit

let run job _room ctx msg =
  login job ctx.host ctx.port (make_login ctx.device ctx.user ctx.pwd)
  >>= fun login_response ->
  let auth_token = Login.Post.Response.get_access_token login_response in
  resolve_alias job ctx.host ctx.port "#ocaml-matrix:my.domain.name"
  >>= fun resolved_alias ->
  let room_id =
    Option.get (Room.Resolve_alias.Response.get_room_id resolved_alias) in
  let txn_id = Uuidm.(v `V4 |> to_string) in
  let message =
    Room_event.Put.Message_event.Request.make
      ~event:
        (Matrix_common.Events.Event_content.Message.Text
           (Matrix_common.Events.Event_content.Message.Text.make ~body:msg ()))
      () in
  send_message job ctx.host ctx.port auth_token txn_id message room_id
  >>= fun () ->
  logout job ctx.host ctx.port auth_token >>= fun _ -> Lwt.return_ok ()
