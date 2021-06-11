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

let login host login =
  let open Login.Post in
  Http.post host "_matrix/client/r0/login" None login Request.encoding
    Response.encoding None

let logout host auth_token =
  let open Logout.Logout in
  Http.post host "_matrix/client/r0/logout" None (Request.make ()) Request.encoding
    Response.encoding auth_token

let joined_rooms host auth_token =
  let open Joined in
  Http.get host "/_matrix/client/r0/joined_rooms" None Response.encoding auth_token

let send_message host auth_token txn_id message room_id =
  let open Room_event.Put.Message_event in
  Http.put host
    (Fmt.str "/_matrix/client/r0/rooms/%s/send/%s/%s" room_id "m.room.message"
       txn_id)
    None message Request.encoding Response.encoding auth_token
  >>= fun _ -> return_unit

let run ctx msg =
  login (ctx.host, ctx.port) (make_login ctx.device ctx.user ctx.pwd) >>= fun login_response ->
  let auth_token = Login.Post.Response.get_access_token login_response in
  joined_rooms (ctx.host, ctx.port) auth_token >>= fun joined_response ->
  let joined_rooms = Joined.Response.get_joined joined_response in
  let txn_id = Uuidm.(v `V4 |> to_string) in
  let message =
    Room_event.Put.Message_event.Request.make
      ~event:
        (Matrix_common.Events.Event_content.Message.Text
          (Matrix_common.Events.Event_content.Message.Text.make ~body:msg ()))
      ()
  in
  Lwt_list.iter_s (send_message (ctx.host, ctx.port) auth_token txn_id message) joined_rooms
  >>= fun () -> logout (ctx.host, ctx.port) auth_token
  >>= fun _ -> Lwt.return_ok ()
