open Lwt
open Lwt.Syntax
open Matrix_ctos
module Room = Matrix_ctos.Room

type t = {
  server: Http.Server.t;
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

let login job server login =
  Current.Job.log job "Login to %a" Http.Server.pp server;
  let open Login.Post in
  Http.post server "_matrix/client/r0/login" None login Request.encoding
    Response.encoding None

let logout job server auth_token =
  Current.Job.log job "Logout from server";
  let open Logout.Logout in
  Http.post server "_matrix/client/r0/logout" None (Request.make ())
    Request.encoding Response.encoding auth_token

let resolve_alias job server room_alias =
  Current.Job.log job "Resolving alias `%s` for room name" room_alias;
  let open Room.Resolve_alias in
  Http.get server
    (Fmt.str "/_matrix/client/r0/directory/room/%s" room_alias)
    None Response.encoding None

let send_message job server auth_token txn_id message room_id =
  Current.Job.log job "Sending message to room `%s`" room_id;
  let open Room_event.Put.Message_event in
  Http.put server
    (Fmt.str "/_matrix/client/r0/rooms/%s/send/%s/%s" room_id "m.room.message"
       txn_id)
    None message Request.encoding Response.encoding auth_token
  >|= ignore

let send_state job server auth_token room_id (state_kind, state, state_key) =
  Current.Job.log job "Sending state to room `%s`: %s" room_id state_kind;
  let open Room_event.Put.State_event in
  Http.put server
    (Fmt.str "/_matrix/client/r0/rooms/%s/state/%s/%s" room_id state_kind
       state_key)
    None state Request.encoding Response.encoding auth_token
  >|= ignore

let post ~job ~room_id ctx message =
  let* login_response =
    login job ctx.server (make_login ctx.device ctx.user ctx.pwd)
  in
  let auth_token = Login.Post.Response.get_access_token login_response in
  let txn_id = Uuidm.(v `V4 |> to_string) in
  let message =
    Room_event.Put.Message_event.Request.make
      ~event:
        (Matrix_common.Events.Event_content.Message.Text
           (Matrix_common.Events.Event_content.Message.Text.make ~body:message
              ()))
      () in
  let* () = send_message job ctx.server auth_token txn_id message room_id in
  let+ _ = logout job ctx.server auth_token in
  Ok ()

let create_room ~job server auth_token room (name, topic) =
  let power_level_content_override =
    Matrix_common.Events.Event_content.Power_levels.make ~events_default:100 ()
  in
  Current.Job.log job "Creating room `%s`" room;
  let open Room.Create in
  Http.post server "/_matrix/client/r0/createRoom" None
    (Request.make ~visibility:Public ~room_alias_name:room ~name ~topic
       ~power_level_content_override ())
    Request.encoding Response.encoding auth_token

let update_room ~job server auth_token room_id (name, topic) =
  Current.Job.log job "Updating room `%s`" room_id;
  let state_name =
    Room_event.Put.State_event.Request.make
      ~event:(Name (Matrix_common.Events.Event_content.Name.make ~name ()))
      () in
  let* () =
    send_state job server auth_token room_id ("m.room.name", state_name, "")
  in
  let state_topic =
    Room_event.Put.State_event.Request.make
      ~event:(Topic (Matrix_common.Events.Event_content.Topic.make ~topic ()))
      () in
  send_state job server auth_token room_id ("m.room.topic", state_topic, "")

let get_room ~job ~alias ~name ~topic ctx =
  let* login_response =
    login job ctx.server (make_login ctx.device ctx.user ctx.pwd)
  in
  let auth_token = Login.Post.Response.get_access_token login_response in
  (* we look for the room *)
  let* existing_room_alias =
    Lwt.catch
      (fun () ->
        let+ alias =
          resolve_alias job ctx.server ("#" ^ alias ^ ":" ^ ctx.server.host)
        in
        Room.Resolve_alias.Response.get_room_id alias)
      (fun _ -> Lwt.return_none)
  in
  let+ room_id =
    match existing_room_alias with
    | None ->
      let+ create_room_response =
        create_room ~job ctx.server auth_token alias (name, topic)
      in
      Room.Create.Response.get_room_id create_room_response
    | Some room_id ->
      Current.Job.log job
        "Room already exists, making sure it has the correct name and topic.";
      let+ () = update_room ~job ctx.server auth_token room_id (name, topic) in
      room_id
  in
  Current.Job.log job "Room id: %s" room_id;
  Ok room_id
