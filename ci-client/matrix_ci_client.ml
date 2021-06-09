open Lwt
open Matrix_ctos
open Cmdliner

type t = {
  device_id: string;
  user: string;
  password: string;
  commit_id: string;
  commit_message: string;
  commit_link: string;
}

let make_login device_id user password =
  let identifier = Identifier.User (Identifier.User.make ~user ()) in
  let auth =
    Authentication.Password
      (V2 (Authentication.Password.V2.make ~identifier ~password ())) in
  Login.Post.Request.make ~auth ~device_id ()

let login login =
  let open Login.Post in
  Http.post "_matrix/client/r0/login" None login Request.encoding
    Response.encoding None

let logout auth_token =
  let open Logout.Logout in
  Http.post "_matrix/client/r0/logout" None (Request.make ()) Request.encoding
    Response.encoding auth_token

let joined_rooms auth_token =
  let open Joined in
  Http.get "/_matrix/client/r0/joined_rooms" None Response.encoding auth_token

let make_message commit_id commit_message commit_link =
  let body =
    Fmt.str "Commit %s was pushed: \"%s\"\nAccess commit here: %s" commit_id
      commit_message commit_link in
  Room_event.Put.Message_event.Request.make
    ~event:
      (Matrix_common.Events.Event_content.Message.Text
         (Matrix_common.Events.Event_content.Message.Text.make ~body ()))
    ()

let send_message auth_token txn_id message room_id =
  let open Room_event.Put.Message_event in
  Http.put
    (Fmt.str "/_matrix/client/r0/rooms/%s/send/%s/%s" room_id "m.room.message"
       txn_id)
    None message Request.encoding Response.encoding auth_token
  >>= fun _ -> return_unit

let run t =
  login (make_login t.device_id t.user t.password) >>= fun login_response ->
  let auth_token = Login.Post.Response.get_access_token login_response in
  joined_rooms auth_token >>= fun joined_response ->
  let joined_rooms = Joined.Response.get_joined joined_response in
  let txn_id = Uuidm.(v `V4 |> to_string) in
  let message = make_message t.commit_id t.commit_message t.commit_link in
  Lwt_list.iter_s (send_message auth_token txn_id message) joined_rooms
  >>= fun () -> logout auth_token

let main () =
  let t =
    {
      device_id= "foo";
      user= "bot_2";
      password= "$crapaud$";
      commit_id= "foo-bar";
      commit_message= "This is a commit";
      commit_link= "github.com";
    } in
  Lwt_main.run (run t)

let setup level =
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs.format_reporter ())

let () =
  let info =
    let doc = "poc of a matrix client for the CI" in
    Term.info "server" ~version:"%%VERSION%%" ~doc in
  Term.exit
  @@ Term.eval (Term.(const main $ Term.(const setup $ Logs_cli.level ())), info)
