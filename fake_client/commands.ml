open Matrix
open Http

let to_user_id login = "@" ^ login ^ ":my.domain.name"

let options uri =
  Fmt.(pf stdout "%a: %s\n%!" (styled `Cyan (styled `Bold string)) "Options:" uri);
  Lwt_main.run (Http.options uri [] (fun s -> s))

let versions () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Versions:");
  Lwt_main.run (get Versions.path (Versions.Query.args (Versions.Query.make ())) Versions.Response.encoding Versions.needs_auth)

let well_known () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Well known:");
  Lwt_main.run (get Well_known.path (Well_known.Query.args (Well_known.Query.make ())) Well_known.Response.encoding Well_known.needs_auth)

let sync ?since () =
  let args =
    Sync.Query.make
    ?filter:None
    ?since:since
    ?full_state:None
    ?set_presence:(Some Online)
    ?timeout:None
    ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Sync:");
  Lwt_main.run (get Sync.path (Sync.Query.args args) Sync.Response.encoding Sync.needs_auth)

let register username =
  let args =
    Register.Register.Query.make
      ?kind:None
      ()
  in
  let register =
    Register.Register.Request.make
      ?auth:
        (Some (Authentication.Dummy
          (Authentication.Dummy.make ())))
      ?bind_email:(Some false)
      ?bind_msisdn:(Some false)
      ?username:(Some username)
      ?password:(Some "bob")
      ?device_id:None
      ?initial_device_display_name:None
      ?inhibit_login:(Some false)
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Register:");
  let response = Lwt_main.run (post Register.Register.path (Register.Register.Query.args args) register Register.Register.Request.encoding Register.Register.Response.encoding Register.Register.needs_auth) in
  Option.iter ((:=) auth_token) (Register.Register.Response.get_access_token response)

let gwen =
  Login.Post.Request.make
    ~auth:
      (Authentication.Password
        (Authentication.Password.make
          ~identifier:
            (Identifier.User
              (Identifier.User.make
                ~user:"gwenaelle"
                ()))
          ~password:"bob"
          ()))
    ?device_id:(Some "gwen's phone")
    ?initial_device_display_name:None
    ()

let lili =
  Login.Post.Request.make
    ~auth:
      (Authentication.Password
        (Authentication.Password.make
          ~identifier:
            (Identifier.User
              (Identifier.User.make
                ~user:"lilianne"
                ()))
          ~password:"bob"
          ()))
    ?device_id:None
    ?initial_device_display_name:None
    ()

let error =
  Login.Post.Request.make
    ~auth:
      (Authentication.Token
        (Authentication.Token.make
          ~token:""
          ~txn_id:""
          ()))
    ?device_id:None
    ?initial_device_display_name:None
    ()

let login_get () =
  Login.Get.(
    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login: get");
    Lwt_main.run (get path (Query.args (Query.make ())) Response.encoding needs_auth))

let login login =
  Login.Post.(
    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login:");
    let response = Lwt_main.run (post path (Query.args (Query.make ())) login Request.encoding Response.encoding needs_auth) in
    Option.iter (fun token -> auth_token := token) (Response.get_access_token response);
    response
  )

let logout () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Logout:");
  Lwt_main.run (post Logout.Logout.path (Logout.Logout.Query.args (Logout.Logout.Query.make ())) (Logout.Logout.Request.make ()) Logout.Logout.Request.encoding Logout.Logout.Response.encoding Logout.Logout.needs_auth)

let base_room =
  Room.Create.Request.make
    ?visibility:None
    ?room_alias_name:None
    ?name:None
    ?topic:None
    ?invite:None
    ?invite_3pid:None
    ?room_version:None
    ?creation_content:None
    ?initial_state:None
    ?preset:None
    ?is_direct:None
    ?power_level_content_override:None
    ()

let public_room =
  Room.Create.Request.make
    ?visibility:(Some Public)
    ?room_alias_name:None
    ?name:None
    ?topic:None
    ?invite:None
    ?invite_3pid:None
    ?room_version:None
    ?creation_content:None
    ?initial_state:None
    ?preset:None
    ?is_direct:None
    ()

let create_room room =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Create room:");
  Lwt_main.run (post Room.Create.path (Room.Create.Query.args (Room.Create.Query.make ())) room Room.Create.Request.encoding Room.Create.Response.encoding Room.Create.needs_auth)

let invite room login =
  let invite =
    Joining.Invite.Request.make
      ~user_id:(to_user_id login)
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Invite:");
  Lwt_main.run (post (Joining.Invite.path room) (Joining.Invite.Query.args (Joining.Invite.Query.make ())) invite Joining.Invite.Request.encoding Joining.Invite.Response.encoding Joining.Invite.needs_auth)

let joined () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Joined room:");
  Lwt_main.run (get Joined.path (Joined.Query.args (Joined.Query.make ())) Joined.Response.encoding Joined.needs_auth)

let join room =
  let args =
    Joining.Join.Query.make
    ?server_name:None
    ()
  in
  let join =
    Joining.Join.Request.make
    ?third_party_signed:None
    ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Join:");
  Lwt_main.run (post (Joining.Join.path room) (Joining.Join.Query.args args) join Joining.Join.Request.encoding Joining.Join.Response.encoding Joining.Join.needs_auth)

let leave room =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Leave:");
  Lwt_main.run (post (Leaving.Leave.path room) (Leaving.Leave.Query.args (Leaving.Leave.Query.make ())) (Leaving.Leave.Request.make ()) Leaving.Leave.Request.encoding Leaving.Leave.Response.encoding Leaving.Leave.needs_auth)

let ban room login =
  let invite =
    Banning.Ban.Request.make
    ~user_id:(to_user_id login)
    ?reason:(Some "Grrr")
    ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Invite:");
  Lwt_main.run (post (Banning.Ban.path room) (Banning.Ban.Query.args (Banning.Ban.Query.make ())) invite Banning.Ban.Request.encoding Banning.Ban.Response.encoding Banning.Ban.needs_auth)

let unban room login =
  let invite =
    Banning.Unban.Request.make
    ~user_id:(to_user_id login)
    ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Invite:");
  Lwt_main.run (post (Banning.Unban.path room) (Banning.Unban.Query.args (Banning.Unban.Query.make ())) invite Banning.Unban.Request.encoding Banning.Unban.Response.encoding Banning.Unban.needs_auth)

let get_visibility room =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get room visibility:");
  Lwt_main.run (get (Room_listing.Get_visibility.path room) (Room_listing.Get_visibility.Query.args (Room_listing.Get_visibility.Query.make ())) Room_listing.Get_visibility.Response.encoding Room_listing.Get_visibility.needs_auth)

let private_visibility =
  Room_listing.Set_visibility.Request.make
    ?visibility:(Some Private)
let public_visibility =
  Room_listing.Set_visibility.Request.make
    ?visibility:(Some Public)
let none_visibility =
  Room_listing.Set_visibility.Request.make
    ?visibility:None

let set_visibility room visibility =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Set room visibility:");
  Lwt_main.run (put (Room_listing.Set_visibility.path room) (Room_listing.Set_visibility.Query.args (Room_listing.Set_visibility.Query.make ())) visibility Room_listing.Set_visibility.Request.encoding Room_listing.Set_visibility.Response.encoding Room_listing.Set_visibility.needs_auth)

let get_public_rooms () =
  let args =
    Room_listing.Get_public_rooms.Query.make
      ?limit:None
      ?since:None
      ?server:None
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get public rooms:");
  Lwt_main.run (get Room_listing.Get_public_rooms.path (Room_listing.Get_public_rooms.Query.args args) Room_listing.Get_public_rooms.Response.encoding Room_listing.Get_public_rooms.needs_auth)

let filter_public_rooms () =
  let (args: Room_listing.Filter_public_rooms.Query.t) =
    Room_listing.Filter_public_rooms.Query.make
      ?server:None
      ()
  in
  let (filter: Room_listing.Filter_public_rooms.Request.t) =
    Room_listing.Filter_public_rooms.Request.make
      ?limit:None
      ?since:None
      ?filter:None
      ?include_all_networks:None
      ?third_party_instance_id:None
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Filter public rooms:");
  Lwt_main.run (post Room_listing.Filter_public_rooms.path (Room_listing.Filter_public_rooms.Query.args args) filter Room_listing.Filter_public_rooms.Request.encoding Room_listing.Filter_public_rooms.Response.encoding Room_listing.Filter_public_rooms.needs_auth)

let search_user search =
  let search =
    User_directory.Search.Request.make
      ?limited:None
      ~search_term:search
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Search user:");
  Lwt_main.run (post User_directory.Search.path (User_directory.Search.Query.args (User_directory.Search.Query.make ())) search User_directory.Search.Request.encoding User_directory.Search.Response.encoding User_directory.Search.needs_auth)

let voip_turn_server () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Voip: Turn server");
  Lwt_main.run (get Voip.path (Voip.Query.args (Voip.Query.make ())) Voip.Response.encoding Voip.needs_auth)

let typing room login timeout =
  let user = to_user_id login in
  let typing =
    Typing.Request.make
      ~typing:true
      ?timeout:(Some timeout)
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Typing:");
  Lwt_main.run (put (Typing.path room user) (Typing.Query.args (Typing.Query.make ())) typing Typing.Request.encoding Typing.Response.encoding Typing.needs_auth)

let receipt room event =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Receipt:");
  Lwt_main.run (post (Receipt.path room "m.read" event) (Receipt.Query.args (Receipt.Query.make ())) (Receipt.Request.make ()) Receipt.Request.encoding Receipt.Response.encoding Receipt.needs_auth)

let get_presence login =
  let user = to_user_id login in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get user presence:");
  Lwt_main.run (get (Presence.Get.path user) (Presence.Get.Query.args (Presence.Get.Query.make ())) Presence.Get.Response.encoding Presence.Get.needs_auth)

let online =
  Presence.Post.Request.make
    ~presence:Online
    ?status_msg:(Some "Hey there !")
let offline =
  Presence.Post.Request.make
    ~presence:Offline
    ?status_msg:(Some "Later !")
let unavailable =
  Presence.Post.Request.make
    ~presence:Unavailable
    ?status_msg:(Some "So busy...")

let set_presence login presence =
  let user = to_user_id login in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Set user presence:");
  Lwt_main.run (put (Presence.Post.path user) (Presence.Post.Query.args (Presence.Post.Query.make ())) presence Presence.Post.Request.encoding Presence.Post.Response.encoding Presence.Post.needs_auth)

let file_bob =
  Media.Upload.Request.make
    ~file:"bob"

let upload (file: Media.Upload.Request.t) filename =
  let args =
    Media.Upload.Query.make
      ~filename:filename
      ()
  in
  let header =
    Media.Upload.Query.Header.make
      ~content_type:"test/plain"
      ~content_length:(String.length (Media.Upload.Request.get_file file))
      ()
  in
  let header = Media.Upload.Query.Header.header header in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Upload media:");
  Lwt_main.run (post_r_j ~header Media.Upload.path (Media.Upload.Query.args args) file Media.Upload.Request.to_string Media.Upload.Response.encoding Media.Upload.needs_auth)

let download server media =
  let args =
    Media.Download.Query.make
      ?allow_remote:None
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Upload media:");
  Lwt_main.run (get_r (Media.Download.path server media) (Media.Download.Query.args args) Media.Download.Response.of_string Media.Download.needs_auth)

let config () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Media config:");
  Lwt_main.run (get Media.Config.path (Media.Config.Query.args (Media.Config.Query.make ())) Media.Config.Response.encoding Media.Config.needs_auth)

let send_to_device login =
  let user = to_user_id login in
  let messages =
    Send_to_device.Request.make
      ?messages:(Some [user, []])
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Sending to device:");
  Lwt_main.run (put (Send_to_device.path "m.message" "bobibab") (Send_to_device.Query.args (Send_to_device.Query.make ())) messages Send_to_device.Request.encoding Send_to_device.Response.encoding Send_to_device.needs_auth)

let list_devices () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Listing devices:");
  Lwt_main.run (get (Devices.List.path) (Devices.List.Query.args (Devices.List.Query.make ())) Devices.List.Response.encoding Devices.List.needs_auth)

let name_device name device_id =
  let name =
    Devices.Put.Request.make
      ?display_name:(Some name)
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Naming device:");
  Lwt_main.run (put (Devices.Put.path device_id) (Devices.Put.Query.args (Devices.Put.Query.make ())) name Devices.Put.Request.encoding Devices.Put.Response.encoding Devices.List.needs_auth)

let delete_device device_id =
  let auth =
    Devices.Delete.Request.make
      ?auth:
        (Some
          (Authentication.PasswordV1
            (Authentication.Password.V1.make
              ~user:"gwenaelle"
              ~password:"bob"
              ())))
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Delete device:");
  Lwt_main.run (delete (Devices.Delete.path device_id) (Devices.Delete.Query.args (Devices.Delete.Query.make ())) auth Devices.Delete.Request.encoding Devices.Delete.Response.encoding Devices.Delete.needs_auth)

let notifications () =
  let args =
    Notifications.Query.make
      ?from:None
      ?limit:None
      ?only:None
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Retrieve notifications:");
  Lwt_main.run (get (Notifications.path) (Notifications.Query.args args) Notifications.Response.encoding Notifications.needs_auth)

let get_rules () =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get rules:");
  Lwt_main.run (get (Push_rules.Get_all.path) (Push_rules.Get_all.Query.args (Push_rules.Get_all.Query.make ())) Push_rules.Get_all.Response.encoding Push_rules.Get_all.needs_auth)

let search_term term =
  Search.Request.make
    ?criterias:
      (Some
        (Search.Request.Criteria.make
          ~search_term:term
          ?keys:None
          ?filter:None
          ?order_by:None
          ?event_context:None
          ?include_state:None
          ?groupings:None
          ()))
    ()

let search search =
  let args =
    Search.Query.make
      ?next_batch:None
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Search:");
  Lwt_main.run (post (Search.path) (Search.Query.args args) search Search.Request.encoding Search.Response.encoding Search.needs_auth)

let preview room_id =
  let args =
    Preview.Query.make
      ?from:None
      ?timeout:(Some 100)
      ~room_id:room_id
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Preview:");
  Lwt_main.run (get (Preview.path) (Preview.Query.args args) Preview.Response.encoding Preview.needs_auth)

let add_account_data login =
  let user = to_user_id login in
  let data =
    Account_data.Put.Request.make
      ~data:(Json_encoding.(construct (assoc string) ["custom_key", "custom_data"]))
      ()
  in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Add account data:");
  Lwt_main.run (put (Account_data.Put.path user "type") (Account_data.Put.Query.args (Account_data.Put.Query.make ())) data Account_data.Put.Request.encoding Account_data.Put.Response.encoding Account_data.Put.needs_auth)

let get_account_data login =
  let user = to_user_id login in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Get account data:");
  Lwt_main.run (get (Account_data.Get.path user "type") (Account_data.Get.Query.args (Account_data.Get.Query.make ())) Account_data.Get.Response.encoding Account_data.Get.needs_auth)

let whois login =
  let user = to_user_id login in
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Whois:");
  Lwt_main.run (get (Whois.path user) (Whois.Query.args (Whois.Query.make ())) Whois.Response.encoding Whois.needs_auth)

let device_keys user_id device_id =
  Keys.Upload.Request.make
    ?device_keys:
      (Some
        (Keys.Upload.Request.Device_keys.make
        ~user_id:user_id
        ~device_id:device_id
        ~algorithms:["bob"]
        ~keys:["bob", "bob"]
        ~signatures:[]
        ()))
    ?one_time_keys:(Some [])
    ()

let upload_keys device_keys =
  Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Upload keys:");
  Lwt_main.run (post (Keys.Upload.path) (Keys.Upload.Query.args (Keys.Upload.Query.make ())) device_keys Keys.Upload.Request.encoding Keys.Upload.Response.encoding Keys.Upload.needs_auth)
