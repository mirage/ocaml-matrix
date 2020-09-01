open Matrix
open Http

let () =
  let style_renderer = `Ansi_tty in
  Fmt_tty.setup_std_outputs ~style_renderer ()

let (login: Login.Post.Request.t) =
  { auth =
    Password
      { identifier =
        User
          { user = "gwenaelle" }
      ; password = "bob" }
  ; device_id = None
  ; initial_device_display_name = None}

let (username: Register.Available.Query.t) =
  { username = "lilianne" }

let (new_password: Account.Password.Request.t) =
  { auth =
      { user = "gwenaelle"
      ; password = "bob" }
  ; new_password = "bob"}

let _ =
  try
    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Well-known:");
    let _ = Lwt_main.run (get Well_known.Query.query (Well_known.Query.args ()) Well_known.Response.encoding Well_known.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Versions:");
    let _ = Lwt_main.run (get Versions.Query.query (Versions.Query.args ()) Versions.Response.encoding Versions.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Register:");
    let _ = Lwt_main.run (post Register.Register.Query.query (Register.Register.Query.args ()) register Register.Register.Request.encoding Register.Register.Response.encoding Register.Register.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login: GET");
    let _ = Lwt_main.run (get Login.Get.Query.query (Login.Get.Query.args ()) Login.Get.Response.encoding Login.Get.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login: POST");
    let response = Lwt_main.run (post Login.Post.Query.query (Login.Post.Query.args ()) login Login.Post.Request.encoding Login.Post.Response.encoding Login.Post.needs_auth) in
    auth_token := response.access_token;

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Logout:");
    let () = Lwt_main.run (post Logout.Logout.Query.query (Logout.Logout.Query.args ()) () Logout.Logout.Request.encoding Logout.Logout.Response.encoding Logout.Logout.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Login: POST");
    let response = Lwt_main.run (post Login.Post.Query.query (Login.Post.Query.args ()) login Login.Post.Request.encoding Login.Post.Response.encoding Login.Post.needs_auth) in
    auth_token := response.access_token;

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Logout: all");
    let _ = Lwt_main.run (post Logout.Logout_all.Query.query (Logout.Logout_all.Query.args ()) () Logout.Logout_all.Request.encoding Logout.Logout_all.Response.encoding Logout.Logout_all.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Register: Available:");
    let _ = Lwt_main.run (get Register.Available.Query.query (Register.Available.Query.args username) Register.Available.Response.encoding Register.Available.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Account: password");
    let _ = Lwt_main.run (post Account.Password.Query.query (Account.Password.Query.args ()) new_password Account.Password.Request.encoding Account.Password.Response.encoding Account.Password.needs_auth) in

    let (register: Register.Register.Request.t) =
      { auth =
        Dummy ()
      ; bind_email = false
      ; bind_msisdn = false
      ; password = "bob"
      ; device_id = None
      ; initial_device_display_name = None
      ; inhibit_login = false}
    in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Register:");
    let response = Lwt_main.run (post Register.Register.Query.query (Register.Register.Query.args ()) register Register.Register.Request.encoding Register.Register.Response.encoding Register.Register.needs_auth) in
    match response.access_token with
    | None -> ()
    | Some access_token -> auth_token := access_token;

    let (deactivate: Account.Deactivate.Request.t) =
      { auth =
          { user = response.user_id
          ; password = "bob" }
      ; id_server = None}
    in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Account: deactivtae");
    let _ = Lwt_main.run (post Account.Deactivate.Query.query (Account.Deactivate.Query.args ()) deactivate Account.Deactivate.Request.encoding Account.Deactivate.Response.encoding Account.Deactivate.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Account: Whoami");
    let _ = Lwt_main.run (get Account.Whoami.Query.query (Account.Whoami.Query.args ()) Account.Whoami.Response.encoding Account.Whoami.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Capabilities");
    let _ = Lwt_main.run (get Capabilities.Query.query (Capabilities.Query.args ()) Capabilities.Response.encoding Capabilities.needs_auth) in











    let (alias: Room.Create_alias.Request.t) =
      { room_id = response.room_id }
    in
    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Create room alias:");
    let _ = Lwt_main.run (put (Room.Create_alias.Query.query "#bob:my.domain.name") (Room.Create_alias.Query.args ()) alias Room.Create_alias.Request.encoding Room.Create_alias.Response.encoding Room.Create_alias.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Sync:");
    let _ = Lwt_main.run (get Sync.Sync.Query.query (Sync.Sync.Query.args args) Sync.Sync.Response.encoding Sync.Sync.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Delete room alias:");
    let _ = Lwt_main.run (delete (Room.Resolve_alias.Query.query "#bob:my.domain.name") (Room.Resolve_alias.Query.args ()) ()  Room.Resolve_alias.Request.encoding Room.Resolve_alias.Response.encoding Room.Resolve_alias.needs_auth) in

    Fmt.(pf stdout "%a\n%!" (styled `Cyan (styled `Bold string)) "Sync:");
    let _ = Lwt_main.run (get Sync.Sync.Query.query (Sync.Sync.Query.args args) Sync.Sync.Response.encoding Sync.Sync.needs_auth) in












    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Gwen:");
    let _ = login gwen in
    let _ = sync () in
    let response = joined () in
    let _ = sync () in
    let _ = unban (List.hd response.joined) "lilianne" in
    let _ = invite (List.hd response.joined) "lilianne" in
    let _ = sync () in
    let _ = logout () in

    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Lili:");
    let _ = login lili in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = join (List.hd response.joined) in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = logout () in

    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Gwen:");
    let _ = login gwen in
    let _ = sync () in
    let _ = ban (List.hd response.joined) "lilianne" in
    let _ = sync () in
    let _ = logout () in

    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Lili:");
    let _ = login lili in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = logout () in

    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Gwen:");
    let _ = login gwen in
    let _ = sync () in
    let _ = unban (List.hd response.joined) "lilianne" in
    let _ = sync () in
    let _ = invite (List.hd response.joined) "lilianne" in
    let _ = sync () in
    let _ = logout () in

    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Lili:");
    let _ = login lili in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = join (List.hd response.joined) in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = leave (List.hd response.joined) in
    let _ = sync () in
    let _ = joined () in
    let _ = sync () in
    let _ = logout () in



    Fmt.(pf stdout "%a\n%!" (styled `Red (styled `Bold string)) "Gwen:");
    let _ = login gwen in
    let response = get_public_rooms () in
    let _ = set_visibility (List.hd response.chunk).room_id private_visibility in
    let _ = get_visibility (List.hd response.chunk).room_id in
    let _ = get_public_rooms () in
    let _ = filter_public_rooms () in
    let _ = set_visibility (List.hd response.chunk).room_id none_visibility in
    let _ = get_visibility (List.hd response.chunk).room_id in
    let _ = get_public_rooms () in
    let _ = filter_public_rooms () in





    let _ = login gwen in
    let _ = sync () in
    let response = joined () in
    let _ = List.map (fun r -> typing r "gwenaelle" 10000) response.joined in





    let _ = login gwen in
    let response = sync () in
    let _ = List.map
      (fun (n, (r: Rooms.Joined_room.t)) ->
        match r.timeline with
        | None -> Fmt.pf Fmt.stdout "NO TIMELINE\n"; ()
        | Some t ->
          let e = List.rev t.events |> List.hd in
          let e =
          match e with
          | Room_event t -> t.event_id
          | Message_event t -> t.event_id
          | State_event t -> t.event_id
          in
          receipt n e)
          response.rooms.join in


    let response = upload bob (Some "bob") in
    let uri = Uri.of_string response.content_uri in
    let host = Uri.host uri in
    let host =
      match host with
      | None -> ""
      | Some host -> host
    in
    let path = Uri.path uri in
    let path = String.split_on_char '/' path in
    let _ = download (host) (List.nth path 1) in


    let _ = login gwen in
    let response = list_devices () in
    let _ =
      match response.devices with
      | None -> ()
      | Some devices ->
        let f (device: Devices.Device.t) =
          let device_id = device.device_id in
          let _ = delete_device device_id in
          ()
        in
        List.iter f devices
    in

    let _ = login gwen in
    let _ = search (search_term "tape") in

    ()
  with
    | Errors.Error t -> Fmt.(pf stdout "Encountered error: %a\n%!" (styled `Red (styled `Bold Errors.pp)) t)
