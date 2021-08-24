open Cmdliner
open Matrix_common
open Server_utility.Data
open Server_utility.Store

let setup =
  let setup level =
    let style_renderer = `Ansi_tty in
    Fmt_tty.setup_std_outputs ~style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let open Term in
  const setup $ Logs_cli.level ()

let root_cmd =
  let open Term in
  let info = info "server_utility" in
  let term = ret (const (fun () -> `Help (`Pager, None)) $ setup) in
  term, info

module User = struct
  let f user_id password () =
    (* Verify if the user already exists *)
    let%lwt s_user = Store.find store (Store.Key.v ["users"; user_id]) in
    match s_user with
    | Some _ ->
      Logs.err (fun m -> m "user id %s already exists" user_id);
      Lwt.return 1
    | None ->
      (* Create the new user *)
      let user = User.make ~username:user_id ~password ~devices:[] () in
      let json_user =
        Json_encoding.construct User.encoding user |> Ezjsonm.value_to_string
      in
      let%lwt _ =
        Store.set ~info:Irmin.Info.none store
          (Store.Key.v ["users"; user_id])
          json_user in
      Lwt.return 0

  let user_id =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"user" ~doc:"user id to create")

  let pwd =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"password" ~doc:"user password")

  let cmd =
    let info =
      let doc = "Creates a user." in
      Term.info "user" ~doc in
    let term =
      Term.(app (const Lwt_main.run) (const f $ user_id $ pwd $ setup)) in
    term, info
end

module Room = struct
  let f alias user_id () =
    let%lwt s_user = Store.find store (Store.Key.v ["aliases"; alias]) in
    match s_user with
    | Some _ ->
      Logs.err (fun m -> m "room alias %s already exist" alias);
      Lwt.return 1
    | None -> (
      let%lwt s_user = Store.find store (Store.Key.v ["users"; user_id]) in
      match s_user with
      | None ->
        Logs.err (fun m -> m "user id %s does not exist" user_id);
        Lwt.return 1
      | Some _ ->
        let room_id = "$" ^ Uuidm.(v `V4 |> to_string) in
        (* Create the head for the message feed *)
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "messages_id"; "head"])
            "" in
        (* Create the state events of the room *)
        (* power_level *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Power_levels
                           (Events.Event_content.Power_levels.make
                              ?users:(Some [user_id, 100])
                              ?users_default:(Some (-1)) ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.power_levels"])
            json_event in
        (* history_visibility *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.History_visibility
                           (Events.Event_content.History_visibility.make
                              ~visibility:Shared ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v
               ["rooms"; room_id; "state"; "m.room.history_visibility"])
            json_event in
        (* create *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Create
                           (Events.Event_content.Create.make ~creator:user_id
                              ?room_version:(Some "4") ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.create"])
            json_event in
        (* member *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Member
                           (Events.Event_content.Member.make ~avatar_url:None
                              ~displayname:(Some user_id) ~membership:Join ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:user_id () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.member"; user_id])
            json_event in
        (* join_rules *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Join_rules
                           (Events.Event_content.Join_rules.make
                              ~join_rule:Public ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.join_rules"])
            json_event in
        (* name *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Name
                           (Events.Event_content.Name.make ~name:alias ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.name"])
            json_event in
        (* canonical_alias *)
        let id = "$" ^ Uuidm.(v `V4 |> to_string) in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.Event_content.Canonical_alias
                           (Events.Event_content.Canonical_alias.make
                              ~alias:(Some alias) ()))
                      ())
                 ~event_id:id ~sender:user_id ())
            ~state_key:"" () in
        let json_event =
          Json_encoding.construct Events.State_event.encoding event
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["rooms"; room_id; "state"; "m.room.canonical_alias"])
            json_event in
        (* Saving the alias in the aliases folder *)
        let json_alias =
          Alias.make ~room_id ()
          |> Json_encoding.construct Alias.encoding
          |> Ezjsonm.value_to_string in
        let%lwt _ =
          Store.set ~info:Irmin.Info.none store
            (Store.Key.v ["aliases"; alias])
            json_alias in
        Lwt.return 0)

  let alias =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"alias" ~doc:"alias used for retrieving the created room")

  let user_id =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"user" ~doc:"user added into the room")

  let cmd =
    let info =
      let doc = "Creates a room for a project." in
      Term.info "room" ~doc in
    let term =
      Term.(app (const Lwt_main.run) (const f $ alias $ user_id $ setup)) in
    term, info
end

let () =
  let open Term in
  exit_status @@ eval_choice root_cmd [User.cmd; Room.cmd]
