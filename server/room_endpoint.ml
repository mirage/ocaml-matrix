open Lwt.Infix
open Json_encoding
open Matrix_ctos
open Store
open Helper

let set_state_event room_id event_type state_key event_id =
  if state_key = ""
  then
    Store.set store Key.(v "rooms" / room_id / "state" / event_type / "this_is_a_temporary_escaping") event_id
  else
    Store.set store Key.(v "rooms" / room_id / "state" / event_type / state_key) event_id

let get_state_event room_id event_type state_key =
  if state_key = ""
  then
    Store.get store Key.(v "rooms" / room_id / "state" / event_type / "this_is_a_temporary_escaping")
  else
    Store.get store Key.(v "rooms" / room_id / "state" / event_type / state_key)

let create_room =
  let open Room.Create in
  let f () request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let user_id = username_to_user_id username in
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let room_id = room_id () in
        Store.set store Key.(v "rooms" / room_id / "messages" / "head") "" >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok () ->
            let id = event_id () in
            let event =
              State_events.make
                ~event:
                  (State_events.State_event.Power_levels
                     (State_events.State_event.Power_levels.make
                        ~event:
                          (Room_events.Room_event.Power_levels.make
                             ?ban:(Some 50)
                             ?events:(Some
                                        [ "m.room.name", 50
                                        ; "m.room.power_levels", 100
                                        ; "m.room.history_visibility", 100
                                        ; "m.room.canonical_alias", 50
                                        ; "m.room.avatar", 50
                                        ])
                             ?events_default:(Some 0)
                             ?invite:(Some 0)
                             ?kick:(Some 50)
                             ?redact:(Some 50)
                             ?state_default:(Some 0)
                             ?users:(Some [user_id, 100])
                             ?users_default:(Some 0)
                             ())
                        ()))
                ?event_id:(Some id)
                ?sender:(Some user_id)
                ?origin_server_ts:(Some (time ()))
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                set_state_event room_id "m.room.power_levels" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    let id = event_id () in
                    let event =
                      State_events.make
                        ~event:
                          (State_events.State_event.History_visibility
                             (State_events.State_event.History_visibility.make
                                ~event:
                                  (Room_events.Room_event.History_visibility.make
                                     ~visibility:Room_events.Room_event.History_visibility.Shared
                                     ())
                                ()))
                        ?event_id:(Some id)
                        ?sender:(Some user_id)
                        ?origin_server_ts:(Some (time ()))
                        ()
                    in
                    let encoded_event = Json_encoding.construct State_events.encoding event in
                    Event_store.set event_store Key.(v id) encoded_event >>=
                    (function
                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                      | Ok () ->
                        set_state_event room_id "m.room.history_visibility" (State_events.get_state_key event) id >>=
                        (function
                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                          | Ok () ->
                            let id = event_id () in
                            let event =
                              State_events.make
                                ~event:
                                  (State_events.State_event.Create
                                     (State_events.State_event.Create.make
                                        ~event:
                                          (Room_events.Room_event.Create.make
                                             ~creator:username
                                             ?room_version:(Some Const.room_version)
                                             ())
                                        ()))
                                ?event_id:(Some id)
                                ?sender:(Some user_id)
                                ?origin_server_ts:(Some (time ()))
                                ()
                            in
                            let encoded_event = Json_encoding.construct State_events.encoding event in
                            Event_store.set event_store Key.(v id) encoded_event >>=
                            (function
                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                              | Ok () ->
                                set_state_event room_id "m.room.create" (State_events.get_state_key event) id >>=
                                (function
                                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                  | Ok () ->
                                    let id = event_id () in
                                    let event =
                                      State_events.make
                                        ~event:
                                          (State_events.State_event.Member
                                             (State_events.State_event.Member.make
                                                ~user_id:user_id
                                                ~event:
                                                  (Room_events.Room_event.Member.make
                                                     ~avatar_url:None
                                                     ~displayname:(Some user_id)
                                                     ~membership:Room_events.Membership.Join
                                                     ())
                                                ()))
                                        ?event_id:(Some id)
                                        ?sender:(Some user_id)
                                        ?origin_server_ts:(Some (time ()))
                                        ()
                                    in
                                    let encoded_event = Json_encoding.construct State_events.encoding event in
                                    Event_store.set event_store Key.(v id) encoded_event >>=
                                    (function
                                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                      | Ok () ->
                                        set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                                        (function
                                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                          | Ok () ->
                                            let join_rule =
                                              match Request.get_preset request with
                                              | Some Request.Preset.Public -> Room_events.Room_event.Join_rules.Public
                                              | Some Request.Preset.Private -> Room_events.Room_event.Join_rules.Invite
                                              | Some Request.Preset.Trusted_private -> Room_events.Room_event.Join_rules.Invite
                                              | None ->
                                                (match Request.get_visibility request with
                                                 | Some Room.Visibility.Public -> Room_events.Room_event.Join_rules.Public
                                                 | Some Room.Visibility.Private -> Room_events.Room_event.Join_rules.Invite
                                                 | None -> Room_events.Room_event.Join_rules.Invite)
                                            in
                                            let id = event_id () in
                                            let event =
                                              State_events.make
                                                ~event:
                                                  (State_events.State_event.Join_rules
                                                     (State_events.State_event.Join_rules.make
                                                        ~event:
                                                          (Room_events.Room_event.Join_rules.make
                                                             ~join_rule
                                                             ())
                                                        ()))
                                                ?event_id:(Some id)
                                                ?sender:(Some user_id)
                                                ?origin_server_ts:(Some (time ()))
                                                ()
                                            in
                                            let encoded_event = Json_encoding.construct State_events.encoding event in
                                            Event_store.set event_store Key.(v id) encoded_event >>=
                                            (function
                                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                              | Ok () ->
                                                set_state_event room_id "m.room.join_rules" (State_events.get_state_key event) id >>=
                                                (function
                                                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                                  | Ok () ->
                                                    (match Request.get_name request with
                                                     | Some room_name ->
                                                       let id = event_id () in
                                                       let event =
                                                         State_events.make
                                                           ~event:
                                                             (State_events.State_event.Name
                                                                (State_events.State_event.Name.make
                                                                   ~event:
                                                                     (Message_event.Message_event.Name.make
                                                                        ~name:room_name
                                                                        ())
                                                                   ()))
                                                           ?event_id:(Some id)
                                                           ?sender:(Some user_id)
                                                           ?origin_server_ts:(Some (time ()))
                                                           ()
                                                       in
                                                       let encoded_event = Json_encoding.construct State_events.encoding event in
                                                       Event_store.set event_store Key.(v id) encoded_event >>=
                                                       (function
                                                         | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                                         | Ok () ->
                                                           set_state_event room_id "m.room.name" (State_events.get_state_key event) id >>=
                                                           (function
                                                             | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                                             | Ok () -> Lwt.return_ok ()))
                                                     | None -> Lwt.return_ok ()) >>=
                                                    (function
                                                      | Error err -> Lwt.return err
                                                      | Ok () ->
                                                        (match Request.get_topic request with
                                                         | Some room_topic ->
                                                           let id = event_id () in
                                                           let event =
                                                             State_events.make
                                                               ~event:
                                                                 (State_events.State_event.Topic
                                                                    (State_events.State_event.Topic.make
                                                                       ~event:
                                                                         (Message_event.Message_event.Topic.make
                                                                            ~topic:room_topic
                                                                            ())
                                                                       ()))
                                                               ?event_id:(Some id)
                                                               ?sender:(Some user_id)
                                                               ?origin_server_ts:(Some (time ()))
                                                               ()
                                                           in
                                                           let encoded_event = Json_encoding.construct State_events.encoding event in
                                                           Event_store.set event_store Key.(v id) encoded_event >>=
                                                           (function
                                                             | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                                             | Ok () ->
                                                               set_state_event room_id "m.room.topic" (State_events.get_state_key event) id >>=
                                                               (function
                                                                 | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                                                                 | Ok () -> Lwt.return_ok ()))
                                                         | None -> Lwt.return_ok ()) >>=
                                                        (function
                                                          | Error err -> Lwt.return err
                                                          | Ok () ->
                                                            let response =
                                                              Response.make
                                                                ~room_id:room_id
                                                                ()
                                                            in
                                                            let response =
                                                              construct Response.encoding response |>
                                                              Ezjsonm.value_to_string
                                                            in
                                                            Lwt.return (`OK, response)))))))))))))))
  in
  needs_auth, f

let room_members =
  let open Room_event.Get.Members in
  let f ((), room_id) _ _ _ =
    Store.list store Key.(v "rooms" / room_id / "state" / "m.room.member") >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok state_keys ->
        Lwt_list.filter_map_p
          (fun (state_key, _) ->
             get_state_event room_id "m.room_member" state_key >>=
             (function
               | Error _ -> Lwt.return_none
               | Ok event_id ->
                 Event_store.get event_store Key.(v event_id) >>=
                 (function
                   | Error _ -> Lwt.return_none
                   | Ok event -> Lwt.return_some event))) state_keys >>=
        (fun events ->
           let events = List.map (destruct State_events.encoding) events in
           let response =
             Response.make
               ~chunk:events
               ()
           in
           let response =
             construct Response.encoding response |>
             Ezjsonm.value_to_string
           in
           Lwt.return (`OK, response)))
  in
  needs_auth, f

let room_typing =
  let open Typing in
  let f (((), room_id), _user_id) request _ token = (* maybe do something about _user_id *)
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        match Request.get_typing request, Request.get_timeout request with
        | true, Some timeout ->
          let until = Unix.time () +. (float_of_int timeout /. 1000.) in
          Store.set store Key.(v "rooms" / room_id / "ephemeral" / "typing" / username) (string_of_float until) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
            | Ok () ->
              let response =
                Response.make ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response))
        | _ ->
          Store.exists store Key.(v "rooms" / room_id / "ephemeral" / "typing" / username) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
            | Ok (Some _) ->
              Store.remove store Key.(v "rooms" / room_id / "ephemeral" / "typing" / username) >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                | Ok () ->
                  let response =
                    Response.make ()
                  in
                  let response =
                    construct Response.encoding response |>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response))
            | Ok None ->
              let response =
                Response.make ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response)))
  in
  needs_auth, f

let read_markers =
  let open Fully_read in
  let f ((), room_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        Store.set store Key.(v "rooms" / room_id / "ephemeral" / "fully_read" / username) (Request.get_fully_read request) >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok () ->
            let response =
              Response.make ()
            in
            let response =
              construct Response.encoding response |>
              Ezjsonm.value_to_string
            in
            Lwt.return (`OK, response)))
  in
  needs_auth, f

let public_rooms =
  let open Room_listing.Get_public_rooms in
  let f () _ _ _ =
    Store.list store Key.(v "rooms") >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok rooms ->
        Lwt_list.filter_map_p
          (fun (room_id, _) ->
             get_state_event room_id "m.room.join_rules" "" >>=
             (function
               | Error _ -> Lwt.return_none
               | Ok event_id ->
                 Event_store.get event_store Key.(v event_id) >>=
                 (function
                   | Error _ -> Lwt.return_none
                   | Ok event ->
                     Store.list store Key.(v "rooms" / room_id / "state" / "m.room.member") >>=
                     (function
                       | Error _ -> Lwt.return_none
                       | Ok user_ids ->
                         Lwt_list.fold_left_s
                           (fun acc (user_id, _) ->
                              get_state_event room_id "m.room.member" user_id >>=
                              (function
                                | Error _ -> Lwt.return acc
                                | Ok event_id ->
                                  Event_store.get event_store Key.(v event_id) >>=
                                  (function
                                    | Error _ -> Lwt.return acc
                                    | Ok event ->
                                      let event = destruct State_events.encoding event in
                                      (match State_events.get_event event with
                                       | State_events.State_event.Member event ->
                                         let event = State_events.State_event.Member.get_event event in
                                         (match Room_events.Room_event.Member.get_membership event with
                                          | Room_events.Membership.Join -> Lwt.return (acc + 1)
                                          | _ -> Lwt.return acc)
                                       | _ -> Lwt.return acc)))) 0 user_ids >>=
                         (fun room_members ->
                            get_state_event room_id "m.room.name" "" >>=
                            (function
                              | Error _ -> Lwt.return_none
                              | Ok event_id ->
                                Event_store.get event_store Key.(v event_id) >>=
                                (function
                                  | Error _ -> Lwt.return_none
                                  | Ok event ->
                                    let event = destruct State_events.encoding event in
                                    (match State_events.get_event event with
                                     | State_events.State_event.Name event ->
                                       let event = State_events.State_event.Name.get_event event in
                                       Lwt.return_some (Message_event.Message_event.Name.get_name event)
                                     | _ -> Lwt.return_none))) >>=
                            (fun room_name ->
                               get_state_event room_id "m.room.topic" "" >>=
                               (function
                                 | Error _ -> Lwt.return_none
                                 | Ok event_id ->
                                   Event_store.get event_store Key.(v event_id) >>=
                                   (function
                                     | Error _ -> Lwt.return_none
                                     | Ok event ->
                                       let event = destruct State_events.encoding event in
                                       (match State_events.get_event event with
                                        | State_events.State_event.Name event ->
                                          let event = State_events.State_event.Name.get_event event in
                                          Lwt.return_some (Message_event.Message_event.Name.get_name event)
                                        | _ -> Lwt.return_none))) >>=
                               (fun room_topic ->
                                  let event = destruct State_events.encoding event in
                                  (match State_events.get_event event with
                                   | State_events.State_event.Join_rules event ->
                                     let event = State_events.State_event.Join_rules.get_event event in
                                     (match Room_events.Room_event.Join_rules.get_join_rule event with
                                      | Public ->
                                        let room_summary =
                                          Response.Public_rooms_chunk.make
                                            ?aliases:(Some [])
                                            ?canonical_alias:None
                                            ?name:room_name
                                            ~num_joined_members:room_members
                                            ~room_id:room_id
                                            ?topic:room_topic
                                            ~world_readable:false
                                            ~guest_can_join:false
                                            ?avatar_url:None
                                            ?federate:(Some false)
                                            ()
                                        in
                                        Lwt.return_some room_summary
                                      | _ -> Lwt.return_none)
                                   | _ -> Lwt.return_none)))))))) rooms >>=
        (fun rooms ->
           let response =
             Response.make
               ~chunk:rooms
               ()
           in
           let response =
             construct Response.encoding response |>
             Ezjsonm.value_to_string
           in
           Lwt.return (`OK, response)))
  in
  needs_auth, f

let get_room_alias =
  let open Room.Resolve_alias in
  let f ((), alias) _ _ _ =
    let response =
      Response.make
        ?room_id:None
        ?servers:None
        ()
    in
    let _response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`Not_found, error "M_NOT_FOUND" (Fmt.str "Room alias %s not found." alias))
  in
  needs_auth, f

let invite_to_room =
  let open Joining.Invite in
  let f ((), room_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let user_id = Request.get_user_id request in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return_ok ()
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Join -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s had already joined the room" user_id))
                    | Ban -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id))
                    | _ -> Lwt.return_ok ())
                 | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))) >>=
        (function
          | Error err -> Lwt.return err
          | Ok () ->
            let id = event_id () in
            let event =
              State_events.make
                ~event:
                  (State_events.State_event.Member
                     (State_events.State_event.Member.make
                        ~user_id:user_id
                        ~event:
                          (Room_events.Room_event.Member.make
                             ~avatar_url:None
                             ~displayname:(Some user_id)
                             ~membership:Room_events.Membership.Invite
                             ())
                        ()))
                ?event_id:(Some id)
                ?sender:(Some (username_to_user_id username))
                ?origin_server_ts:(Some (time ()))
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    let response =
                      Response.make
                        ()
                    in
                    let response =
                      construct Response.encoding response |>
                      Ezjsonm.value_to_string
                    in
                    Lwt.return (`OK, response)))))
  in
  needs_auth, f

let join_room =
  let open Joining.Join in
  let f ((), room_id) _ _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let user_id = username_to_user_id username in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return_ok ()
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Join -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s had already joined the room" user_id))
                    | Ban -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id))
                    | _ -> Lwt.return_ok ())
                 | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))) >>=
        (function
          | Error err -> Lwt.return err
          | Ok () ->
            let id = event_id () in
            let event =
              State_events.make
                ~event:
                  (State_events.State_event.Member
                     (State_events.State_event.Member.make
                        ~user_id:user_id
                        ~event:
                          (Room_events.Room_event.Member.make
                             ~avatar_url:None
                             ~displayname:(Some user_id)
                             ~membership:Room_events.Membership.Join
                             ())
                        ()))
                ?event_id:(Some id)
                ?sender:(Some user_id)
                ?origin_server_ts:(Some (time ()))
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    let response =
                      Response.make
                        ~room_id:room_id
                        ()
                    in
                    let response =
                      construct Response.encoding response |>
                      Ezjsonm.value_to_string
                    in
                    Lwt.return (`OK, response)))))
  in
  needs_auth, f

let leave_room =
  let open Leaving.Leave in
  let f ((), room_id) _ _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let user_id = username_to_user_id username in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return_ok ()
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Join -> Lwt.return_ok ()
                    | _ -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id)))
                 | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))) >>=
        (function
          | Error err -> Lwt.return err
          | Ok () ->
            let id = event_id () in
            let event =
              State_events.make
                ~event:
                  (State_events.State_event.Member
                     (State_events.State_event.Member.make
                        ~user_id:user_id
                        ~event:
                          (Room_events.Room_event.Member.make
                             ~avatar_url:None
                             ~displayname:(Some user_id)
                             ~membership:Room_events.Membership.Leave
                             ())
                        ()))
                ?event_id:(Some id)
                ?sender:(Some user_id)
                ?origin_server_ts:(Some (time ()))
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    let response =
                      Response.make
                        ()
                    in
                    let response =
                      construct Response.encoding response |>
                      Ezjsonm.value_to_string
                    in
                    Lwt.return (`OK, response)))))
  in
  needs_auth, f

let forget_room =
  let open Leaving.Leave in
  let f ((), room_id) _ _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let user_id = username_to_user_id username in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Leave ->
                      Event_store.remove event_store Key.(v event_id) >>=
                      (function
                        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                        | Ok () ->
                          Store.remove store Key.(v "rooms" / room_id / "state" / "m.room.member" / user_id) >>=
                          (function
                            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                            | Ok () ->
                              let response =
                                Response.make
                                  ()
                              in
                              let response =
                                construct Response.encoding response |>
                                Ezjsonm.value_to_string
                              in
                              Lwt.return (`OK, response)))
                    | _ -> Lwt.return (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id)))
                 | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))))
  in
  needs_auth, f

let kick_room =
  let open Leaving.Kick in
  let f ((), room_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some _username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let user_id = Request.get_user_id request in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return_ok ()
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Join -> Lwt.return_ok ()
                    | _ -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id)))
                 | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))) >>=
        (function
          | Error err -> Lwt.return err
          | Ok () ->
            let id = event_id () in
            let event =
              State_events.make
                ~event:
                  (State_events.State_event.Member
                     (State_events.State_event.Member.make
                        ~user_id:user_id
                        ~event:
                          (Room_events.Room_event.Member.make
                             ~avatar_url:None
                             ~displayname:(Some user_id)
                             ~membership:Room_events.Membership.Leave
                             ())
                        ()))
                ?event_id:(Some id)
                ?sender:(Some user_id)
                ?origin_server_ts:(Some (time ()))
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    let response =
                      Response.make
                        ()
                    in
                    let response =
                      construct Response.encoding response |>
                      Ezjsonm.value_to_string
                    in
                    Lwt.return (`OK, response)))))
  in
  needs_auth, f

let ban_room =
  let open Banning.Ban in
  let f ((), room_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some _username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let user_id = Request.get_user_id request in
        let id = event_id () in
        let event =
          State_events.make
            ~event:
              (State_events.State_event.Member
                 (State_events.State_event.Member.make
                    ~user_id:user_id
                    ~event:
                      (Room_events.Room_event.Member.make
                         ~avatar_url:None
                         ~displayname:(Some user_id)
                         ~membership:Room_events.Membership.Ban
                         ())
                    ()))
            ?event_id:(Some id)
            ?sender:(Some user_id)
            ?origin_server_ts:(Some (time ()))
            ()
        in
        let encoded_event = Json_encoding.construct State_events.encoding event in
        Event_store.set event_store Key.(v id) encoded_event >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok () ->
            set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                let response =
                  Response.make
                    ()
                in
                let response =
                  construct Response.encoding response |>
                  Ezjsonm.value_to_string
                in
                Lwt.return (`OK, response))))
  in
  needs_auth, f

let unban_room =
  let open Leaving.Kick in
  let f ((), room_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some _username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let user_id = Request.get_user_id request in
        get_state_event room_id "m.room.member" user_id >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok event ->
                let event = destruct State_events.encoding event in
                (match State_events.get_event event with
                 | State_events.State_event.Member event ->
                   let event = State_events.State_event.Member.get_event event in
                   (match Room_events.Room_event.Member.get_membership event with
                    | Ban ->
                      Event_store.remove event_store Key.(v event_id) >>=
                      (function
                        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                        | Ok () ->
                          Store.remove store Key.(v "rooms" / room_id / "state" / "m.room.member" / user_id) >>=
                          (function
                            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                            | Ok () ->
                              let response =
                                Response.make
                                  ()
                              in
                              let response =
                                construct Response.encoding response |>
                                Ezjsonm.value_to_string
                              in
                              Lwt.return (`OK, response)))
                    | _ -> Lwt.return (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id)))
                 | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))))
  in
  needs_auth, f

let send_message =
  let open Room_event.Put.Message_event in
  let f ((((), room_id), _), _txn_id) request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let id = event_id () in
        let user_id = username_to_user_id username in
        let message = Request.get_event request in
        let event =
          Message_event.make
            ~event:message
            ~event_id:id
            ~sender:user_id
            ~origin_server_ts:(time ())
            ()
        in
        let encoded_event = Json_encoding.construct Message_event.encoding event in
        Event_store.set event_store Key.(v id) encoded_event >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok () ->
            Store.get store Key.(v "rooms" / room_id / "messages" / "head") >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok prev_head ->
                Store.set store Key.(v "rooms" / room_id / "messages" / "head") id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                  | Ok () ->
                    Store.set store Key.(v "rooms" / room_id / "messages" / id) prev_head >>=
                    (function
                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
                      | Ok () ->
                        let response =
                          Response.make
                            ~event_id:id
                            ()
                        in
                        let response =
                          construct Response.encoding response |>
                          Ezjsonm.value_to_string
                        in
                        Lwt.return (`OK, response))))))
  in
  needs_auth, f













(* ------------- *)

let get_room_timeline room_id =
  let rec f acc id =
    Store.get store Key.(v "rooms" / room_id / "messages" / id) >>=
    (function
      | Error err -> Lwt.return_error err
      | Ok next_id ->
        match next_id with
        | "" -> Lwt.return_ok acc
        | next_id -> f (id::acc) next_id)
  in
  f [] "head" >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok messages ->
      Lwt_list.filter_map_p
        (fun (message_id) ->
           Event_store.get event_store Key.(v message_id) >>=
           (function
             | Error _ -> Lwt.return_none
             | Ok event -> Lwt.return_some event)) messages >>=
      (fun events ->
         List.map
           (fun event ->
              Events.Message_event (destruct Message_event.encoding event)) events |> Lwt.return_ok))

let get_room_ephemeral room_id =
  Store.list store Key.(v "rooms" / room_id / "ephemeral" / "typing") >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok users ->
      Lwt_list.filter_map_p
        (fun (username, _) ->
           Store.get store Key.(v "rooms" / room_id / "ehpemeral" / "typing" / username ) >>=
           (function
             | Error _ -> Lwt.return_none
             | Ok until ->
               let _until = float_of_string until in
               Lwt.return_some username)) users >>=
      (fun users ->
         Lwt.return_ok (Event.Typing.make ~users_id:users ())))

let get_room_state room_id =
  Store.list store Key.(v "rooms" / room_id / "state") >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok event_types ->
      Lwt_list.filter_map_p
        (fun (event_type, _) ->
           Store.list store Key.(v "rooms" / room_id / "state" / event_type) >>=
           (function
             | Error _ -> Lwt.return_none
             | Ok state_keys ->
               Lwt_list.filter_map_p
                 (fun (state_key, _) ->
                    get_state_event room_id event_type state_key >>=
                    (function
                      | Error _ -> Lwt.return_none
                      | Ok event_id ->
                        Event_store.get event_store Key.(v event_id) >>=
                        (function
                          | Error _ -> Lwt.return_none
                          | Ok event -> Lwt.return_some event))) state_keys >>=
               (fun events -> Lwt.return_some events))) event_types >>=
      (fun events ->
         List.flatten events |> List.map (destruct State_events.encoding) |> Lwt.return_ok))

let get_rooms_membership user_id =
  Store.list store Key.(v "rooms") >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok rooms ->
      Lwt_list.filter_map_p
        (fun (room_id, _) ->
           get_state_event room_id "m.room.member" user_id >>=
           (function
             | Error _ -> Lwt.return_none
             | Ok event_id ->
               Event_store.get event_store Key.(v event_id) >>=
               (function
                 | Error _ -> Lwt.return_none
                 | Ok event ->
                   let event = destruct State_events.encoding event in
                   (match State_events.get_event event with
                    | State_events.State_event.Member event ->
                      let event = State_events.State_event.Member.get_event event in
                      Lwt.return_some (room_id, Room_events.Room_event.Member.get_membership event)
                    | _ -> Lwt.return_none)))) rooms >>=
      (fun rooms ->
         let f (j, i, l) = function
           | room_id, Room_events.Membership.Join -> room_id::j, i, l
           | room_id, Room_events.Membership.Invite -> j, room_id::i, l
           | room_id, Room_events.Membership.Ban
           | room_id, Room_events.Membership.Leave -> j, i, room_id::l
           | _room_id, Room_events.Membership.Knock -> j, i, l (* maybe do something for knocked rooms *)
         in
         let rooms = List.fold_left f ([], [], []) rooms in
         Lwt.return_ok rooms))

let get_rooms user_id =
  get_rooms_membership user_id >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok (j, i, l) ->
      Lwt_list.filter_map_p
        (fun room_id ->
           get_room_state room_id >>=
           (function
             | Error _ -> Lwt.return_none
             | Ok state_events ->
               get_room_timeline room_id >>=
               (function
                 | Error _ -> Lwt.return_none
                 | Ok message_events ->
                   get_room_ephemeral room_id >>=
                   (function
                     | Error _ -> Lwt.return_none
                     | Ok _ephemeral_events ->
                       let timeline =
                         Rooms.Timeline.make
                           ?events:(Some message_events)
                           ()
                       in
                       let room =
                         Rooms.Joined_room.make
                           ?state:(Some state_events)
                           ?timeline:(Some timeline)
                           (* ?ephemeral:(Some [ephemeral_events]) *)
                           ()
                       in
                       Lwt.return_some (room_id, room))))) j >>=
      (fun joined ->
         Lwt_list.filter_map_p
           (fun room_id ->
              get_room_state room_id >>=
              (function
                | Error _ -> Lwt.return_none
                | Ok events ->
                  let room =
                    Rooms.Invited_room.make
                      ?invite_state:(Some events)
                      ()
                  in
                  Lwt.return_some (room_id, room))) i >>=
         (fun invited ->
            Lwt_list.filter_map_p
              (fun room_id ->
                 get_room_state room_id >>=
                 (function
                   | Error _ -> Lwt.return_none
                   | Ok events ->
                     let room =
                       Rooms.Left_room.make
                         ?state:(Some events)
                         ()
                     in
                     Lwt.return_some (room_id, room))) l >>=
            (fun leaved ->
               Lwt.return_ok (joined, invited, leaved)))))
