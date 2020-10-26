open Lwt.Infix
open Json_encoding
open Matrix_ctos
open Store
open Helpers
open Room_helpers

let create_room =
  let open Room.Create in
  let f () request _ token =
    get_logged_user token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
      | Ok (Some user_id) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let room_id = room_id () in
        Store.set store Key.(v "rooms" / room_id / "messages" / "head") "" >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                ~origin_server_ts:(time () * 1000)
                ~unsigned:
                  (Room_events.Unsigned.make
                    ~whatever:(`O ["age", `Float 0.])
                    ())
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
              | Ok () ->
                set_state_event room_id "m.room.power_levels" (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                        ~origin_server_ts:(time () * 1000)
                        ~unsigned:
                          (Room_events.Unsigned.make
                            ~whatever:(`O ["age", `Float 0.])
                            ())
                        ()
                    in
                    let encoded_event = Json_encoding.construct State_events.encoding event in
                    Event_store.set event_store Key.(v id) encoded_event >>=
                    (function
                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                      | Ok () ->
                        set_state_event room_id "m.room.history_visibility" (State_events.get_state_key event) id >>=
                        (function
                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                          | Ok () ->
                            let id = event_id () in
                            let event =
                              State_events.make
                                ~event:
                                  (State_events.State_event.Create
                                     (State_events.State_event.Create.make
                                        ~event:
                                          (Room_events.Room_event.Create.make
                                             ~creator:user_id
                                             ?room_version:(Some Const.room_version)
                                             ())
                                        ()))
                                ?event_id:(Some id)
                                ?sender:(Some user_id)
                                ~origin_server_ts:(time () * 1000)
                                ~unsigned:
                                  (Room_events.Unsigned.make
                                    ~whatever:(`O ["age", `Float 0.])
                                    ())
                                ()
                            in
                            let encoded_event = Json_encoding.construct State_events.encoding event in
                            Event_store.set event_store Key.(v id) encoded_event >>=
                            (function
                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                              | Ok () ->
                                set_state_event room_id "m.room.create" (State_events.get_state_key event) id >>=
                                (function
                                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                                        ~origin_server_ts:(time () * 1000)
                                        ~unsigned:
                                          (Room_events.Unsigned.make
                                            ~whatever:(`O ["age", `Float 0.])
                                            ())
                                        ()
                                    in
                                    let encoded_event = Json_encoding.construct State_events.encoding event in
                                    Event_store.set event_store Key.(v id) encoded_event >>=
                                    (function
                                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                                      | Ok () ->
                                        set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                                        (function
                                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                                                ~origin_server_ts:(time () * 1000)
                                                ~unsigned:
                                                  (Room_events.Unsigned.make
                                                    ~whatever:(`O ["age", `Float 0.])
                                                    ())
                                                ()
                                            in
                                            let encoded_event = Json_encoding.construct State_events.encoding event in
                                            Event_store.set event_store Key.(v id) encoded_event >>=
                                            (function
                                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                                              | Ok () ->
                                                set_state_event room_id "m.room.join_rules" (State_events.get_state_key event) id >>=
                                                (function
                                                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                                                           ~origin_server_ts:(time () * 1000)
                                                           ~unsigned:
                                                             (Room_events.Unsigned.make
                                                               ~whatever:(`O ["age", `Float 0.])
                                                               ())
                                                           ()
                                                       in
                                                       let encoded_event = Json_encoding.construct State_events.encoding event in
                                                       Event_store.set event_store Key.(v id) encoded_event >>=
                                                       (function
                                                         | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                                                         | Ok () ->
                                                           set_state_event room_id "m.room.name" (State_events.get_state_key event) id >>=
                                                           (function
                                                             | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                                                               ~origin_server_ts:(time () * 1000)
                                                               ~unsigned:
                                                                 (Room_events.Unsigned.make
                                                                   ~whatever:(`O ["age", `Float 0.])
                                                                   ())
                                                               ()
                                                           in
                                                           let encoded_event = Json_encoding.construct State_events.encoding event in
                                                           Event_store.set event_store Key.(v id) encoded_event >>=
                                                           (function
                                                             | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                                                             | Ok () ->
                                                               set_state_event room_id "m.room.topic" (State_events.get_state_key event) id >>=
                                                               (function
                                                                 | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                                                            Lwt.return (`OK, response, None)))))))))))))))
  in
  needs_auth, f

module Room_alias =
struct
  let put =
    let open Room.Create_alias in
    let f ((), alias) request _ _ =
      let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
      let room_id = Request.get_room_id request in
      Store.exists store Key.(v "room_aliases" / alias) >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok (Some _) -> Lwt.return (`Bad_request, error "M_UNKNOWN" (Fmt.str "Room alias %s already exists." alias), None)
        | Ok (None) ->
          Store.set store Key.(v "room_aliases" / alias) room_id >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              let response =
                Response.make
                  ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None)))
    in
    needs_auth, f

  let get =
    let open Room.Resolve_alias in
    let f ((), alias) _ _ _ =
      Store.get store Key.(v "room_aliases" / alias) >>=
      (function
        | Error (`Not_found _) -> Lwt.return (`Not_found, error "M_NOT_FOUND" (Fmt.str "Room alias %s not found." alias), None)
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok room_id ->
          let response =
            Response.make
              ~room_id:room_id
              ~servers:[Const.homeserver] (* Need to add other servers *)
              ()
          in
          let response =
            construct Response.encoding response |>
            Ezjsonm.value_to_string
          in
          Lwt.return (`OK, response, None))
    in
    needs_auth, f

  let delete =
    let open Room.Delete_alias in
    let f ((), alias) _ _ _ =
      (* Check that the user has the rights to remove this alias *)
      Store.exists store Key.(v "room_aliases" / alias) >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Not_found, error "M_NOT_FOUND" (Fmt.str "Room alias %s not found." alias), None)
        | Ok (Some _) ->
          Store.remove store Key.(v "room_aliases" / alias) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              let response =
                Response.make
                  ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None)))
    in
    needs_auth, f
end

module Membership =
struct
  let invite =
    let open Joining.Invite in
    let f ((), room_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some request_user_id) ->
          let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
          let user_id = Request.get_user_id request in
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return_ok ()
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Join -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s had already joined the room" user_id), None)
                      | Ban -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None)
                      | _ -> Lwt.return_ok ())
                  | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))) >>=
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
                  ?sender:(Some request_user_id)
                  ~origin_server_ts:(time () * 1000)
                  ~unsigned:
                    (Room_events.Unsigned.make
                      ~whatever:(`O ["age", `Float 0.])
                      ())
                  ()
              in
              let encoded_event = Json_encoding.construct State_events.encoding event in
              Event_store.set event_store Key.(v id) encoded_event >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                  (function
                    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                    | Ok () ->
                      let response =
                        Response.make
                          ()
                      in
                      let response =
                        construct Response.encoding response |>
                        Ezjsonm.value_to_string
                      in
                      Lwt.return (`OK, response, None)))))
    in
    needs_auth, f

  let join =
    let open Joining.Join in
    let f ((), room_id) _ _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return_ok ()
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Join -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s had already joined the room" user_id), None)
                      | Ban -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None)
                      | _ -> Lwt.return_ok ())
                  | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))) >>=
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
                  ~origin_server_ts:(time () * 1000)
                  ~unsigned:
                    (Room_events.Unsigned.make
                      ~whatever:(`O ["age", `Float 0.])
                      ())
                  ()
              in
              let encoded_event = Json_encoding.construct State_events.encoding event in
              Event_store.set event_store Key.(v id) encoded_event >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                  (function
                    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                      Lwt.return (`OK, response, None)))))
    in
    needs_auth, f

  let leave =
    let open Leaving.Leave in
    let f ((), room_id) _ _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return_ok ()
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Join -> Lwt.return_ok ()
                      | Leave -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is not in the room" user_id), None)
                      | _ -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None))
                  | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))) >>=
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
                  ~origin_server_ts:(time () * 1000)
                  ~unsigned:
                    (Room_events.Unsigned.make
                      ~whatever:(`O ["age", `Float 0.])
                      ())
                  ()
              in
              let encoded_event = Json_encoding.construct State_events.encoding event in
              Event_store.set event_store Key.(v id) encoded_event >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                  (function
                    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                    | Ok () ->
                      let response =
                        Response.make
                          ()
                      in
                      let response =
                        construct Response.encoding response |>
                        Ezjsonm.value_to_string
                      in
                      Lwt.return (`OK, response, None)))))
    in
    needs_auth, f

  let forget =
    let open Leaving.Leave in
    let f ((), room_id) _ _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Leave ->
                        Event_store.remove event_store Key.(v event_id) >>=
                        (function
                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                          | Ok () ->
                            Store.remove store Key.(v "rooms" / room_id / "state" / "m.room.member" / user_id) >>=
                            (function
                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                              | Ok () ->
                                let response =
                                  Response.make
                                    ()
                                in
                                let response =
                                  construct Response.encoding response |>
                                  Ezjsonm.value_to_string
                                in
                                Lwt.return (`OK, response, None)))
                      | _ -> Lwt.return (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None))
                  | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))))
    in
    needs_auth, f

  let kick =
    let open Leaving.Kick in
    let f ((), room_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some request_user_id) ->
          let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
          let user_id = Request.get_user_id request in
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return_ok ()
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Join -> Lwt.return_ok ()
                      | _ -> Lwt.return_error (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None))
                  | _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))) >>=
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
                  ?sender:(Some request_user_id)
                  ~origin_server_ts:(time () * 1000)
                  ~unsigned:
                    (Room_events.Unsigned.make
                      ~whatever:(`O ["age", `Float 0.])
                      ())
                  ()
              in
              let encoded_event = Json_encoding.construct State_events.encoding event in
              Event_store.set event_store Key.(v id) encoded_event >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
                  (function
                    | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                    | Ok () ->
                      let response =
                        Response.make
                          ()
                      in
                      let response =
                        construct Response.encoding response |>
                        Ezjsonm.value_to_string
                      in
                      Lwt.return (`OK, response, None)))))
    in
    needs_auth, f
  let ban =
    let open Banning.Ban in
    let f ((), room_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some request_user_id) ->
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
              ?sender:(Some request_user_id)
              ~origin_server_ts:(time () * 1000)
              ~unsigned:
                (Room_events.Unsigned.make
                  ~whatever:(`O ["age", `Float 0.])
                  ())
              ()
          in
          let encoded_event = Json_encoding.construct State_events.encoding event in
          Event_store.set event_store Key.(v id) encoded_event >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              set_state_event room_id "m.room.member" (State_events.get_state_key event) id >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  let response =
                    Response.make
                      ()
                  in
                  let response =
                    construct Response.encoding response |>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response, None))))
    in
    needs_auth, f

  let unban =
    let open Leaving.Kick in
    let f ((), room_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some _request_user_id) ->
          let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
          let user_id = Request.get_user_id request in
          get_state_event room_id "m.room.member" user_id >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok event_id ->
              Event_store.get event_store Key.(v event_id) >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok event ->
                  let event = destruct State_events.encoding event in
                  (match State_events.get_event event with
                  | State_events.State_event.Member event ->
                    let event = State_events.State_event.Member.get_event event in
                    (match Room_events.Room_event.Member.get_membership event with
                      | Ban ->
                        Event_store.remove event_store Key.(v event_id) >>=
                        (function
                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                          | Ok () ->
                            Store.remove store Key.(v "rooms" / room_id / "state" / "m.room.member" / user_id) >>=
                            (function
                              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                              | Ok () ->
                                let response =
                                  Response.make
                                    ()
                                in
                                let response =
                                  construct Response.encoding response |>
                                  Ezjsonm.value_to_string
                                in
                                Lwt.return (`OK, response, None)))
                      | _ -> Lwt.return (`Internal_server_error, error "M_FORBIDDEN" (Fmt.str "%s is banned from the room" user_id), None))
                  | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)))))
    in
    needs_auth, f
end

module Listing =
struct
  let get_visibility =
    let open Room_listing.Get_visibility in
    let f ((), room_id) _ _ _ =
      get_state_event room_id "m.room.join_rules" "" >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok event_id ->
          Event_store.get event_store Key.(v event_id) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok event ->
              let event = destruct State_events.encoding event in
              (match State_events.get_event event with
                | State_events.State_event.Join_rules event ->
                  let event = State_events.State_event.Join_rules.get_event event in
                  let visibility =
                    (match Room_events.Room_event.Join_rules.get_join_rule event with
                      | Public -> Room.Visibility.Public
                      | _ -> Room.Visibility.Private)
                    in
                    let response =
                      Response.make
                        ~visibility
                        ()
                    in
                    let response =
                      construct Response.encoding response |>
                      Ezjsonm.value_to_string
                    in
                    Lwt.return (`OK, response, None)
                | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None))))
    in
    needs_auth, f

  let set_visibility =
    let open Room_listing.Set_visibility in
    let f ((), room_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
          let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
          let join_rule =
            match Request.get_visibility request with
              | Some Room.Visibility.Public -> Room_events.Room_event.Join_rules.Public
              | Some Room.Visibility.Private -> Room_events.Room_event.Join_rules.Invite
              | None -> Room_events.Room_event.Join_rules.Invite
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
              ~event_id:id
              ~sender:user_id
              ~origin_server_ts:(time () * 1000)
              ~unsigned:
                (Room_events.Unsigned.make
                  ~whatever:(`O ["age", `Float 0.])
                  ())
              ()
          in
          let encoded_event = Json_encoding.construct State_events.encoding event in
          Event_store.set event_store Key.(v id) encoded_event >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              set_state_event room_id "m.room.join_rules" (State_events.get_state_key event) id >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  let response =
                    Response.make
                      ()
                  in
                  let response =
                    construct Response.encoding response |>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response, None))))
    in
    needs_auth, f

  let public_rooms =
    let open Room_listing.Get_public_rooms in
    let f () _ _ _ =
      Store.list store Key.(v "rooms") >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
            Lwt.return (`OK, response, None)))
    in
    needs_auth, f
end

module Events =
struct
  module Get =
  struct
    let state =
      let open Room_event.Get.State_key in (* check user rights as well as handle if room was left *)
      let f ((((), room_id), event_type), state_key) _ _ _ =
        get_state_event room_id event_type state_key >>=
        (function
          | Error (`Not_found _) -> Lwt.return (`Not_found, error "M_UNKNOWN" "Room has no such state event", None)
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
          | Ok event_id ->
            Event_store.get event_store Key.(v event_id) >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
              | Ok event ->
                (* Needs huge rework *)
                let event_content = Ezjsonm.get_dict event |> List.assoc "content" in
                let response = Ezjsonm.value_to_string event_content in
                Lwt.return (`OK, response, None)))
      in
      needs_auth, f

    let members =
      let open Room_event.Get.Members in
      let f ((), room_id) _ _ _ =
        Store.list store Key.(v "rooms" / room_id / "state" / "m.room.member") >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
              Lwt.return (`OK, response, None)))
      in
      needs_auth, f

  end

  module Put =
  struct
    let state =
      let open Room_event.Put.State_event in
      let f ((((), room_id), _), state_key) request _ token = (* should use event_type *)
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
            let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
            let id = event_id () in
            let event = Request.get_event request in
            let state_event = State_events.State_event.of_room_event event state_key in
            let event =
              State_events.make
                ~event:state_event
                ~event_id:id
                ~sender:user_id
                ~origin_server_ts:(time () * 1000)
                ~unsigned:
                  (Room_events.Unsigned.make
                    ~whatever:(`O ["age", `Float 0.])
                    ())
                ()
            in
            let encoded_event = Json_encoding.construct State_events.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
              | Ok () ->
                set_state_event room_id state_key (State_events.get_state_key event) id >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                  Lwt.return (`OK, response, None))))
      in
      needs_auth, f

    let send_message =
      let open Room_event.Put.Message_event in
      let f ((((), room_id), _), _txn_id) request _ token =
      get_logged_user token >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
        | Ok (Some user_id) ->
            let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
            let id = event_id () in
            let message = Request.get_event request in
            let event =
              Message_event.make
                ~event:
                  (Message_event.Message_event.Message message)
                ~event_id:id
                ~room_id:room_id
                ~sender:user_id
                ~origin_server_ts:(time () * 1000)
                ~unsigned:
                  (Room_events.Unsigned.make
                    ~whatever:(`O ["age", `Float 0.])
                    ())
                ()
            in
            let encoded_event = Json_encoding.construct Message_event.encoding event in
            Event_store.set event_store Key.(v id) encoded_event >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
              | Ok () ->
                Store.get store Key.(v "rooms" / room_id / "messages" / "head") >>=
                (function
                  | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                  | Ok prev_head ->
                    Store.set store Key.(v "rooms" / room_id / "messages" / "head") id >>=
                    (function
                      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                      | Ok () ->
                        Store.set store Key.(v "rooms" / room_id / "messages" / id) prev_head >>=
                        (function
                          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
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
                            Lwt.return (`OK, response, None))))))
      in
      needs_auth, f
  end
end

let room_typing =
  let open Typing in
  let f (((), room_id), _user_id) request _ token = (* maybe do something about _user_id *)
    get_logged_user token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
      | Ok (Some user_id) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        match Request.get_typing request, Request.get_timeout request with
        | true, Some timeout ->
          let until = Unix.time () +. (float_of_int timeout /. 1000.) in
          Store.set store Key.(v "rooms" / room_id / "ephemeral" / "typing" / user_id) (string_of_float until) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              let response =
                Response.make ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None))
        | _ ->
          Store.exists store Key.(v "rooms" / room_id / "ephemeral" / "typing" / user_id) >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok (Some _) ->
              Store.remove store Key.(v "rooms" / room_id / "ephemeral" / "typing" / user_id) >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok () ->
                  let response =
                    Response.make ()
                  in
                  let response =
                    construct Response.encoding response |>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response, None))
            | Ok None ->
              let response =
                Response.make ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None)))
  in
  needs_auth, f

let read_markers =
  let open Fully_read in
  let f ((), room_id) request _ token =
    get_logged_user token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None) (* should not happend *)
      | Ok (Some user_id) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        match Request.get_fully_read request with
        | Some fully_read ->
          Store.set store Key.(v "rooms" / room_id / "ephemeral" / "fully_read" / user_id) fully_read >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              let response =
                Response.make ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None))
        | None ->
          let response =
            Response.make ()
          in
          let response =
            construct Response.encoding response |>
            Ezjsonm.value_to_string
          in
          Lwt.return (`OK, response, None))
  in
  needs_auth, f
