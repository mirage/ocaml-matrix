open Lwt.Infix
open Json_encoding
open Store
open Matrix_common
open Helpers
open Room_helpers

let placeholder = placeholder

module Listing =
struct
  module Public_rooms =
  struct
    let get =
      let open Matrix_stos.Public_rooms.Get_public_rooms in
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
                                          let event = destruct Events.State_event.encoding event in
                                          (match Events.State_event.get_event_content event with
                                          | Member event ->
                                            (match Events.Event_content.Member.get_membership event with
                                              | Join -> Lwt.return (acc + 1)
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
                                        let event = destruct Events.State_event.encoding event in
                                        (match Events.State_event.get_event_content event with
                                        | Name event ->
                                          Lwt.return_some (Events.Event_content.Name.get_name event)
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
                                          let event = destruct Events.State_event.encoding event in
                                          (match Events.State_event.get_event_content event with
                                            | Name event ->
                                              Lwt.return_some (Events.Event_content.Name.get_name event)
                                            | _ -> Lwt.return_none))) >>=
                                  (fun room_topic ->
                                      let event = destruct Events.State_event.encoding event in
                                      (match Events.State_event.get_event_content event with
                                      | Join_rules event ->
                                        (match Events.Event_content.Join_rules.get_join_rule event with
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

    let post = (* needs rework so it actually filters the public rooms *)
      let open Matrix_stos.Public_rooms.Filter_public_rooms in
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
                                          let event = destruct Events.State_event.encoding event in
                                          (match Events.State_event.get_event_content event with
                                          | Member event ->
                                            (match Events.Event_content.Member.get_membership event with
                                              | Join -> Lwt.return (acc + 1)
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
                                        let event = destruct Events.State_event.encoding event in
                                        (match Events.State_event.get_event_content event with
                                        | Name event ->
                                          Lwt.return_some (Events.Event_content.Name.get_name event)
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
                                          let event = destruct Events.State_event.encoding event in
                                          (match Events.State_event.get_event_content event with
                                            | Name event ->
                                              Lwt.return_some (Events.Event_content.Name.get_name event)
                                            | _ -> Lwt.return_none))) >>=
                                  (fun room_topic ->
                                      let event = destruct Events.State_event.encoding event in
                                      (match Events.State_event.get_event_content event with
                                      | Join_rules event ->
                                        (match Events.Event_content.Join_rules.get_join_rule event with
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
end

module Join =
struct
  let get =
    let open Matrix_stos.Joining_rooms.Make_join in
    let f (((), room_id), user_id) _ _ _ =
      let response =
        Response.make
          ~room_version:"2"
          ~event_template:
            (Response.Event_template.make
              ~sender:user_id
              ~origin:(homeserver_of_user_id user_id)
              ~origin_server_ts:(time () * 1000)
              ~event_type:"m.room.member"
              ~state_key:user_id
              ~room_id
              ())
          ()
      in
      let response =
        construct Response.encoding response |>
        Ezjsonm.value_to_string
      in
      (`OK, response, None) |> Lwt.return
    in
    needs_auth, f

  let put_v1 = placeholder

  let put_v2 =
    let open Matrix_stos.Joining_rooms.Send_join.V2 in
    let f (((), room_id), _user_id) request _ _ = (* Use room_id and user_id for some checks*)
      let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
      let id = event_id () in
      let event =
        Events.State_event.make
          ~room_event:
            (Events.Room_event.make
              ~event:
                (Events.Event.make
                    ~event_content:
                      (Events.Event_content.Member
                        (Request.get_content request))
                    ())
              ~event_id:id
              ~sender:(Request.get_sender request)
              ~origin_server_ts:(time () * 1000)
              ~unsigned:
                (Events.Room_event.Unsigned.make
                    ~age:0
                    ())
              ())
          ~state_key:(Request.get_state_key request)
          ()
      in
      let encoded_event = Json_encoding.construct Events.State_event.encoding event in
      Event_store.set event_store Key.(v id) encoded_event >>=
      (function
        | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
        | Ok () ->
          set_state_event room_id "m.room.member" (Events.State_event.get_state_key event) id >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
            | Ok () ->
              Room_helpers.get_room_state room_id 0 >>=
              (function
                | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure", None)
                | Ok state ->
                  let response =
                    Response.make
                      ~origin:Const.homeserver
                      ~auth_chain:[] (* Do some things here *)
                      ~state
                      ()
                  in
                  let response =
                    construct Response.encoding response |>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response, None))))
    in
    needs_auth, f
end
