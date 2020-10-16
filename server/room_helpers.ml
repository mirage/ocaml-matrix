open Lwt.Infix
open Json_encoding
open Store
open Matrix_ctos

let set_state_event room_id event_type state_key event_id =
  let state_key = if state_key = "" then "_" else state_key in
  Store.set store Key.(v "rooms" / room_id / "state" / event_type / state_key) event_id

let get_state_event room_id event_type state_key =
  let state_key = if state_key = "" then "_" else state_key in
  Store.get store Key.(v "rooms" / room_id / "state" / event_type / state_key)

let get_event_since key since =
  Store.last_modified store key >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok (_, last_modified) ->
      if Int64.of_int since < last_modified
      then
        Store.get store key
      else
        Lwt.return_error (`Not_found key))

let get_state_event_since room_id event_type state_key since =
  let state_key = if state_key = "" then "_" else state_key in
  get_event_since Key.(v "rooms" / room_id / "state" / event_type / state_key) since

let get_room_timeline room_id since =
  let rec f acc id =
    get_event_since Key.(v "rooms" / room_id / "messages" / id) since >>=
    (function
      | Error (`Not_found _) ->
        Lwt.return_ok acc
      | Error err ->
        Lwt.return_error err
      | Ok next_id ->
        match next_id with
        | "" -> Lwt.return_ok (id::acc)
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

let get_room_state room_id since =
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
                    get_state_event_since room_id event_type state_key since >>=
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

let get_rooms_membership user_id since =
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
                get_state_event_since room_id "m.room.member" user_id since >>=
                (function
                  | Error _ -> Lwt.return_false
                  | Ok _ -> Lwt.return_true) >>=
                (fun changed ->
                  Event_store.get event_store Key.(v event_id) >>=
                  (function
                    | Error _ -> Lwt.return_none
                    | Ok event ->
                      let event = destruct State_events.encoding event in
                      (match State_events.get_event event with
                        | State_events.State_event.Member event ->
                          let event = State_events.State_event.Member.get_event event in
                          Lwt.return_some (room_id, Room_events.Room_event.Member.get_membership event, changed)
                        | _ -> Lwt.return_none))))) rooms >>=
      (fun rooms ->
         let f (j, i, l) = function
           | room_id, Room_events.Membership.Join, changed -> (room_id, changed)::j, i, l
           | room_id, Room_events.Membership.Invite, changed -> j, (room_id, changed)::i, l
           | room_id, Room_events.Membership.Ban, changed
           | room_id, Room_events.Membership.Leave, changed -> j, i, (room_id, changed)::l
           | _room_id, Room_events.Membership.Knock, _ -> j, i, l (* maybe do something for knocked rooms *)
         in
         let rooms = List.fold_left f ([], [], []) rooms in
         Lwt.return_ok rooms))

let get_rooms user_id since =
  let update = ref false in
  get_rooms_membership user_id since >>=
  (function
    | Error err -> Lwt.return_error err
    | Ok (j, i, l) ->
      Lwt_list.filter_map_p
        (fun (room_id, changed) ->
          let since = if changed then 0 else since in
          get_room_state room_id since >>=
          (function
            | Error _ -> Lwt.return_none
            | Ok state_events ->
              get_room_timeline room_id since >>=
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
                      update := !update || (List.length state_events + (List.length message_events) <> 0);
                      Lwt.return_some (room_id, room))))) j >>=
      (fun joined ->
         Lwt_list.filter_map_p
            (fun (room_id, changed) ->
              let since = if changed then 0 else since in
              get_room_state room_id since >>=
              (function
                | Error _ -> Lwt.return_none
                | Ok events ->
                  let room =
                    Rooms.Invited_room.make
                      ?invite_state:(Some events)
                      ()
                  in
                  update := !update || (List.length events <> 0);
                  Lwt.return_some (room_id, room))) i >>=
         (fun invited ->
            Lwt_list.filter_map_p
              (fun (room_id, changed) ->
                let since = if changed then 0 else since in
                 get_room_state room_id since >>=
                 (function
                   | Error _ -> Lwt.return_none
                   | Ok events ->
                     let room =
                       Rooms.Left_room.make
                         ?state:(Some events)
                         ()
                     in
                      update := !update || (List.length events <> 0);
                     Lwt.return_some (room_id, room))) l >>=
            (fun leaved ->
              Lwt.return_ok (joined, invited, leaved, !update)))))
