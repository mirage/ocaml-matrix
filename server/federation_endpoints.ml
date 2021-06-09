open Lwt.Infix
open Json_encoding
open Store
open Matrix_common
open Helpers
open Room_helpers

let placeholder = placeholder

module Keys = struct
  module Server = struct
    let read_key file =
      let i = open_in file in
      let len = in_channel_length i in
      let bytes = Bytes.create len in
      really_input i bytes 0 len;
      Fmt.pr "%s" (Bytes.to_string bytes);
      match
        Rresult.R.error_msg_to_invalid_arg
          (X509.Private_key.decode_pem (Cstruct.of_bytes bytes))
      with
      | `ED25519 key -> key, Mirage_crypto_ec.Ed25519.pub_of_priv key
      | _ -> raise @@ Invalid_argument "Not an ED25519 key"

    let get_direct =
      let open Matrix_stos.Key.Direct_query in
      let f ((), _) _ _ _ =
        let priv_key, pub_key = read_key "/home/gwenaelle/.ssh/ed25519key.pem" in
        match
          Base64.encode ~pad:false
            (Cstruct.to_string
            @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct pub_key)
        with
        | Ok base64_key ->
          let response =
            Response.make ~server_name:Const.homeserver
              ~verify_keys:
                ["ed25519:foo_bar", Response.Verify_key.make ~key:base64_key ()]
              ~old_verify_keys:[]
              ~valid_until_ts:((time () + 60) * 1000)
              () in
          let response =
            construct
              (Matrix_stos.Signatures.encoding
                 [Const.homeserver, ["ed25519:foo_bar", priv_key]]
                 Response.encoding)
              response
            |> Ezjsonm.value_to_string in
          Lwt.return (`OK, response, None)
        | Error _ ->
          Lwt.return
            ( `Internal_server_error,
              error "M_UNKNOWN" "Internal base64 error",
              None ) in
      false, f

    let get_all_indirect =
      let open Matrix_stos.Key.Indirect_query in
      let f () _ _ _ =
        let priv_key, pub_key = read_key "/home/gwenaelle/.ssh/ed25519key.pem" in
        match
          Base64.encode ~pad:false
            (Cstruct.to_string
            @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct pub_key)
        with
        | Ok base64_key ->
          let key =
            Matrix_stos.Key.Server_key.make ~server_name:Const.homeserver
              ~verify_keys:
                [
                  ( "ed25519:foo_bar",
                    Matrix_stos.Key.Server_key.Verify_key.make ~key:base64_key
                      () );
                ]
              ~old_verify_keys:[]
              ~valid_until_ts:((time () + 60) * 1000)
              () in
          let response = Response.make ~server_keys:[key] () in
          let response =
            construct
              (Matrix_stos.Signatures.encoding
                 [Const.homeserver, ["ed25519:foo_bar", priv_key]]
                 Response.encoding)
              response
            |> Ezjsonm.value_to_string in
          Lwt.return (`OK, response, None)
        | Error _ ->
          Lwt.return
            ( `Internal_server_error,
              error "M_UNKNOWN" "Internal base64 error",
              None ) in
      false, f
  end
end

module Listing = struct
  module Public_rooms = struct
    let get =
      let open Matrix_stos.Public_rooms.Get_public_rooms in
      let f () _ _ _ =
        Store.list store Key.(v "rooms") >>= function
        | Error _ ->
          Lwt.return
            ( `Internal_server_error,
              error "M_UNKNOWN" "Internal storage failure",
              None )
        | Ok rooms ->
          Lwt_list.filter_map_p
            (fun (room_id, _) ->
              get_state_event room_id "m.room.join_rules" "" >>= function
              | Error _ -> Lwt.return_none
              | Ok event_id -> (
                Event_store.get event_store Key.(v event_id) >>= function
                | Error _ -> Lwt.return_none
                | Ok event -> (
                  Store.list store
                    Key.(v "rooms" / room_id / "state" / "m.room.member")
                  >>= function
                  | Error _ -> Lwt.return_none
                  | Ok user_ids -> (
                    Lwt_list.fold_left_s
                      (fun acc (user_id, _) ->
                        get_state_event room_id "m.room.member" user_id
                        >>= function
                        | Error _ -> Lwt.return acc
                        | Ok event_id -> (
                          Event_store.get event_store Key.(v event_id)
                          >>= function
                          | Error _ -> Lwt.return acc
                          | Ok event -> (
                            let event =
                              destruct Events.State_event.encoding event in
                            match
                              Events.State_event.get_event_content event
                            with
                            | Member event -> (
                              match
                                Events.Event_content.Member.get_membership event
                              with
                              | Join -> Lwt.return (acc + 1)
                              | _ -> Lwt.return acc)
                            | _ -> Lwt.return acc)))
                      0 user_ids
                    >>= fun room_members ->
                    (get_state_event room_id "m.room.name" "" >>= function
                     | Error _ -> Lwt.return_none
                     | Ok event_id -> (
                       Event_store.get event_store Key.(v event_id) >>= function
                       | Error _ -> Lwt.return_none
                       | Ok event -> (
                         let event =
                           destruct Events.State_event.encoding event in
                         match Events.State_event.get_event_content event with
                         | Name event ->
                           Lwt.return_some
                             (Events.Event_content.Name.get_name event)
                         | _ -> Lwt.return_none)))
                    >>= fun room_name ->
                    (get_state_event room_id "m.room.topic" "" >>= function
                     | Error _ -> Lwt.return_none
                     | Ok event_id -> (
                       Event_store.get event_store Key.(v event_id) >>= function
                       | Error _ -> Lwt.return_none
                       | Ok event -> (
                         let event =
                           destruct Events.State_event.encoding event in
                         match Events.State_event.get_event_content event with
                         | Name event ->
                           Lwt.return_some
                             (Events.Event_content.Name.get_name event)
                         | _ -> Lwt.return_none)))
                    >>= fun room_topic ->
                    let event = destruct Events.State_event.encoding event in
                    match Events.State_event.get_event_content event with
                    | Join_rules event -> (
                      match
                        Events.Event_content.Join_rules.get_join_rule event
                      with
                      | Public ->
                        let room_summary =
                          Response.Public_rooms_chunk.make ~aliases:[]
                            ?canonical_alias:None ?name:room_name
                            ~num_joined_members:room_members ~room_id
                            ?topic:room_topic ~world_readable:false
                            ~guest_can_join:false ?avatar_url:None
                            ~federate:false () in
                        Lwt.return_some room_summary
                      | _ -> Lwt.return_none)
                    | _ -> Lwt.return_none))))
            rooms
          >>= fun rooms ->
          let response =
            Response.make ~chunk:rooms
              ~total_room_count_estimate:(List.length rooms) () in
          let response =
            construct Response.encoding response |> Ezjsonm.value_to_string
          in
          Lwt.return (`OK, response, None) in
      false, f

    let post =
      (* needs rework so it actually filters the public rooms *)
      let open Matrix_stos.Public_rooms.Filter_public_rooms in
      let f () _ _ _ =
        Store.list store Key.(v "rooms") >>= function
        | Error _ ->
          Lwt.return
            ( `Internal_server_error,
              error "M_UNKNOWN" "Internal storage failure",
              None )
        | Ok rooms ->
          Lwt_list.filter_map_p
            (fun (room_id, _) ->
              get_state_event room_id "m.room.join_rules" "" >>= function
              | Error _ -> Lwt.return_none
              | Ok event_id -> (
                Event_store.get event_store Key.(v event_id) >>= function
                | Error _ -> Lwt.return_none
                | Ok event -> (
                  Store.list store
                    Key.(v "rooms" / room_id / "state" / "m.room.member")
                  >>= function
                  | Error _ -> Lwt.return_none
                  | Ok user_ids -> (
                    Lwt_list.fold_left_s
                      (fun acc (user_id, _) ->
                        get_state_event room_id "m.room.member" user_id
                        >>= function
                        | Error _ -> Lwt.return acc
                        | Ok event_id -> (
                          Event_store.get event_store Key.(v event_id)
                          >>= function
                          | Error _ -> Lwt.return acc
                          | Ok event -> (
                            let event =
                              destruct Events.State_event.encoding event in
                            match
                              Events.State_event.get_event_content event
                            with
                            | Member event -> (
                              match
                                Events.Event_content.Member.get_membership event
                              with
                              | Join -> Lwt.return (acc + 1)
                              | _ -> Lwt.return acc)
                            | _ -> Lwt.return acc)))
                      0 user_ids
                    >>= fun room_members ->
                    (get_state_event room_id "m.room.name" "" >>= function
                     | Error _ -> Lwt.return_none
                     | Ok event_id -> (
                       Event_store.get event_store Key.(v event_id) >>= function
                       | Error _ -> Lwt.return_none
                       | Ok event -> (
                         let event =
                           destruct Events.State_event.encoding event in
                         match Events.State_event.get_event_content event with
                         | Name event ->
                           Lwt.return_some
                             (Events.Event_content.Name.get_name event)
                         | _ -> Lwt.return_none)))
                    >>= fun room_name ->
                    (get_state_event room_id "m.room.topic" "" >>= function
                     | Error _ -> Lwt.return_none
                     | Ok event_id -> (
                       Event_store.get event_store Key.(v event_id) >>= function
                       | Error _ -> Lwt.return_none
                       | Ok event -> (
                         let event =
                           destruct Events.State_event.encoding event in
                         match Events.State_event.get_event_content event with
                         | Name event ->
                           Lwt.return_some
                             (Events.Event_content.Name.get_name event)
                         | _ -> Lwt.return_none)))
                    >>= fun room_topic ->
                    let event = destruct Events.State_event.encoding event in
                    match Events.State_event.get_event_content event with
                    | Join_rules event -> (
                      match
                        Events.Event_content.Join_rules.get_join_rule event
                      with
                      | Public ->
                        let room_summary =
                          Response.Public_rooms_chunk.make ?aliases:(Some [])
                            ?canonical_alias:None ?name:room_name
                            ~num_joined_members:room_members ~room_id
                            ?topic:room_topic ~world_readable:false
                            ~guest_can_join:false ?avatar_url:None
                            ?federate:(Some false) () in
                        Lwt.return_some room_summary
                      | _ -> Lwt.return_none)
                    | _ -> Lwt.return_none))))
            rooms
          >>= fun rooms ->
          let response = Response.make ~chunk:rooms () in
          let response =
            construct Response.encoding response |> Ezjsonm.value_to_string
          in
          Lwt.return (`OK, response, None) in
      true, f
  end
end

module Join = struct
  let get =
    let open Matrix_stos.Joining_rooms.Make_join in
    let f (((), room_id), user_id) _ _ _ =
      let response =
        Response.make ~room_version:"2"
          ~event_template:
            (Response.Event_template.make ~sender:user_id
               ~origin:(homeserver_of_user_id user_id)
               ~origin_server_ts:(time () * 1000)
               ~event_type:"m.room.member" ~state_key:user_id ~room_id ())
          () in
      let response =
        construct Response.encoding response |> Ezjsonm.value_to_string in
      (`OK, response, None) |> Lwt.return in
    true, f

  let put_v1 = placeholder

  let put_v2 =
    let open Matrix_stos.Joining_rooms.Send_join.V2 in
    let f (((), room_id), _user_id) request _ _ =
      (* Use room_id and user_id for some checks*)
      let request =
        destruct Request.encoding (Ezjsonm.value_from_string request) in
      match Request.get_event request with
      | `Room_event _ | `Event _ ->
        Lwt.return (`Forbidden, error "M_FORBIDDEN" "", None)
      | `State_event event -> (
        let id = event_id () in
        let event =
          Events.State_event.make
            ~room_event:
              (Events.Room_event.make
                 ~event:
                   (Events.Event.make
                      ~event_content:
                        (Events.State_event.get_event_content event)
                      ())
                 ~event_id:id
                 ?sender:
                   (Events.State_event.get_room_event event
                   |> Events.Room_event.get_sender)
                 ~origin_server_ts:(time () * 1000)
                 ~unsigned:(Events.Room_event.Unsigned.make ~age:0 ())
                 ())
            ~state_key:(Events.State_event.get_state_key event)
            () in
        let encoded_event =
          Json_encoding.construct Events.State_event.encoding event in
        Event_store.set event_store Key.(v id) encoded_event >>= function
        | Error _ ->
          Lwt.return
            ( `Internal_server_error,
              error "M_UNKNOWN" "Internal storage failure",
              None )
        | Ok () -> (
          set_state_event room_id "m.room.member"
            (Events.State_event.get_state_key event)
            id
          >>= function
          | Error _ ->
            Lwt.return
              ( `Internal_server_error,
                error "M_UNKNOWN" "Internal storage failure",
                None )
          | Ok () -> (
            Room_helpers.get_room_state room_id 0 >>= function
            | Error _ ->
              Lwt.return
                ( `Internal_server_error,
                  error "M_UNKNOWN" "Internal storage failure",
                  None )
            | Ok state ->
              let f s =
                Events.Pdu.make ~event:(`State_event s) ~prev_events:[] ~depth:0
                  () in
              let state = List.map f state in
              let response =
                Response.make ~origin:Const.homeserver
                  ~auth_chain:[] (* Do some things here *) ~state () in
              let response =
                construct Response.encoding response |> Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response, None)))) in
    true, f
end
