open Helper
open Middleware
open Store
open Matrix_common
open Matrix_stos
open Common_routes

let placeholder _ = assert false

let sign t encoding =
  Signatures.encoding [t.server_name, ["ed25519:" ^ t.key_name, t.priv_key]] encoding

module Key = struct
  module V2 = struct
    (* Notes:
      - Handle old_verify_keys
      - Use a proper and appropriate validity time
    *)
    let direct_query t _request =
      let open Key.Direct_query in
      (* The key_id path parameter is deprecated, and therefore ignored.
         Instead, all the keys are returned if several of them have been
         defined *)
      match
        Base64.encode ~pad:false
          (Cstruct.to_string
          @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct t.pub_key)
      with
      | Ok base64_key ->
        let response =
          Response.make ~server_name:t.server_name
            ~verify_keys:
              ["ed25519:" ^ t.key_name, Response.Verify_key.make ~key:base64_key ()]
            ~old_verify_keys:[]
            ~valid_until_ts:((time () + 60) * 1000)
            ()
          |> Json_encoding.construct (sign t Response.encoding)
          |> Ezjsonm.value_to_string in
        Dream.json response
      | Error (`Msg s) ->
        Dream.error (fun m -> m "Base64 key encode error: %s" s);
        Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKOWN"}|}
  end
end

module Public_rooms =
struct
  (* Notes:
    - Filter & pagination are ignored for now
  *)
  let get _t _request =
    let open Public_rooms.Get_public_rooms in
    let%lwt tree = Store.tree store in
    (* retrieve the list of the rooms *)
    let%lwt rooms = Store.Tree.list tree @@ Store.Key.v ["rooms"] in
    (* filter out the public rooms*)
    let%lwt public_rooms =
      Lwt_list.map_p
        (fun (room_id, room_tree) ->
          (* retrieve the room's canonical_alias if any *)
          let%lwt canonical_alias =
            let%lwt json = Store.Tree.find room_tree @@ Store.Key.v ["state"; "m.room.join_rules"] in
            match json with
            | None -> Lwt.return_none
            | Some json ->
              let event =
                Json_encoding.destruct Events.State_event.encoding (Ezjsonm.value_from_string json)
              in
              match Events.State_event.get_event_content event with
              | Canonical_alias canonical_alias ->
                Lwt.return (Option.join @@ Events.Event_content.Canonical_alias.get_alias canonical_alias)
              | _ -> Lwt.return_none
          in
          (* retrieve the room's name if any *)
          let%lwt name =
            let%lwt json = Store.Tree.find room_tree @@ Store.Key.v ["state"; "m.room.name"] in
            match json with
            | None -> Lwt.return_none
            | Some json ->
              let event =
                Json_encoding.destruct Events.State_event.encoding (Ezjsonm.value_from_string json)
              in
              match Events.State_event.get_event_content event with
              | Name name ->
                Lwt.return_some (Events.Event_content.Name.get_name name)
              | _ -> Lwt.return_none
          in
          (* retrieve the room's members number *)
          (* let num_joined_members =
            let%lwt json = Store.Tree.find room_tree @@ Store.Key.v ["state"; "m.room.name"] in
            let event =
              Json_encoding.destruct Events.State_event.encoding (Ezjsonm.value_from_string json)
            in
            match Events.State_event.get_event_content event with
            | Name name ->
              Lwt.return_some (Events.Event_content.Name.get_name name)
            | _ -> Lwt.return_none
          in *)
          (* retrieve the room's topic if any *)
          let%lwt topic =
            let%lwt json = Store.Tree.find room_tree @@ Store.Key.v ["state"; "m.room.topic"] in
            match json with
            | None -> Lwt.return_none
            | Some json ->
              let event =
                Json_encoding.destruct Events.State_event.encoding (Ezjsonm.value_from_string json)
              in
              match Events.State_event.get_event_content event with
              | Topic topic ->
                Lwt.return_some (Events.Event_content.Topic.get_topic topic)
              | _ -> Lwt.return_none
          in
          (* retrieve the room's topic if any *)
          let%lwt avatar_url =
            let%lwt json = Store.Tree.find room_tree @@ Store.Key.v ["state"; "m.room.avatar"] in
            match json with
            | None -> Lwt.return_none
            | Some json ->
              let event =
                Json_encoding.destruct Events.State_event.encoding (Ezjsonm.value_from_string json)
              in
              match Events.State_event.get_event_content event with
              | Avatar avatar ->
                Lwt.return_some (Events.Event_content.Avatar.get_url avatar)
              | _ -> Lwt.return_none
          in
          (* Notes:
            - aliases are ignored for now
            - as guests are totally ignored, world_readable guest_can_join are
              set to false
            - federate is not in the documentation, so set to false for now,
              needs investigation in order to know what it means *)
          let room =
            Response.Public_rooms_chunk.make ~aliases:[]
              ?canonical_alias ?name
              ~num_joined_members:0 ~room_id
              ?topic ~world_readable:false
              ~guest_can_join:false ?avatar_url
              ~federate:false () in
          Lwt.return room)
        rooms
    in
    let response =
      Response.make ~chunk:public_rooms
        ~total_room_count_estimate:(List.length rooms) ()
      |> Json_encoding.construct Response.encoding
      |> Ezjsonm.value_to_string in
    Dream.json response
end

let router (t : Common_routes.t) =
  Dream.router
    [
      Dream.scope "/_matrix" []
        [
          Dream.scope "/federation" []
            [
              Dream.scope "/v1" []
                [
                  Dream.get "/version" placeholder;
                  Dream.put "/3pid/onbind" placeholder;
                  Dream.get "/openid/userinfo" placeholder;
                  Dream.scope "" [is_logged_server t]
                    [
                      Dream.put "/send/:txn_id" placeholder;
                      Dream.get "/event_auth/:room_id/:event_id" placeholder;
                      Dream.get "/backfill/:room_id" placeholder;
                      Dream.post "/get_missing_events/:room_id" placeholder;
                      Dream.get "/state/:room_id" placeholder;
                      Dream.get "/state_ids/:room_id" placeholder;
                      Dream.get "/event/:event_id" placeholder;
                      Dream.get "/make_join/:room_id/:user_id" placeholder;
                      Dream.put "/send_join/:room_id/:event_id" placeholder;
                      Dream.put "/invite/:room_id/:event_id" placeholder;
                      Dream.get "/make_leave/:room_id/:user_id" placeholder;
                      Dream.put "/send_leave/:room_id/:event_id" placeholder;
                      Dream.put "/exchange_third_party_invite/:room_id"
                        placeholder; Dream.get "/publicRooms" (Public_rooms.get t);
                      Dream.post "/publicRooms" placeholder;
                      Dream.scope "/query" []
                        [
                          Dream.get "/:query_type" placeholder;
                          Dream.get "/directory" placeholder;
                          Dream.get "/profile" placeholder;
                        ];
                      Dream.scope "/user" []
                        [
                          Dream.get "/devices/:user_id" placeholder;
                          Dream.scope "/keys" []
                            [
                              Dream.post "/claim" placeholder;
                              Dream.post "/query" placeholder;
                            ];
                        ];
                    ];
                ];
              Dream.scope "/v2" []
                [
                  Dream.scope "" [is_logged_server t]
                    [
                      Dream.put "/send_join/:room_id/:event_id" placeholder;
                      Dream.put "/invite/:room_id/:event_id" placeholder;
                      Dream.put "/send_leave/:room_id/:event_id" placeholder;
                    ];
                ];
            ];
          Dream.scope "/key/v2" []
            [
              Dream.get "/server" (Key.V2.direct_query t);
              Dream.get "/server/:key_id" (Key.V2.direct_query t);
              Dream.get "/query/:server_name/:key_id" placeholder;
              Dream.post "/query" placeholder;
            ];
        ];
    ]
