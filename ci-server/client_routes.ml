open Store
open Middleware
open Matrix_common
open Matrix_ctos
open Common_routes
open Helper

(** Notes:
  - In need of adding the support of the get route in order to advertise
  which authentication mechanism is supported: at the moment, only password v2
  with user name identifiers
  - Support both localpart only users and full user_id
  - Timing attacks should also be taken into consideration
*)
let login t request =
  let open Login.Post in
  let%lwt body = Dream.body request in
  let login =
    Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
  in
  (* Verify that the login mechanism is password v2*)
  match Request.get_auth login with
  | Password (V2 auth) -> (
    (* Verify that the identifier mechanism is username*)
    match Authentication.Password.V2.get_identifier auth with
    | User user -> (
      (* check the user data *)
      let user_id =
        Identifiers.User_id.to_string
          (Identifier.User.get_user user)
          t.server_name in
      let%lwt pwd =
        Store.find store (Store.Key.v ["users"; user_id; "password"]) in
      match pwd with
      | None -> Dream.json ~status:`Unauthorized {|{"errcode": "M_FORBIDDEN"}|}
      | Some pwd -> (
        (* Verify the username/password combination *)
        let password = Authentication.Password.V2.get_password auth in
        let%lwt salt =
          Store.get store (Store.Key.v ["users"; user_id; "salt"]) in
        let digest = Digestif.BLAKE2B.hmac_string ~key:salt password in
        let pwd = Digestif.BLAKE2B.of_hex pwd in
        if not (Digestif.BLAKE2B.equal digest pwd) then
          Dream.json ~status:`Unauthorized {|{"errcode": "M_FORBIDDEN"}|}
        else
          let device =
            match Request.get_device_id login with
            | None -> Uuidm.(v `V4 |> to_string)
            | Some device -> Fmt.str "%s:%s" device user_id in
          let token = Uuidm.(v `V4 |> to_string) in
          let%lwt tree = Store.tree store in
          (* add token *)
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["tokens"; token; "device"]) device
          in
          let expires_at = Float.to_string @@ (Unix.gettimeofday () +. 3600.) in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["tokens"; token; "expires_at"])
              expires_at in
          (* add device *)
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["devices"; device; "user_id"])
              user_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["devices"; device; "token"]) token
          in
          (* update user *)
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["users"; user_id; "devices"; device])
              device in
          (* saving new tree *)
          let%lwt return =
            Store.set_tree
              ~info:(Helper.info t ~message:"login user")
              store (Store.Key.v []) tree in
          match return with
          | Ok () ->
            let response =
              Response.make ~access_token:token ~device_id:device ()
              |> Json_encoding.construct Response.encoding
              |> Ezjsonm.value_to_string in
            Dream.json response
          | Error write_error ->
            Dream.error (fun m ->
                m "Write error: %a"
                  (Irmin.Type.pp Store.write_error_t)
                  write_error);
            Dream.json ~status:`Internal_Server_Error
              {|{"errcode": "M_UNKNOWN"}|}))
    | _ ->
      Dream.json ~status:`Bad_Request
        {|{"errcode": "M_UNKNOWN", "error": "Bad identifier type"}|})
  | _ ->
    Dream.json ~status:`Bad_Request
      {|{"errcode": "M_UNKNOWN", "error": "Bad login type"}|}

(** Notes:
    - Error handling again
  *)
let logout t request =
  let open Logout.Logout in
  let%lwt () =
    match
      Dream.local logged_user request, Dream.local logged_device request
    with
    | Some username, Some device -> (
      let%lwt token =
        Store.get store (Store.Key.v ["devices"; device; "token"]) in
      (* remove the device data and the associated token *)
      let%lwt tree = Store.tree store in
      let%lwt tree =
        Store.Tree.remove tree
          (Store.Key.v ["users"; username; "devices"; device]) in
      let%lwt tree = Store.Tree.remove tree (Store.Key.v ["devices"; device]) in
      let%lwt tree = Store.Tree.remove tree (Store.Key.v ["tokens"; token]) in
      let%lwt return =
        Store.set_tree
          ~info:(Helper.info t ~message:"logout user")
          store (Store.Key.v []) tree in
      match return with
      | Ok () -> Lwt.return_unit
      | Error write_error ->
        Dream.error (fun m ->
            m "Write error: %a" (Irmin.Type.pp Store.write_error_t) write_error);
        Lwt.return_unit)
    | _ -> Lwt.return_unit
    (* should never happen if is_logged middleware is used *) in
  let response =
    Response.make ()
    |> Json_encoding.construct Response.encoding
    |> Ezjsonm.value_to_string in
  Dream.json response

(** Notes:
    - Use the federation api ? We dont really want to do that however, as we
      want a minimalist server.
    - At least verify if the server part of the alias is the right one
  *)
let resolve_alias request =
  let open Room.Resolve_alias in
  let alias = Dream.param "alias" request in
  let room_alias, _ = Identifiers.Room_alias.of_string_exn alias in
  let%lwt room_id = Store.find store (Store.Key.v ["aliases"; room_alias]) in
  match room_id with
  | None ->
    Dream.json ~status:`Not_Found
      (Fmt.str
         {|{"errcode": "M_NOT_FOUND", "error": "Room alias %s not found."}|}
         room_alias)
  | Some room_id ->
    let response =
      Response.make ~room_id ()
      |> Json_encoding.construct Response.encoding
      |> Ezjsonm.value_to_string in
    Dream.json response

let create_room t request =
  let open Room.Create in
  let%lwt body = Dream.body request in
  let create_room =
    Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
  in
  (* Verify that the request is for a public room *)
  match Request.get_visibility create_room with
  | Some Public -> (
    match Request.get_room_alias_name create_room with
    | None ->
      Dream.json ~status:`Bad_Request
        {|{"errcode": "M_INVALID_ROOM_STATE"; "error": "Room alias is mandatory for a public room"}|}
    | Some alias -> (
      let%lwt s_alias = Store.find store (Store.Key.v ["aliases"; alias]) in
      match s_alias with
      | Some _ ->
        Dream.json ~status:`Bad_Request
          {|{"errcode": "M_ROOM_IN_USE"; "error": "Room alias already in use"}|}
      | None -> (
        match Dream.local logged_user request with
        | Some user_id -> (
          let%lwt tree = Store.tree store in
          let room_id = "!" ^ Uuidm.(v `V4 |> to_string) ^ ":" ^ t.server_name in
          (* Create the state events of the room *)
          let auth_events = [] in
          let depth = 1 in
          (* create *)
          let event_content =
            Events.Event_content.Create
              (Events.Event_content.Create.make ~creator:user_id
                 ?room_version:(Some "6") ()) in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ()) ~prev_events:[]
              ~prev_state:[] ~room_id ~sender:user_id ~signatures:[]
              ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let create_hash = "$" ^ event_id in
          let auth_events = create_hash :: auth_events in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["rooms"; room_id; "state"; "m.room.create"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* member *)
          let event_content =
            Events.Event_content.Member
              (Events.Event_content.Member.make ~avatar_url:None
                 ~displayname:(Some user_id) ~membership:Join ()) in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:[create_hash] ~prev_state:[] ~room_id ~sender:user_id
              ~signatures:[] ~state_key:user_id
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let member_hash = "$" ^ event_id in
          let auth_events = member_hash :: auth_events in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v
                 ["rooms"; room_id; "state"; "m.room.member"; user_id])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* power_level *)
          let event_content =
            Events.Event_content.Power_levels
              (match Request.get_power_level_content_override create_room with
              | None ->
                Events.Event_content.Power_levels.make
                  ?users:(Some [user_id, 100])
                  ?users_default:(Some (-1)) ()
              | Some power_level ->
                let user_default =
                  match
                    Events.Event_content.Power_levels.get_users_default
                      power_level
                  with
                  | Some _ as user_default -> user_default
                  | None -> Some (-1) in
                let users =
                  match
                    Events.Event_content.Power_levels.get_users power_level
                  with
                  | Some _ as users -> users
                  | None -> Some [user_id, 100] in
                let power_level =
                  Events.Event_content.Power_levels.set_users_default
                    power_level user_default in
                Events.Event_content.Power_levels.set_users power_level users)
          in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:[member_hash] ~prev_state:[] ~room_id ~sender:user_id
              ~signatures:[] ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let power_level_hash = "$" ^ event_id in
          let auth_events = power_level_hash :: auth_events in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["rooms"; room_id; "state"; "m.room.power_levels"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in

          (* join_rules *)
          let event_content =
            Events.Event_content.Join_rules
              (Events.Event_content.Join_rules.make ~join_rule:Public ()) in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:[power_level_hash] ~prev_state:[] ~room_id
              ~sender:user_id ~signatures:[] ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["rooms"; room_id; "state"; "m.room.join_rules"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* history_visibility *)
          let event_content =
            Events.Event_content.History_visibility
              (Events.Event_content.History_visibility.make ~visibility:Shared
                 ()) in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:["$" ^ event_id] ~prev_state:[] ~room_id
              ~sender:user_id ~signatures:[] ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v
                 ["rooms"; room_id; "state"; "m.room.history_visibility"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* name *)
          let name =
            match Request.get_name create_room with
            | Some name -> name
            | None -> alias in
          let event_content =
            Events.Event_content.Name (Events.Event_content.Name.make ~name ())
          in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:["$" ^ event_id] ~prev_state:[] ~room_id
              ~sender:user_id ~signatures:[] ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let depth = succ depth in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v ["rooms"; room_id; "state"; "m.room.name"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* canonical_alias *)
          let event_content =
            Events.Event_content.Canonical_alias
              (Events.Event_content.Canonical_alias.make ~alias:(Some alias) ())
          in
          let event =
            Events.Pdu.make ~auth_events ~event_content ~depth
              ~origin:t.server_name ~origin_server_ts:(time ())
              ~prev_events:["$" ^ event_id] ~prev_state:[] ~room_id
              ~sender:user_id ~signatures:[] ~state_key:""
              ~event_type:(Events.Event_content.get_type event_content)
              () in
          let event = compute_hash_and_sign t event in
          let event_id = compute_event_reference_hash event in
          let json_event =
            Json_encoding.construct Events.Pdu.encoding event
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree
              (Store.Key.v
                 ["rooms"; room_id; "state"; "m.room.canonical_alias"])
              event_id in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event
          in
          (* Save the most recent event id *)
          let json =
            Json_encoding.(construct (list string) [event_id])
            |> Ezjsonm.value_to_string in
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["rooms"; room_id; "head"]) json
          in
          (* Saving the alias in the aliases folder *)
          let%lwt tree =
            Store.Tree.add tree (Store.Key.v ["aliases"; alias]) room_id in
          (* saving update tree *)
          let%lwt return =
            Store.set_tree
              ~info:(Helper.info t ~message:"create room")
              store (Store.Key.v []) tree in
          match return with
          | Ok () ->
            let response =
              Response.make ~room_id ()
              |> Json_encoding.construct Response.encoding
              |> Ezjsonm.value_to_string in
            Dream.json response
          | Error write_error ->
            Dream.error (fun m ->
                m "Write error: %a"
                  (Irmin.Type.pp Store.write_error_t)
                  write_error);
            Dream.json ~status:`Internal_Server_Error
              {|{"errcode": "M_UNKNOWN"}|})
        | None -> assert false (* should not happend *))))
  | _ ->
    Dream.json ~status:`Bad_Request
      {|{"errcode": "M_FORBIDDEN"; "error": "Current implementation only accepts public rooms"}|}

(* FIXME: Rework this, it is broken, canonical alias is always parsed from json *)
let state t request ~with_state_key =
  let open Room_event.Put.State_event in
  let%lwt body = Dream.body request in
  let state =
    Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
  in
  match Dream.local logged_user request with
  | Some user_id ->
    let room_id = Dream.param "room_id" request in
    let event_type = Dream.param "event_type" request in
    let%lwt b = Helper.is_room_user room_id user_id in
    if b then (
      let id = "$" ^ Uuidm.(v `V4 |> to_string) ^ ":" ^ t.server_name in
      let event_content = Room_event.Put.State_event.Request.get_event state in
      let state_key, store_key =
        if with_state_key then
          let state_key = Dream.param "state_key" request in
          state_key, ["rooms"; room_id; "state"; event_type; state_key]
        else "", ["rooms"; room_id; "state"; event_type] in
      let event =
        Events.State_event.make
          ~room_event:
            (Events.Room_event.make
               ~event:(Events.Event.make ~event_content ())
               ~event_id:id ~sender:user_id
               ~origin_server_ts:((Unix.time () |> Float.to_int) * 1000)
               ())
          ~state_key () in
      let json_event =
        Json_encoding.construct Events.State_event.encoding event
        |> Ezjsonm.value_to_string in
      let%lwt tree = Store.tree store in
      let%lwt tree = Store.Tree.add tree (Store.Key.v store_key) json_event in
      (* saving update tree *)
      let%lwt return =
        Store.set_tree
          ~info:(Helper.info t ~message:"set room state")
          store (Store.Key.v []) tree in
      match return with
      | Ok () ->
        let event_id = "$" ^ id ^ ":" ^ t.server_name in
        let response =
          Response.make ~event_id ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response
      | Error write_error ->
        Dream.error (fun m ->
            m "Write error: %a" (Irmin.Type.pp Store.write_error_t) write_error);
        Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKNOWN"}|})
    else Dream.json ~status:`Unauthorized {|{"errcode": "M_FORBIDDEN"}|}
  | None -> assert false

(** Notes:
    - Properly use the ID
    - Support all message type
    - Ensure indempotency
  *)
let send t request =
  let open Room_event.Put.Message_event in
  let%lwt body = Dream.body request in
  let message =
    Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
  in
  match Dream.local logged_user request with
  | Some user_id ->
    let room_id = Dream.param "room_id" request in
    let%lwt b = Helper.is_room_user room_id user_id in
    if b then (
      let%lwt tree = Store.tree store in
      let%lwt state_tree =
        Store.Tree.get_tree tree (Store.Key.v ["rooms"; room_id; "state"]) in
      let%lwt create_event =
        Store.Tree.get state_tree (Store.Key.v ["m.room.create"]) in
      let%lwt power_level =
        Store.Tree.get state_tree (Store.Key.v ["m.room.power_levels"]) in
      let%lwt member =
        Store.Tree.get state_tree (Store.Key.v ["m.room.member"; user_id]) in
      let message_content = Request.get_event message in
      let event_content = Events.Event_content.Message message_content in
      let%lwt old_depth, prev_events = get_room_prev_events room_id in
      let depth = old_depth + 1 in
      let event =
        Events.Pdu.make
          ~auth_events:["$" ^ create_event; "$" ^ power_level; "$" ^ member]
          ~event_content ~depth ~origin:t.server_name
          ~origin_server_ts:(time ()) ~prev_events ~prev_state:[] ~room_id
          ~sender:user_id ~signatures:[]
          ~event_type:(Events.Event_content.get_type event_content)
          () in
      let event = compute_hash_and_sign t event in
      let event_id = compute_event_reference_hash event in
      let json_event =
        Json_encoding.construct Events.Pdu.encoding event
        |> Ezjsonm.value_to_string in
      let%lwt tree =
        Store.Tree.add tree (Store.Key.v ["events"; event_id]) json_event in
      (* save the new head of the events *)
      let json =
        Json_encoding.(construct (list string) [event_id])
        |> Ezjsonm.value_to_string in
      let%lwt tree =
        Store.Tree.add tree (Store.Key.v ["rooms"; room_id; "head"]) json in
      (* saving update tree *)
      let%lwt return =
        Store.set_tree
          ~info:(Helper.info t ~message:"set message")
          store (Store.Key.v []) tree in
      match return with
      | Ok () ->
        let%lwt () = notify_room_servers t room_id [event] in
        let event_id = "$" ^ event_id ^ ":" ^ t.server_name in
        let response =
          Response.make ~event_id ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response
      | Error write_error ->
        Dream.error (fun m ->
            m "Write error: %a" (Irmin.Type.pp Store.write_error_t) write_error);
        Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKNOWN"}|})
    else Dream.json ~status:`Unauthorized {|{"errcode": "M_FORBIDDEN"}|}
  | None -> assert false
(* Should obviously return a 401 instead: If this case
   was to happen, it would mean that the user of the matrix library has
   forgotten to use the authentication middleware. But we might prefer having
   a clean error than a simple raise. Or maybe a 5XX would be more appropriate *)

let router (t : Common_routes.t) =
  Dream.router
    [
      Dream.scope "/_matrix" []
        [
          Dream.scope "/client" []
            [
              Dream.scope "/r0" []
                [
                  Dream.scope "" [Rate_limit.rate_limited]
                    [Dream.post "/login" (login t)];
                  Dream.get "/directory/room/:alias" resolve_alias;
                  Dream.scope "" [is_logged]
                    [
                      Dream.post "/createRoom" (create_room t);
                      Dream.put "/rooms/:room_id/state/:event_type/:state_key"
                        (state t ~with_state_key:true);
                      Dream.put "/rooms/:room_id/state/:event_type/"
                        (state t ~with_state_key:false);
                      Dream.put "/rooms/:room_id/send/:event_type/:txn_id"
                        (send t); Dream.post "/logout" (logout t);
                    ];
                ];
            ];
        ];
    ]
