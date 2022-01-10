open Store
open Matrix_common
open Matrix_stos
open Common_routes

(* Notes:
   Those notes are server wide:
   - The support for ACLs should be implemented
   - The event merging algorithm is simply non existant.
   - Events stay forever in the database, maybe crunching them after some time
     could be a good idea ?
*)

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
  module Middleware = Middleware.Make (Pclock) (Time) (Stack)
  module Helper = Helper.Make (Pclock) (Time) (Stack)
  open Helper

  let sign t encoding =
    Signatures.encoding
      [t.server_name, ["ed25519:" ^ t.key_name, t.priv_key]]
      encoding

  module Version = struct
    let get _ =
      let open Version in
      let response =
        Response.make ~name:"%%NAME%%" ~version:"%%VERSION%%" ()
        |> Json_encoding.construct Response.encoding
        |> Ezjsonm.value_to_string in
      Dream.json response
  end

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
                [
                  ( "ed25519:" ^ t.key_name,
                    Response.Verify_key.make ~key:base64_key () );
                ]
              ~old_verify_keys:[]
              ~valid_until_ts:(time () + 3600)
              ()
            |> Json_encoding.construct (sign t Response.encoding)
            |> Ezjsonm.value_to_string in
          Dream.json response
        | Error (`Msg s) ->
          Dream.error (fun m -> m "Base64 key encode error: %s" s);
          Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKOWN"}|}

      (* Notes:
         - Only fetching the key of the server for now
         - Should query other keys as well
         - Use a proper and appropriate validity time
      *)
      let indirect_batch_query t request =
        let open Key.Indirect_batch_query in
        let%lwt _request, body = Middleware.body request in
        let%lwt body = body in
        let requested_keys =
          Json_encoding.destruct Request.encoding
            (Ezjsonm.value_from_string body)
          |> Request.get_server_keys in
        let server_keys =
          match List.assoc_opt t.server_name requested_keys with
          | None -> []
          | Some keys -> (
            match List.assoc_opt ("ed25519:" ^ t.key_name) keys with
            | None -> []
            | Some _query_criteria -> (
              match
                Base64.encode ~pad:false
                  (Cstruct.to_string
                  @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct t.pub_key)
              with
              | Ok base64_key ->
                [
                  Key.Server_key.make ~server_name:t.server_name
                    ~verify_keys:
                      [
                        ( "ed25519:" ^ t.key_name,
                          Key.Server_key.Verify_key.make ~key:base64_key () );
                      ]
                    ~old_verify_keys:[]
                    ~valid_until_ts:(time () + 3600)
                    ();
                ]
              | Error (`Msg _s) -> [])) in
        let response =
          Response.make ~server_keys ()
          |> Json_encoding.construct (sign t Response.encoding)
          |> Ezjsonm.value_to_string in
        Dream.json response

      (* Notes:
         - Only fetching the key of the server for now
         - Should query other keys as well
         - Use a proper and appropriate validity time
      *)
      let indirect_query t request =
        let open Key.Indirect_query in
        let server_name = Dream.param "server_name" request in
        let key_id = Dream.param "key_id" request in
        let server_keys =
          if t.server_name = server_name && "ed25519:" ^ t.key_name = key_id
          then
            match
              Base64.encode ~pad:false
                (Cstruct.to_string
                @@ Mirage_crypto_ec.Ed25519.pub_to_cstruct t.pub_key)
            with
            | Ok base64_key ->
              [
                Key.Server_key.make ~server_name:t.server_name
                  ~verify_keys:
                    [
                      ( "ed25519:" ^ t.key_name,
                        Key.Server_key.Verify_key.make ~key:base64_key () );
                    ]
                  ~old_verify_keys:[]
                  ~valid_until_ts:(time () + 3600)
                  ();
              ]
            | Error (`Msg _s) -> []
          else [] in
        let response =
          Response.make ~server_keys ()
          |> Json_encoding.construct (sign t Response.encoding)
          |> Ezjsonm.value_to_string in
        Dream.json response
    end
  end

  module Public_rooms = struct
    (* Notes:
       - Filter & pagination are ignored for now
    *)
    let get (t : Common_routes.t) _request =
      let open Public_rooms.Get_public_rooms in
      let%lwt tree = Store.tree t.store in
      (* retrieve the list of the rooms *)
      let%lwt rooms = Store.Tree.list tree @@ Store.Key.v ["rooms"] in
      (* filter out the public rooms*)
      let%lwt public_rooms =
        Lwt_list.map_p
          (fun (room_id, room_tree) ->
            (* retrieve the room's canonical_alias if any *)
            let%lwt canonical_alias =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.join_rules"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Canonical_alias canonical_alias ->
                  Lwt.return
                    (Option.join
                    @@ Events.Event_content.Canonical_alias.get_alias
                         canonical_alias)
                | _ -> Lwt.return_none) in
            (* retrieve the room's name if any *)
            let%lwt name =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.name"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Name name ->
                  Lwt.return_some (Events.Event_content.Name.get_name name)
                | _ -> Lwt.return_none) in
            (* retrieve the room's members number *)
            let%lwt num_joined_members =
              let%lwt members =
                Store.Tree.list room_tree
                @@ Store.Key.v ["state"; "m.room.member"] in
              let f n (_, member_tree) =
                let%lwt event_id =
                  Store.Tree.get member_tree @@ Store.Key.v [] in
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Member member ->
                  if
                    Events.Event_content.Member.get_membership member
                    = Events.Event_content.Membership.Join
                  then Lwt.return (n + 1)
                  else Lwt.return n
                | _ -> Lwt.return n in
              Lwt_list.fold_left_s f 0 members in
            (* retrieve the room's topic if any *)
            let%lwt topic =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.topic"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Topic topic ->
                  Lwt.return_some (Events.Event_content.Topic.get_topic topic)
                | _ -> Lwt.return_none) in
            (* retrieve the room's topic if any *)
            let%lwt avatar_url =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.avatar"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Avatar avatar ->
                  Lwt.return_some (Events.Event_content.Avatar.get_url avatar)
                | _ -> Lwt.return_none) in
            (* Notes:
               - aliases are ignored for now
               - as guests are totally ignored, world_readable guest_can_join are
                 set to false
               - federate is not in the documentation, so set to false for now,
                 needs investigation in order to know what it means *)
            let room =
              Response.Public_rooms_chunk.make ~aliases:[] ?canonical_alias
                ?name ~num_joined_members ~room_id ?topic ~world_readable:false
                ~guest_can_join:false ?avatar_url ~federate:false () in
            Lwt.return room)
          rooms in
      let response =
        Response.make ~chunk:public_rooms
          ~total_room_count_estimate:(List.length rooms) ()
        |> Json_encoding.construct Response.encoding
        |> Ezjsonm.value_to_string in
      Dream.json response

    (* Notes:
       - Filter & pagination are ignored for now
    *)
    let post (t : Common_routes.t) _request =
      let open Public_rooms.Filter_public_rooms in
      let%lwt tree = Store.tree t.store in
      (* retrieve the list of the rooms *)
      let%lwt rooms = Store.Tree.list tree @@ Store.Key.v ["rooms"] in
      (* filter out the public rooms*)
      let%lwt public_rooms =
        Lwt_list.map_p
          (fun (room_id, room_tree) ->
            (* retrieve the room's canonical_alias if any *)
            let%lwt canonical_alias =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.join_rules"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Canonical_alias canonical_alias ->
                  Lwt.return
                    (Option.join
                    @@ Events.Event_content.Canonical_alias.get_alias
                         canonical_alias)
                | _ -> Lwt.return_none) in
            (* retrieve the room's name if any *)
            let%lwt name =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.name"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Name name ->
                  Lwt.return_some (Events.Event_content.Name.get_name name)
                | _ -> Lwt.return_none) in
            (* retrieve the room's members number *)
            let%lwt num_joined_members =
              let%lwt members =
                Store.Tree.list room_tree
                @@ Store.Key.v ["state"; "m.room.member"] in
              let f n (_, member_tree) =
                let%lwt event_id =
                  Store.Tree.get member_tree @@ Store.Key.v [] in
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Member member ->
                  if
                    Events.Event_content.Member.get_membership member
                    = Events.Event_content.Membership.Join
                  then Lwt.return (n + 1)
                  else Lwt.return n
                | _ -> Lwt.return n in
              Lwt_list.fold_left_s f 0 members in
            (* retrieve the room's topic if any *)
            let%lwt topic =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.topic"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Topic topic ->
                  Lwt.return_some (Events.Event_content.Topic.get_topic topic)
                | _ -> Lwt.return_none) in
            (* retrieve the room's topic if any *)
            let%lwt avatar_url =
              let%lwt event_id =
                Store.Tree.find room_tree
                @@ Store.Key.v ["state"; "m.room.avatar"] in
              match event_id with
              | None -> Lwt.return_none
              | Some event_id -> (
                let%lwt json =
                  Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
                let event =
                  Json_encoding.destruct Events.State_event.encoding
                    (Ezjsonm.value_from_string json) in
                match Events.State_event.get_event_content event with
                | Avatar avatar ->
                  Lwt.return_some (Events.Event_content.Avatar.get_url avatar)
                | _ -> Lwt.return_none) in
            (* Notes:
               - aliases are ignored for now
               - as guests are totally ignored, world_readable guest_can_join are
                 set to false
               - federate is not in the documentation, so set to false for now,
                 needs investigation in order to know what it means *)
            let room =
              Response.Public_rooms_chunk.make ~aliases:[] ?canonical_alias
                ?name ~num_joined_members ~room_id ?topic ~world_readable:false
                ~guest_can_join:false ?avatar_url ~federate:false () in
            Lwt.return room)
          rooms in
      let response =
        Response.make ~chunk:public_rooms
          ~total_room_count_estimate:(List.length rooms) ()
        |> Json_encoding.construct Response.encoding
        |> Ezjsonm.value_to_string in
      Dream.json response
  end

  module Join = struct
    (* Notes:
       - Work on supporting other room versions in the future ?
    *)
    let make t request =
      let open Joining_rooms.Make_join in
      let versions = Dream.queries "ver" request in
      let user_id = Dream.param "user_id" request in
      let room_id = Dream.param "room_id" request in
      (* FIX-ME: Hardcoded room version to 6 *)
      let room_version = "6" in
      if List.exists (String.equal room_version) versions then
        (* fetch the auth events *)
        let%lwt state_tree =
          Store.get_tree t.store (Store.Key.v ["rooms"; room_id; "state"]) in
        let%lwt create_event =
          Store.Tree.get state_tree (Store.Key.v ["m.room.create"]) in
        let%lwt power_level =
          Store.Tree.get state_tree (Store.Key.v ["m.room.power_levels"]) in
        let%lwt join_rules =
          Store.Tree.get state_tree (Store.Key.v ["m.room.join_rules"]) in
        let event_content =
          Events.Event_content.Member
            (Events.Event_content.Member.make ~membership:Join ()) in
        let%lwt old_depth, prev_events = get_room_prev_events t room_id in
        let depth = old_depth + 1 in
        let origin =
          match Dream.local Middleware.logged_server request with
          | Some logged_server -> logged_server
          | None -> t.server_name in
        let event_template =
          Events.Pdu.make
            ~auth_events:
              ["$" ^ create_event; "$" ^ power_level; "$" ^ join_rules]
            ~event_content ~depth ~origin ~origin_server_ts:(time ())
            ~prev_events ~prev_state:[] ~room_id ~sender:user_id ~signatures:[]
            ~state_key:user_id
            ~event_type:(Events.Event_content.get_type event_content)
            () in
        let response =
          Response.make ~room_version ~event_template ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response
      else
        Dream.json ~status:`Bad_Request
          (Fmt.str
             {|{"errcode": "M_INCOMPATIBLE_ROOM_VERSION", "error": "Your homeserver does not support the features required to join this room", "room_version": %s}|}
             room_version)

    (* Notes:
       - verify previous state
       - verify the given event id
       - better error returns
    *)
    let send t request =
      let open Joining_rooms.Send_join.V2 in
      let _event_id = Dream.param "event_id" request in
      let room_id = Dream.param "room_id" request in
      let%lwt _request, body = Middleware.body request in
      let%lwt body = body in
      let member_event =
        Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
      in
      (* Verify if the event really comes from the server *)
      let origin = Option.get @@ Dream.local Middleware.logged_server request in
      let%lwt verify = Helper.check_event_signature t origin member_event in
      if not verify then
        Dream.json ~status:`Forbidden {|{"errcode": "M_UNKNOWN"}|}
      else
        (* Verify that we have a member event *)
        match Events.Pdu.get_event_content member_event with
        | Events.Event_content.Member _ -> (
          let event_origin = Events.Pdu.get_origin member_event in
          if event_origin <> origin then
            Dream.json ~status:`Forbidden {|{"errcode": "M_UNKNOWN"}|}
          else
            let member_event = compute_hash_and_sign t member_event in
            let event_id = compute_event_reference_hash member_event in
            (* need error handling *)
            let state_key =
              Events.Pdu.get_state_key member_event |> Option.get in
            (* Check if the user is not already in the room *)
            let%lwt presence = Helper.is_room_user t room_id state_key in
            if presence then
              Dream.json ~status:`Bad_Request
                {|{"errcode": "M_UNKNOWN"; "error": "User is already in the room"}|}
            else
              let json_event =
                Json_encoding.construct Events.Pdu.encoding member_event
                |> Ezjsonm.value_to_string in
              let%lwt tree = Store.tree t.store in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v
                     ["rooms"; room_id; "state"; "m.room.member"; state_key])
                  event_id in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["events"; event_id])
                  json_event in
              (* save the new previous event id*)
              let json =
                Json_encoding.(construct (list string) [event_id])
                |> Ezjsonm.value_to_string in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["rooms"; room_id; "head"])
                  json in
              (* saving update tree *)
              let%lwt return =
                Store.set_tree
                  ~info:(Helper.info t ~message:"add joining member")
                  t.store (Store.Key.v []) tree in
              match return with
              | Ok () ->
                (* fetch the state of the room *)
                let%lwt tree = Store.tree t.store in
                let%lwt state_tree =
                  Store.Tree.get_tree tree
                    (Store.Key.v ["rooms"; room_id; "state"]) in
                let%lwt state =
                  Store.Tree.fold
                    ~contents:(fun _ event_id events ->
                      let open Events in
                      let%lwt json =
                        Store.Tree.get tree @@ Store.Key.v ["events"; event_id]
                      in
                      let event =
                        Ezjsonm.from_string json
                        |> Json_encoding.destruct Pdu.encoding in
                      Lwt.return (event :: events))
                    state_tree [] in
                let response =
                  Response.make ~origin:t.server_name ~auth_chain:state ~state
                    ()
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
        | _ -> Dream.json ~status:`Forbidden {|{"errcode": "M_UNKNOWN"}|}
  end

  module Retrieve = struct
    (* Notes:
       - Needs a better error handling *)
    let get_event t request =
      let open Retrieve.Event in
      let event_id = Dream.param "event_id" request in
      let%lwt tree = Store.tree t.store in
      let event_id = Identifiers.Event_id.of_string_exn event_id in
      let%lwt json = Store.Tree.find tree @@ Store.Key.v ["events"; event_id] in
      match json with
      | None -> Dream.json ~status:`Forbidden {|{"errcode": "M_UNKNOWN"}|}
      | Some json ->
        let event =
          Json_encoding.destruct Events.Pdu.encoding
            (Ezjsonm.value_from_string json) in
        let room_id = Events.Pdu.get_room_id event in
        let server_name =
          Option.get @@ Dream.local Middleware.logged_server request in
        let%lwt b = is_room_participant t server_name room_id in
        (* check if the server is in the room *)
        if not b then
          Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
        else
          let response =
            Response.make ~origin:t.server_name ~origin_server_ts:(time ())
              ~pdus:[event] ()
            |> Json_encoding.construct Response.encoding
            |> Ezjsonm.value_to_string in
          Dream.json response
  end

  module Transaction = struct
    (* Notes:
       - Use the transaction ID
       - Notify the servers for all events at once and after they are saved ?
       - Properly implement the user rights
    *)
    let send t request =
      let open Send in
      let txn_id = Dream.param "txn_id" request in
      let%lwt _request, body = Middleware.body request in
      let%lwt body = body in
      let transaction =
        Json_encoding.destruct Request.encoding (Ezjsonm.value_from_string body)
      in
      let pdus = Request.get_pdus transaction in
      let%lwt tree = Store.tree t.store in
      let f (tree, results) event =
        let event = compute_hash_and_sign t event in
        let event_id = compute_event_reference_hash event in
        let full_event_id = "$" ^ event_id ^ ":" ^ Events.Pdu.get_origin event in
        let event_type = Events.Pdu.get_event_type event in
        let room_id = Events.Pdu.get_room_id event in
        (* Verify if the event really comes from the server *)
        let origin =
          Option.get @@ Dream.local Middleware.logged_server request in
        let%lwt verify = Helper.check_event_signature t origin event in
        if not verify then
          Lwt.return
            ( tree,
              ( full_event_id,
                Response.Pdu_processing_result.make
                  ~error:
                    "Distant server dit not sign the event or signature was \
                     forged"
                  () )
              :: results )
        else
          (* check if the server is in the room *)
          let%lwt b = is_room_participant t origin room_id in
          if not b then
            Lwt.return
              ( tree,
                ( full_event_id,
                  Response.Pdu_processing_result.make
                    ~error:"You are not allowed to send a message to this room"
                    () )
                :: results )
          else
            (* check if the user has the rights for this event *)
            (* right now, we only allow a user to leave *)
            let allowed =
              match Events.Pdu.get_event_content event with
              | Member member -> (
                match Events.Event_content.Member.get_membership member with
                | Leave -> true
                | _ -> false)
              | _ -> false in
            let state_key = Events.Pdu.get_state_key event |> Option.get in
            let sender = Events.Pdu.get_sender event in
            if (not allowed) || state_key <> sender then
              Lwt.return
                ( tree,
                  ( full_event_id,
                    Response.Pdu_processing_result.make
                      ~error:
                        "You are not allowed to send a message to this room" ()
                  )
                  :: results )
            else
              (* need error handling *)
              let state_key = Events.Pdu.get_state_key event |> Option.get in
              let json_event =
                Json_encoding.construct Events.Pdu.encoding event
                |> Ezjsonm.value_to_string in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v
                     ["rooms"; room_id; "state"; event_type; state_key])
                  event_id in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["events"; event_id])
                  json_event in
              (* save the new previous event id*)
              let json =
                Json_encoding.(construct (list string) [event_id])
                |> Ezjsonm.value_to_string in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["rooms"; room_id; "head"])
                  json in
              let%lwt () = notify_room_servers t room_id [event] in
              Lwt.return
                ( tree,
                  (full_event_id, Response.Pdu_processing_result.make ())
                  :: results ) in
      let%lwt tree, results = Lwt_list.fold_left_s f (tree, []) pdus in
      (* saving update tree *)
      let message =
        Fmt.str "add transaction %s from %s" txn_id
          (Request.get_origin transaction) in
      let%lwt return =
        Store.set_tree ~info:(Helper.info t ~message) t.store (Store.Key.v [])
          tree in
      match return with
      | Ok () ->
        let response =
          Response.make ~pdus:results ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response
      | Error write_error ->
        Dream.error (fun m ->
            m "Write error: %a" (Irmin.Type.pp Store.write_error_t) write_error);
        Dream.json ~status:`Internal_Server_Error {|{"errcode": "M_UNKNOWN"}|}
  end

  module Backfill = struct
    (* Notes:
       - Use the limit/do the pagination
       - Error handling
       - It's way too bad, needs a lot of rework in order to do less operations
    *)
    let get t request =
      let open Backfill in
      let room_id = Dream.param "room_id" request in
      let v = Dream.queries "v" request in
      let _limit = Dream.query "limit" request in
      let server_name =
        Option.get @@ Dream.local Middleware.logged_server request in
      let%lwt b = is_room_participant t server_name room_id in
      (* check if the server is in the room or if no events were asked for *)
      if (not b) || List.length v == 0 then
        Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
      else
        let%lwt tree = Store.tree t.store in
        let rec f (event_ids, events) event_id =
          let event_id = Identifiers.Event_id.of_string_exn event_id in
          if List.exists (String.equal event_id) event_ids then
            Lwt.return (event_ids, events)
          else
            let%lwt json =
              Store.Tree.find tree @@ Store.Key.v ["events"; event_id] in
            match json with
            | None -> Lwt.return (event_ids, events)
            | Some json ->
              let event =
                Json_encoding.destruct Events.Pdu.encoding
                  (Ezjsonm.value_from_string json) in
              (* check if the event is in the room *)
              if room_id <> Events.Pdu.get_room_id event then
                Lwt.return (event_ids, events)
              else
                let prev_event = Events.Pdu.get_prev_events event in
                Lwt_list.fold_left_s f
                  (event_id :: event_ids, event :: events)
                  prev_event in
        let%lwt _, events = Lwt_list.fold_left_s f ([], []) v in
        let response =
          Response.make ~origin:t.server_name ~origin_server_ts:(time ())
            ~pdus:events ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response

    (* Notes:
       - Use the limit/do the pagination
       - Error handling
       - It's way too bad, needs a lot of rework in order to do less operations
    *)
    let get_missing_event t request =
      let open Get_missing_events in
      let room_id = Dream.param "room_id" request in
      let server_name =
        Option.get @@ Dream.local Middleware.logged_server request in
      let%lwt b = is_room_participant t server_name room_id in
      (* check if the server is in the room *)
      if not b then Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
      else
        let%lwt _request, body = Middleware.body request in
        let%lwt body = body in
        let missing_events =
          Json_encoding.destruct Request.encoding
            (Ezjsonm.value_from_string body) in
        let earliest_events = Request.get_earliest_events missing_events in
        let lastest_events = Request.get_lastest_events missing_events in
        let _limit =
          Option.value ~default:10 @@ Request.get_limit missing_events in
        let _min_depth =
          Option.value ~default:0 @@ Request.get_min_depth missing_events in

        let%lwt tree = Store.tree t.store in
        let rec f (event_ids, events) event_id =
          let event_id = Identifiers.Event_id.of_string_exn event_id in
          if List.exists (String.equal event_id) event_ids then
            Lwt.return (event_ids, events)
          else
            let%lwt json =
              Store.Tree.find tree @@ Store.Key.v ["events"; event_id] in
            match json with
            | None -> Lwt.return (event_ids, events)
            | Some json ->
              let event =
                Json_encoding.destruct Events.Pdu.encoding
                  (Ezjsonm.value_from_string json) in
              (* check if the event is in the room *)
              if room_id <> Events.Pdu.get_room_id event then
                Lwt.return (event_ids, events)
              else
                let prev_event = Events.Pdu.get_prev_events event in
                Lwt_list.fold_left_s f
                  (event_id :: event_ids, event :: events)
                  prev_event in
        let%lwt _, events =
          Lwt_list.fold_left_s f (earliest_events, []) lastest_events in
        let response =
          Response.make ~events ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response
  end

  module Event_auth = struct
    let get t request =
      let open Event_auth in
      let room_id = Dream.param "room_id" request in
      let server_name =
        Option.get @@ Dream.local Middleware.logged_server request in
      let%lwt b = is_room_participant t server_name room_id in
      (* check if the server is in the room or if no events were asked for *)
      if not b then Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
      else
        let event_id = Dream.param "event_id" request in
        (* fetch the event *)
        let event_id = Identifiers.Event_id.of_string_exn event_id in
        let%lwt tree = Store.tree t.store in
        let%lwt json =
          Store.Tree.find tree @@ Store.Key.v ["events"; event_id] in
        match json with
        | None -> Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
        | Some json ->
          let event =
            Json_encoding.destruct Events.Pdu.encoding
              (Ezjsonm.value_from_string json) in
          (* check if the event is in the room *)
          if room_id <> Events.Pdu.get_room_id event then
            Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
          else
            let auth_events = Events.Pdu.get_auth_events event in
            let f l event_id =
              let event_id = Identifiers.Event_id.of_string_exn event_id in
              let%lwt json =
                Store.Tree.find tree @@ Store.Key.v ["events"; event_id] in
              match json with
              | None -> Lwt.return l
              | Some json ->
                let event =
                  Json_encoding.destruct Events.Pdu.encoding
                    (Ezjsonm.value_from_string json) in
                Lwt.return (event :: l) in
            let%lwt auth_chain = Lwt_list.fold_left_s f [] auth_events in
            let response =
              Response.make ~auth_chain ()
              |> Json_encoding.construct Response.encoding
              |> Ezjsonm.value_to_string in
            Dream.json response
  end

  module State = struct
    (* Notes:
       - Those endpoints should be implemented as they might be used by other
         servers when they stumble upon unkown states/wants a clearer idea of the
         room state at a given point
       - A forbidden is not the right answer for not however
    *)
    let get _ = Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}

    (* Notes:
       - Those endpoints should be implemented as they might be used by other
         servers when they stumble upon unkown states/wants a clearer idea of the
         room state at a given point
         - A forbidden is not the right answer for not however
    *)
    let get_ids _ = Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
  end

  (* --- Deactivated endpoints --- *)
  (* The rights which we currently give to the joining users do not allow to
     invite other users, therefore we simply forbid them to try to do it.
  *)
  module Invite = struct
    let invite _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "error": "User cannot invite the target user."}|}
  end

  (* The leave endpoints are only used when trying to refuse an invitation to a
     room, if the user is already in the room he would use the send endpoint and
     directly share his new member event to the other rooms. Therefore as we are
     refusing any invitations, there is no point in implementing this for now
  *)
  module Leave = struct
    let make _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "User is not in the room"}|}

    let send _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "User is not in the room"}|}
  end

  module Query = struct
    (* Notes:
       - Do we want to give the participants to foreign servers ? The spec
         doesnt seem to consider it.
    *)
    let directory t request =
      let open Query.Directory in
      let room_alias = Dream.query "room_alias" request in
      match room_alias with
      | None ->
        Dream.json ~status:`Not_Found
          {|{"errcode": "M_NOT_FOUND", "Room alias not found."}|}
      | Some room_alias -> (
        let room_alias, _ = Identifiers.Room_alias.of_string_exn room_alias in
        let%lwt tree = Store.tree t.store in
        let%lwt room_id =
          Store.Tree.find tree @@ Store.Key.v ["aliases"; room_alias] in
        match room_id with
        | None ->
          Dream.json ~status:`Not_Found
            {|{"errcode": "M_NOT_FOUND", "Room alias not found."}|}
        | Some room_id ->
          let%lwt servers = fetch_joined_servers t room_id in
          let response =
            Response.make ~room_id ~servers ()
            |> Json_encoding.construct Response.encoding
            |> Ezjsonm.value_to_string in
          Dream.json response)

    let profile t request =
      let open Query.Profile in
      let user_id = Dream.query "user_id" request in
      match user_id with
      | None ->
        Dream.json ~status:`Not_Found
          {|{"errcode": "M_NOT_FOUND", "User does not exist."}|}
      | Some user_id -> (
        let%lwt tree = Store.tree t.store in
        let%lwt user_tree =
          Store.Tree.find_tree tree @@ Store.Key.v ["users"; user_id] in
        match user_tree with
        | None ->
          Dream.json ~status:`Not_Found
            {|{"errcode": "M_NOT_FOUND", "User does not exist."}|}
        | Some user_tree ->
          let%lwt avatar_url, displayname =
            let field = Dream.query "field" request in
            match field with
            | Some "avatar_url" -> Lwt.return (None, None)
            | _ ->
              let%lwt displayname =
                Store.Tree.get user_tree @@ Store.Key.v ["username"] in
              Lwt.return (None, Some displayname) in
          let response =
            Response.make ?avatar_url ?displayname ()
            |> Json_encoding.construct Response.encoding
            |> Ezjsonm.value_to_string in
          Dream.json response)

    (* Notes:
       - This extension is not supported *)
    let query _ = Dream.json ~status:`Not_Found {|{"errcode": "M_NOT_FOUND"}|}
  end

  module User = struct
    (* Notes:
       - As we do not have keys for devices nor users, we simply return some
         default informations and empty device list *)
    let devices t request =
      let open User.Devices in
      let user_id = Dream.param "user_id" request in
      let%lwt tree = Store.tree t.store in
      let%lwt user_tree =
        Store.Tree.find_tree tree @@ Store.Key.v ["users"; user_id] in
      match user_tree with
      | None ->
        Dream.json ~status:`Not_Found
          {|{"errcode": "M_NOT_FOUND", "User does not exist."}|}
      | Some _user_tree ->
        let response =
          Response.make ~devices:[] ~stream_id:0 ~user_id ()
          |> Json_encoding.construct Response.encoding
          |> Ezjsonm.value_to_string in
        Dream.json response

    (* All of the rooms are public for now, therefore, end-to-end encryption is
       never going to be used (may change in the future with "public but opaque
       rooms"). All of the associated endpoints are therefore disabled *)
    let claim_key _ =
      Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}

    (* All of the rooms are public for now, therefore, end-to-end encryption is
       never going to be used (may change in the future with "public but opaque
       rooms"). All of the associated endpoints are therefore disabled *)
    let query_key _ =
      Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}
  end

  (* All of our users are bots only, therefore thirdparty linking have no uses.
     All the associated endpoints are disabled.
  *)
  module Thirdparty = struct
    let onbind _ = Dream.json ~status:`Forbidden {|{"errcode": "M_FORBIDDEN"}|}

    let exchange _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "User is not in the room"}|}
  end

  (* All of our users are bots only, therefore the openid api will never
     be used.
  *)
  module Openid = struct
    let userinfo _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_UNKNOWN_TOKEN", "Access token unknown or expired"}|}
  end

  (* --- Deprecated endpoints --- *)
  (* Those endpoints are ancient endpoints registered under the v1 path. They are
     only for version 1 and 2 rooms which we do not support at the moment,
     therefore we are simply refusing them for now.
  *)
  module Deprecated = struct
    let join _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "v1 is not supported, use v2 instead"}|}

    let invite _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "v1 is not supported, use v2 instead"}|}

    let leave _ =
      Dream.json ~status:`Forbidden
        {|{"errcode": "M_FORBIDDEN", "v1 is not supported, use v2 instead"}|}
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
                    Dream.get "/version" Version.get;
                    Dream.put "/3pid/onbind" Thirdparty.onbind;
                    Dream.get "/openid/userinfo" Openid.userinfo;
                    Dream.scope ""
                      [Middleware.is_logged_server t]
                      [
                        Dream.put "/send/:txn_id" (Transaction.send t);
                        Dream.get "/event_auth/:room_id/:event_id"
                          (Event_auth.get t);
                        Dream.get "/backfill/:room_id" (Backfill.get t);
                        Dream.post "/get_missing_events/:room_id"
                          (Backfill.get_missing_event t);
                        Dream.get "/state/:room_id" State.get;
                        Dream.get "/state_ids/:room_id" State.get_ids;
                        Dream.get "/event/:event_id" (Retrieve.get_event t);
                        Dream.get "/make_join/:room_id/:user_id" (Join.make t);
                        Dream.put "/send_join/:room_id/:event_id"
                          Deprecated.join;
                        Dream.put "/invite/:room_id/:event_id" Deprecated.invite;
                        Dream.get "/make_leave/:room_id/:user_id" Leave.make;
                        Dream.put "/send_leave/:room_id/:event_id"
                          Deprecated.leave;
                        Dream.put "/exchange_third_party_invite/:room_id"
                          Thirdparty.exchange;
                        Dream.get "/publicRooms" (Public_rooms.get t);
                        Dream.post "/publicRooms" (Public_rooms.post t);
                        Dream.scope "/query" []
                          [
                            Dream.get "/:query_type" Query.query;
                            Dream.get "/directory" (Query.directory t);
                            Dream.get "/profile" (Query.profile t);
                          ];
                        Dream.scope "/user" []
                          [
                            Dream.get "/devices/:user_id" (User.devices t);
                            Dream.scope "/keys" []
                              [
                                Dream.post "/claim" User.claim_key;
                                Dream.post "/query" User.query_key;
                              ];
                          ];
                      ];
                  ];
                Dream.scope "/v2" []
                  [
                    Dream.scope ""
                      [Middleware.is_logged_server t]
                      [
                        Dream.put "/send_join/:room_id/:event_id" (Join.send t);
                        Dream.put "/invite/:room_id/:event_id" Invite.invite;
                        Dream.put "/send_leave/:room_id/:event_id" Leave.send;
                      ];
                  ];
              ];
            Dream.scope "/key/v2" []
              [
                Dream.get "/server" (Key.V2.direct_query t);
                Dream.get "/server/:key_id" (Key.V2.direct_query t);
                Dream.get "/query/:server_name/:key_id"
                  (Key.V2.indirect_query t);
                Dream.post "/query" (Key.V2.indirect_batch_query t);
              ];
          ];
      ]
end
