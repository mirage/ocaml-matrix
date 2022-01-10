open Matrix_common
open Store

module Make
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)

  let is_room_user (t : Common_routes.t) room_id user_id =
    let%lwt tree = Store.tree t.store in
    let%lwt event_id =
      Store.Tree.find tree
        (Store.Key.v ["rooms"; room_id; "state"; "m.room.member"; user_id])
    in
    match event_id with
    | None -> Lwt.return false
    | Some event_id -> (
      let%lwt state_event =
        Store.Tree.get tree (Store.Key.v ["events"; event_id]) in
      let state_event =
        Json_encoding.destruct Events.State_event.encoding
          (Ezjsonm.value_from_string state_event) in
      match Events.State_event.get_event_content state_event with
      | Member member -> (
        match Events.Event_content.Member.get_membership member with
        | Join -> Lwt.return true
        | Leave -> Lwt.return false
        | _ -> assert false)
      | _ -> assert false)

  let time () = Unix.time () |> Float.to_int |> ( * ) 1000

  let info (t : Common_routes.t) ?(message = "") () =
    Irmin.Info.v
      ~date:(Int64.of_float (Unix.gettimeofday ()))
      ~author:t.server_name message

  let compute_hash_and_sign (t : Common_routes.t) pdu =
    let open Events in
    (* ensure already computed hash does not interfere *)
    let pdu = Pdu.set_hashes pdu None in
    (* generate hash *)
    let string_pdu =
      Json_encoding.construct Pdu.encoding pdu
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    let sha256 =
      Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string string_pdu)
      |> Cstruct.to_string
      |> Base64.encode_string ~pad:false ~alphabet:Base64.default_alphabet in
    let hash = Pdu.Hashes.make ~sha256 () in
    let pdu = Pdu.set_hashes pdu (Some hash) in
    (* sign pdu *)
    let string_pdu =
      Json_encoding.construct Pdu.redact pdu
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    let signature =
      Mirage_crypto_ec.Ed25519.sign ~key:t.priv_key
        (Cstruct.of_string string_pdu)
      |> Cstruct.to_string
      |> Base64.encode_string ~pad:false in
    let signatures = Pdu.get_signatures pdu in
    let signatures =
      if List.mem_assoc t.server_name signatures then
        List.map
          (fun (server_name, l) ->
            let l =
              if server_name = t.server_name then
                ("ed25519:" ^ t.key_name, signature) :: l
              else l in
            server_name, l)
          signatures
      else (t.server_name, ["ed25519:" ^ t.key_name, signature]) :: signatures
    in
    let pdu = Pdu.set_signatures pdu signatures in
    pdu

  let compute_event_reference_hash ?(alphabet = Base64.uri_safe_alphabet) pdu =
    let open Events in
    (* generate hash *)
    let string_pdu =
      Json_encoding.construct Pdu.redact pdu
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    let sha256 =
      Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string string_pdu)
      |> Cstruct.to_string
      |> Base64.encode_string ~pad:false ~alphabet in
    sha256

  let is_valid_key key_tree =
    let%lwt valid_until =
      Store.Tree.get key_tree @@ Store.Key.v ["valid_until"] in
    let expires_at = Float.of_string valid_until in
    let current_time = Unix.gettimeofday () in
    Lwt.return (expires_at > current_time)

  (* Notes:
    - Always consider that we have a domain name
  *)
  let resolve_server_name (t : Common_routes.t) server_name =
    let open Matrix_stos.Well_known in
    let uri =
      Uri.make ~scheme:"https" ~port:8448 ~host:server_name
        ~path:("/.well-known/matrix/server")
        () in
    let headers = Cohttp.Header.of_list ["Content-length", "0"] in
    let%lwt resp, body = Paf_cohttp.get ~headers ~ctx:t.ctx uri in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    match Cohttp_lwt.Response.status resp with
    | `OK ->
      (let well_known =
        Json_encoding.destruct Response.encoding (Ezjsonm.value_from_string body)
      in
      let server = Response.get_server well_known in
      match server with
      | None -> Lwt.return (server_name, 8448)
      | Some server ->
        let server_l = String.split_on_char ':' server in
        if List.length server_l > 1
        then
          Lwt.return (List.nth server_l 0, int_of_string @@ List.nth server_l 1)
        else
          Lwt.return (server, 8448))
    | _ -> Lwt.return (server_name, 8448)

  let fetching_key (t : Common_routes.t) server_name key_id =
    let open Matrix_stos.Key.Direct_query in
    let%lwt host, port = resolve_server_name t server_name in
    let uri =
      Uri.make ~scheme:"https" ~port ~host
        ~path:("/_matrix/key/v2/server/" ^ key_id)
        () in
    let headers = Cohttp.Header.of_list ["Content-length", "0"] in
    let%lwt _resp, body = Paf_cohttp.get ~headers ~ctx:t.ctx uri in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    let direct_keys =
      Json_encoding.destruct Response.encoding (Ezjsonm.value_from_string body)
    in
    (* we should use the signatures *)
    let verify_keys = Response.get_verify_keys direct_keys in
    let valid_until = Response.get_valid_until_ts direct_keys in
    match List.assoc_opt key_id verify_keys, valid_until with
    | Some key, Some valid_until ->
      let key = Response.Verify_key.get_key key in
      Lwt.return_some (key, valid_until)
    | _ -> Lwt.return_none

  let is_valid_event_signature signature event key =
    let open Events in
    let string_pdu =
      Json_encoding.construct Pdu.redact event
      |> Json_encoding.canonize
      |> Ezjsonm.value_to_string in
    match Base64.decode ~pad:false key with
    | Error _ -> Lwt.return false
    | Ok key -> (
      match Mirage_crypto_ec.Ed25519.pub_of_cstruct (Cstruct.of_string key) with
      | Error _ -> Lwt.return false
      | Ok key -> (
        match Base64.decode ~pad:false signature with
        | Error _ -> Lwt.return false
        | Ok signature ->
          let verify =
            Mirage_crypto_ec.Ed25519.verify ~key
              (Cstruct.of_string signature)
              ~msg:(Cstruct.of_string string_pdu) in
          Lwt.return verify))

  let check_event_signature (t : Common_routes.t) origin event =
    let open Events in
    let signatures = Pdu.get_signatures event in
    match List.assoc_opt origin signatures with
    | None -> Lwt.return_false
    | Some signatures ->
      let f (key_id, signature) =
        (* check if we have the key, if not, try to fetch it *)
        let%lwt tree = Store.tree t.store in
        let%lwt key_tree =
          Store.Tree.find_tree tree @@ Store.Key.v ["keys"; origin; key_id]
        in
        let%lwt key_tree =
          if key_tree = None then
            let%lwt key_s = fetching_key t origin key_id in
            match key_s with
            | Some (key_s, valid_until) -> (
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["keys"; origin; key_id; "key"])
                  key_s in
              let%lwt tree =
                Store.Tree.add tree
                  (Store.Key.v ["keys"; origin; key_id; "valid_until"])
                  (Int.to_string valid_until) in
              let%lwt return =
                Store.set_tree
                  ~info:(info t ~message:"add server key")
                  t.store (Store.Key.v []) tree in
              match return with
              | Ok () ->
                Store.Tree.find_tree tree
                @@ Store.Key.v ["keys"; origin; key_id]
              | Error write_error ->
                Dream.error (fun m ->
                    m "Write error: %a"
                      (Irmin.Type.pp Store.write_error_t)
                      write_error);
                Lwt.return_none)
            | None -> Lwt.return_none
          else Lwt.return key_tree in
        match key_tree with
        | None -> Lwt.return_false
        | Some key_tree ->
          let%lwt is_valid = is_valid_key key_tree in
          if not is_valid then Lwt.return_false
          else
            let%lwt key = Store.Tree.get key_tree (Store.Key.v ["key"]) in
            is_valid_event_signature signature event key in
      let%lwt checks = Lwt_list.map_p f signatures in
      Lwt.return @@ not @@ List.exists (Bool.equal false) checks

  (* Use older/replaced events once they are implemented *)
  let get_room_prev_events (t : Common_routes.t) room_id =
    let%lwt tree = Store.tree t.store in
    let%lwt json =
      Store.Tree.get tree @@ Store.Key.v ["rooms"; room_id; "head"] in
    let events_id =
      Ezjsonm.from_string json |> Json_encoding.(destruct (list string)) in
    let open Events in
    Lwt_list.fold_left_s
      (fun (d, ids) event_id ->
        let%lwt json = Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
        let event =
          Ezjsonm.from_string json |> Json_encoding.destruct Pdu.encoding in
        Lwt.return (max d (Pdu.get_depth event), ("$" ^ event_id) :: ids))
      (0, []) events_id

  let fetch_joined_servers (t : Common_routes.t) room_id =
    let%lwt tree = Store.tree t.store in
    let%lwt members =
      Store.Tree.list tree
      @@ Store.Key.v ["rooms"; room_id; "state"; "m.room.member"] in
    let f l (_, member_tree) =
      let%lwt event_id = Store.Tree.get member_tree @@ Store.Key.v [] in
      let%lwt json = Store.Tree.get tree @@ Store.Key.v ["events"; event_id] in
      let event =
        Json_encoding.destruct Events.State_event.encoding
          (Ezjsonm.value_from_string json) in
      match Events.State_event.get_event_content event with
      | Member member ->
        let origin =
          Events.State_event.get_room_event event
          |> Events.Room_event.get_origin
          |> Option.get in
        if
          Events.Event_content.Member.get_membership member
          = Events.Event_content.Membership.Join
          && not (List.exists (String.equal origin) l)
        then Lwt.return (origin :: l)
        else Lwt.return l
      | _ -> Lwt.return l in
    Lwt_list.fold_left_s f [] members

  (* Notes:
     - In the future we will want to have a finer approach, by not only checking
       if the server is in the room but if it's users are allow to do precise
       operations. As they are not really allowed to do anything for now however,
       this implemention will suffice.
  *)
  let is_room_participant t server_name room_id =
    let%lwt joined_servers = fetch_joined_servers t room_id in
    Lwt.return (List.exists (String.equal server_name) joined_servers)

  (* Notes:
     - Error handling
     - Use less hardcoded strings *)
  let notify_room_servers (t : Common_routes.t) room_id events =
    (* fetch the servers participating in the room *)
    let open Matrix_stos.Send in
    let%lwt origins = fetch_joined_servers t room_id in
    let origins =
      List.filter (fun s -> not @@ String.equal t.server_name s) origins in
    let f server_name =
      let body =
        Request.make ~origin:server_name ~origin_server_ts:(time ())
          ~pdus:events ()
        |> Json_encoding.construct Request.encoding in
      let content = Some body in
      let txn_id = Uuidm.(v `V4 |> to_string) in
      let path = "/_matrix/federation/v1/send/" ^ txn_id in
      let uri = Uri.make ~scheme:"https" ~port:8448 ~host:server_name ~path () in
      let headers = Cohttp.Header.init () in
      let json =
        Matrix_stos.Federation_request.make ~meth:"PUT" ~uri:path ?content
          ~origin:t.server_name ~destination:"my.domain.name" ()
        |> Json_encoding.construct Matrix_stos.Federation_request.encoding
        |> Json_encoding.sort
        |> Ezjsonm.value_to_string in
      let signature =
        Mirage_crypto_ec.Ed25519.sign ~key:t.priv_key (Cstruct.of_string json)
        |> Cstruct.to_string
        |> Base64.encode_string ~pad:false in
      let headers =
        Cohttp.Header.add headers "Authorization"
          ({|X-Matrix origin=matrix.egar.im,key="ed25519:foo_bar",sig="|}
          ^ signature
          ^ {|"|}) in
      let body = Cohttp_lwt.Body.of_string (Ezjsonm.value_to_string body) in
      let%lwt body_length, body = Cohttp_lwt.Body.length body in
      let headers =
        Cohttp.Header.add headers "Content-length" (Int64.to_string body_length)
      in
      let%lwt _resp, _body = Paf_cohttp.put ~headers ~ctx:t.ctx ~body uri in
      Lwt.return_unit in
    Lwt_list.iter_p f origins
end
