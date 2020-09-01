open Matrix_ctos
open Json_encoding
open Store
open Lwt.Infix
open Helper

let placeholder = placeholder

let deprecated = deprecated

let error = error

let versions =
  let open Versions in
  let f () _ _ _ =
    let response =
      Response.make
        ~versions:Const.versions
        ?unstable_features:Const.unstable_features
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response)
  in
  needs_auth, f

let login_get =
  let open Login.Get in
  let f () _ _ _ =
    let response =
      Response.make
        ?types:(Some ["m.login.password"])
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response)
  in
  needs_auth, f

let login_post =
  let open Login.Post in
  let f () request _ _ =
    let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
    let auth = Request.get_auth request in
    match auth with
    | Password (V2 auth) ->
      let id = Authentication.Password.V2.get_identifier auth in
      let password = Authentication.Password.V2.get_password auth in
      (match id with
       | User user ->
         let username = Identifier.User.get_user user in
         Store.exists store Key.(v "users" / username) >>=
         (function
           | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
           | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "")
           | Ok (Some _) ->
             Store.get store Key.(v "users" / username / "password") >>=
             (function
               | Ok pass ->
                 create_token
                   (fun token ->
                      if pass = password then
                        let response =
                          Response.make
                            ?user_id:(Some (username_to_user_id username))
                            ?access_token:token
                            ?home_server:(Some Const.homeserver)
                            ?device_id:(Some "device_id")
                            ?well_known:None
                            ()
                        in
                        let response =
                          construct Response.encoding response |>
                          Ezjsonm.value_to_string
                        in
                        Lwt.return (`OK, response)
                      else
                        Lwt.return (`Forbidden, error "M_FORBIDDEN" ""))
                   username
               | _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")))
       | _ ->
         (`Bad_request, error "M_UNKNOWN" "Bad identifier type") |> Lwt.return)
    | _ ->
      (`Bad_request, error "M_UNKNOWN" "Bad request type") |> Lwt.return
  in
  needs_auth, f

let logout =
  let open Logout.Logout in
  let f () _ _ token =
    match token with
    | None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "")
    | Some token ->
      Store.remove store Key.(v "tokens" / token) >>=
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
  in
  needs_auth, f

let register =
  let open Register.Register in
  let f () request query _ =
    let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
    let kind = List.assoc_opt "kind" query |> (Option.map List.hd) in
    match kind with
    | Some "guest" ->
      let username = (Option.value (Request.get_username request) ~default:"default") in
      let f token =
        let response =
          Response.make
            ~user_id:(username_to_user_id username)
            ?access_token:token
            ~home_server:Const.homeserver
            ?device_id:(Some "device_id")
            ()
        in
        let response =
          construct Response.encoding response|>
          Ezjsonm.value_to_string
        in
        Lwt.return (`OK, response)
      in
      (match Request.get_inhibit_login request with
       | Some true -> f None
       | _ -> create_token f username)
    | _ ->
      match Request.get_password request with
      | None ->
        (`Unauthorized, {|{"session": "MSoDYilSIFilglrBUmISKWQe", "flows": [{"stages": ["m.login.dummy"]}], "params": {}}|}) |> Lwt.return
      | Some password ->
        let username = Option.value (Request.get_username request) ~default:"default_user" in
        Store.exists store Key.(v "users" / username) >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok (Some _) -> Lwt.return (`Bad_request, error "M_USER_IN_USE" "Desired user ID is already taken.")
          | Ok None ->
            Store.set store Key.(v "users" / username / "password") password >>=
            (function
              | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
              | Ok () ->
                let f token =
                  let response =
                    Response.make
                      ~user_id:(username_to_user_id username)
                      ?access_token:token
                      ~home_server:Const.homeserver
                      ?device_id:(Some "device_id")
                      ()
                  in
                  let response =
                    construct Response.encoding response|>
                    Ezjsonm.value_to_string
                  in
                  Lwt.return (`OK, response)
                in
                (match Request.get_inhibit_login request with
                 | Some true -> f None
                 | _ -> create_token f username)))
  in
  needs_auth, f

let presence_put =
  let open Presence.Post in
  let f ((), _user_id) request _ _ =
    let _request = destruct Request.encoding (Ezjsonm.value_from_string request) in
    let response =
      Response.make ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let presence_get =
  let open Presence.Get in
  let f ((), _user_id) _ _ _ =
    let response =
      Response.make
        ~presence:Event.Presence.Presence.Online
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let pushrules_get =
  let open Push_rules.Get_all in
  let f () _ _ _ =
    let response =
      Response.make ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let filter_post =
  let open Filter.Post in
  let f ((), _) request _ _ =
    let _request = destruct Request.encoding (Ezjsonm.value_from_string request) in
    let response =
      Response.make
        ~filter_id:"filter"
        ()
    in
    let response =
      construct Response.encoding response|>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let sync =
  let open Sync in
  let f () _ query token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let user_id = username_to_user_id username in
        let timeout =
          let timeout = List.assoc_opt "timeout" query |> (Option.map List.hd) in
          Option.value ~default:"0" timeout |> float_of_string
        in
        Lwt_unix.sleep (timeout /. 1000. /. 5.) >>=
        (fun () ->
          Room_endpoint.get_rooms user_id >>=
          (function
            | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
            | Ok (joined_rooms, invited_rooms, leaved_rooms) ->
              let push_rules =
                Event.Push_rules
                  (Event.Push_rules.make
                    ~content:[]
                    ~override:[]
                    ~room:[]
                    ~sender:[]
                    ~underride:[]
                    ())
              in
              let response =
                Response.make
                  ~next_batch:"now"
                  ?rooms:
                  (Some
                    (Rooms.make
                      ~join: joined_rooms
                      ~invite:invited_rooms
                      ~leave:leaved_rooms
                      ()))
                  ?account_data:(Some [push_rules])
                  ()
              in
              let response =
                construct Response.encoding response |>
                Ezjsonm.value_to_string
              in
              Lwt.return (`OK, response))))
  in
  needs_auth, f

let turn_server =
  let open Voip in
  let f () _ _ _ =
    let response =
      Response.make ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let account_data =
  let open Account_data.Get in
  let f (((), _user_id), _type) _ _ _ =
    let response =
      Response.make
        ~data:[]
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let profile_get =
  let open Profile.Get in
  let f ((), user_id) _ _ _ =
    let response =
      Response.make
        ?displayname:(Some user_id)
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let joined_groups =
  let f () _ _ _ =
    (`OK, {|{"groups": []}|}) |> Lwt.return
  in
  true, f

let publicised_groups =
  let f () _ _ _ =
    (`OK, {|{"users": {}}|}) |> Lwt.return
  in
  true, f

let well_known =
  let open Well_known in
  let f () _ _ _ =
    let response =
      Response.make
        ~homeserver:Const.homeserver
        ?identity_server:(Some Const.identity_server)
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    (`OK, response) |> Lwt.return
  in
  needs_auth, f

let keys_upload =
  let open Keys.Upload in
  let f () request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some username) ->
        let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let f (algorithm, key) =
          let key = construct Request.Keys_format.encoding key |> Ezjsonm.value_to_string in
          Store.set store Key.(v "users" / username / "one_time_keys" / "device_id" / algorithm) key >>=
          (function
            | Error _ -> Lwt.return_error (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
            | Ok () -> Lwt.return_ok ())
        in
        ignore (Option.map (List.map f) (Request.get_one_time_keys request));
        Store.list store Key.(v "users" / username / "one_time_keys" / "device_id") >>=
        (function
          | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
          | Ok _l -> (* temporary solution, riot is going crazy with this endpoint *)
            let response =
              Response.make
                ~one_time_key_counts:["signed_curve25519", 50 (* List.length l *)]
                ()
            in
            let response =
              construct Response.encoding response |>
              Ezjsonm.value_to_string
            in
            Lwt.return (`OK, response)))
  in
  needs_auth, f

let keys_query =
  let open Keys.Query in
  let f () request _ token =
    get_logged_username token >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok None -> Lwt.return (`Forbidden, error "M_FORBIDDEN" "") (* should not happend *)
      | Ok (Some _username) ->
        let _request = destruct Request.encoding (Ezjsonm.value_from_string request) in
        let response =
          Response.make
            ?failures:(Some [])
            ?device_keys:(Some [])
            ()
        in
        let response =
          construct Response.encoding response |>
          Ezjsonm.value_to_string
        in
        Lwt.return (`OK, response))
  in
  needs_auth, f

let pushers_get =
  let open Pushers.Get in
  let f () _ _ _ =
    let response =
      Response.make
        ?pushers:(Some [])
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response)
  in
  needs_auth, f

let capabilities =
  let open Capabilities in
  let f () _ _ _ =
    let response =
      Response.make
        ~capabilities:[]
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response)
  in
  needs_auth, f

let thirdparty_protocols =
  let open Third_party_network.Protocols in
  let f () _ _ _ =
    let response =
      Response.make
        ~protocols:[]
        ()
    in
    let response =
      construct Response.encoding response |>
      Ezjsonm.value_to_string
    in
    Lwt.return (`OK, response)
  in
  needs_auth, f

let user_search =
  let open User_directory.Search in
  let f () request _ _ =
    let request = destruct Request.encoding (Ezjsonm.value_from_string request) in
    let search = Request.get_search_term request in
    Store.list store Key.(v "users") >>=
    (function
      | Error _ -> Lwt.return (`Internal_server_error, error "M_UNKNOWN" "Internal storage failure")
      | Ok user_ids ->
        let users =
          List.filter_map
            (fun (username, _) ->
              if Astring.String.is_prefix ~affix:search username
              then
                Some
                  (Response.User.make
                    ~user_id:(username_to_user_id username)
                    ())
              else
                None) user_ids
        in
        let response =
          Response.make
            ~results:users
            ~limited:false
            ()
        in
        let response =
          construct Response.encoding response |>
          Ezjsonm.value_to_string
        in
        Lwt.return (`OK, response))
  in
  needs_auth, f
