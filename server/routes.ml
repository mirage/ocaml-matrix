open Routing

let versions = meths [`GET, Endpoint.versions]
let login = meths [`GET, Endpoint.login_get; `POST, Endpoint.login_post]
let logout = meths [`POST, Endpoint.logout]
let register = meths [`POST, Endpoint.register]

let presence =
  variable
    (paths
       [
         ( "status"
         , meths [`GET, Endpoint.presence_get; `PUT, Endpoint.presence_put] )
       ])

let pushrules =
  node
    ~paths:["", meths [`GET, Endpoint.pushrules_get]]
    ~variable:
      (variable
         (variable
            (node
               ~meths:
                 [
                   `GET, Endpoint.placeholder; `DELETE, Endpoint.placeholder
                 ; `PUT, Endpoint.placeholder
                 ]
               ~paths:
                 [
                   ( "enabled"
                   , meths
                       [`GET, Endpoint.placeholder; `PUT, Endpoint.placeholder]
                   )
                 ; ( "actions"
                   , meths
                       [`GET, Endpoint.placeholder; `PUT, Endpoint.placeholder]
                   )
                 ]
               ())))
    ()

let user =
  variable
    (paths
       [
         ( "filter"
         , node
             ~variable:(meths [`GET, Endpoint.filter_get])
             ~meths:[`POST, Endpoint.filter_post]
             () )
       ; ( "account_data"
         , variable
             (meths
                [
                  `GET, Endpoint.Account_data.get
                ; `PUT, Endpoint.Account_data.put
                ]) )
       ])

let sync = meths [`GET, Endpoint.sync]
let voip = paths ["turnServer", meths [`GET, Endpoint.turn_server]]

let profile =
  variable
    (node
       ~meths:[`GET, Endpoint.Profile.get]
       ~paths:
         [
           ( "displayname"
           , meths
               [
                 `GET, Endpoint.Profile.Displayname.get
               ; `PUT, Endpoint.Profile.Displayname.put
               ] )
         ; ( "avatar_url"
           , meths
               [
                 `GET, Endpoint.Profile.Avatar_url.get
               ; `PUT, Endpoint.Profile.Avatar_url.put
               ] )
         ]
       ())

let joined_groups = meths [`GET, Endpoint.joined_groups]

let keys =
  paths
    [
      "upload", meths [`POST, Endpoint.keys_upload]
    ; "query", meths [`POST, Endpoint.keys_query]
    ; "claim", meths [`POST, Endpoint.placeholder]
    ; "changes", meths [`POST, Endpoint.placeholder]
    ]

let pushers = meths [`GET, Endpoint.pushers_get; `POST, Endpoint.placeholder]
let create_room = meths [`POST, Room_endpoint.create_room]
let capabilities = meths [`GET, Endpoint.capabilities]

let rooms =
  variable
    (paths
       [
         "members", meths [`GET, Room_endpoint.Events.Get.members]
       ; "typing", variable (meths [`PUT, Room_endpoint.room_typing])
       ; "read_markers", meths [`POST, Room_endpoint.read_markers]
       ; "invite", meths [`POST, Room_endpoint.Membership.invite]
       ; "join", meths [`POST, Room_endpoint.Membership.join]
       ; "leave", meths [`POST, Room_endpoint.Membership.leave]
       ; "forget", meths [`POST, Room_endpoint.Membership.forget]
       ; "kick", meths [`POST, Room_endpoint.Membership.kick]
       ; "ban", meths [`POST, Room_endpoint.Membership.ban]
       ; "unban", meths [`POST, Room_endpoint.Membership.unban]
       ; "initialSync", meths [`GET, Endpoint.deprecated]
       ; ( "send"
         , variable
             (variable (meths [`PUT, Room_endpoint.Events.Put.send_message])) )
       ; ( "state"
         , variable
             (variable
                (meths
                   [
                     `GET, Room_endpoint.Events.Get.state
                   ; `PUT, Room_endpoint.Events.Put.state
                   ])) )
       ])

let join = variable (meths [`POST, Room_endpoint.Membership.join])
let public_rooms = meths [`POST, Room_endpoint.Listing.public_rooms]

let thirdparty =
  paths ["protocols", meths [`GET, Endpoint.thirdparty_protocols]]

let directory =
  paths
    [
      ( "room"
      , variable
          (meths
             [
               `PUT, Room_endpoint.Room_alias.put
             ; `GET, Room_endpoint.Room_alias.get
             ; `DELETE, Room_endpoint.Room_alias.delete
             ]) )
    ; ( "list"
      , paths
          [
            ( "room"
            , variable
                (meths
                   [
                     `GET, Room_endpoint.Listing.get_visibility
                   ; `PUT, Room_endpoint.Listing.set_visibility
                   ]) )
          ] )
    ]

let user_directory = paths ["search", meths [`POST, Endpoint.user_search]]
let publicised_groups = meths [`POST, Endpoint.publicised_groups]
let account = paths ["3pid", meths [`GET, Endpoint.Account.Thirdparty_pid.get]]

let r0 =
  paths
    [
      "register", register; "login", login; "logout", logout
    ; "presence", presence; "pushrules", pushrules; "user", user; "sync", sync
    ; "voip", voip; "profile", profile; "joined_groups", joined_groups
    ; "keys", keys; "pushers", pushers; "createRoom", create_room
    ; "capabilities", capabilities; "rooms", rooms; "publicRooms", public_rooms
    ; "join", join; "thirdparty", thirdparty; "directory", directory
    ; "user_directory", user_directory; "publicised_groups", publicised_groups
    ; "account", account
    ]

let unstable =
  paths ["room_keys", paths ["version", meths [`GET, Endpoint.placeholder]]]

let matrix =
  paths
    [
      ( "client"
      , paths
          [
            "versions", versions; "r0", r0
          ; ( "unstable" (* should not be here, not in the spec but whatever *)
            , unstable )
          ] ); "media", Media_routes.routes
    ]

let well_known =
  paths ["matrix", paths ["client", meths [`GET, Endpoint.well_known]]]

let routes : unit t =
  paths ["", paths ["_matrix", matrix; ".well-known", well_known]]
