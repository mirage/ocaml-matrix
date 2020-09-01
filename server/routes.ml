open Routing

let versions =
  node
    ~meths:
      [ `GET, Endpoint.versions
      ]
    ()

let login =
  node
    ~meths:
      [ `GET, Endpoint.login_get
      ; `POST, Endpoint.login_post
      ]
    ()

let logout =
  node
    ~meths:
      [ `POST, Endpoint.logout
      ]
    ()

let register =
  node
    ~meths:
      [ `POST, Endpoint.register
      ]
    ()

let presence =
  node
    ~variable:
      (node
        ~paths:
          [ ("status"
            , node
                ~meths:
                  [ `GET, Endpoint.presence_get
                  ; `PUT, Endpoint.presence_put
                  ]
                ())
          ]
        ())
    ()

let pushrules =
  node
    ~paths:
      [ (""
        , node
            ~meths:
              [ `GET, Endpoint.pushrules_get
              ]
            ())
      ]
    ~variable:
      (node
        ~variable:
          (node
            ~variable:
              (node
                ~meths:
                  [ `GET, Endpoint.placeholder
                  ; `DELETE, Endpoint.placeholder
                  ; `PUT, Endpoint.placeholder
                  ]
                ~paths:
                  [ ("enabled"
                    , node
                        ~meths:
                          [ `GET, Endpoint.placeholder
                          ; `PUT, Endpoint.placeholder
                          ]
                        ())
                  ; ("actions"
                    , node
                        ~meths:
                          [ `GET, Endpoint.placeholder
                          ; `PUT, Endpoint.placeholder
                          ]
                        ())
                  ]
                ())
            ())
        ())
    ()

let user =
  node
    ~variable:
      (node
        ~paths:
          [ ("filter"
            , node
                ~meths:
                  [ `POST, Endpoint.filter_post
                  ]
                ())
          ; ("account_data"
            , node
                ~variable:
                  (node
                    ~meths:
                      [ `GET, Endpoint.account_data
                      ]
                    ())
                ())
          ]
        ())
    ()

let sync =
  node
    ~meths:
      [ `GET, Endpoint.sync
      ]
    ()

let voip =
  node
    ~paths:
      [ ("turnServer"
        , node
            ~meths:
              [ `GET, Endpoint.turn_server
              ]
            ())
      ]
    ()

let profile =
  node
    ~variable:
      (node
        ~meths:
          [ `GET, Endpoint.profile_get
          ]
        ~paths:
          [ ("displayname"
            , node
                ~meths:
                  [ `GET, Endpoint.placeholder
                  ; `PUT, Endpoint.placeholder
                  ]
                ())
          ; ("avatar_url"
            , node
                ~meths:
                  [ `GET, Endpoint.placeholder
                  ; `PUT, Endpoint.placeholder
                  ]
                ())
          ]
        ())
    ()

let joined_groups =
  node
    ~meths:
      [ `GET, Endpoint.joined_groups
      ]
    ()

let keys =
  node
    ~paths:
      [ ("upload"
        , node
            ~meths:
              [ `POST, Endpoint.keys_upload
              ]
            ())
      ; ("query"
        , node
            ~meths:
              [ `POST, Endpoint.keys_query
              ]
            ())
      ; ("claim"
        , node
            ~meths:
              [ `POST, Endpoint.placeholder
              ]
            ())
      ; ("changes"
        , node
            ~meths:
              [ `POST, Endpoint.placeholder
              ]
            ())
      ]
    ()

let pushers =
  node
    ~meths:
      [ `GET, Endpoint.pushers_get
      ; `POST, Endpoint.placeholder
      ]
    ()

let create_room =
  node
    ~meths:
      [ `POST, Room_endpoint.create_room
      ]
    ()

let capabilities =
  node
    ~meths:
      [ `GET, Endpoint.capabilities
      ]
    ()

let rooms =
  node
    ~variable:
      (node
        ~paths:
          [ ("members"
            , node
                ~meths:
                  [ `GET, Room_endpoint.room_members
                  ]
                ())
          ; ("typing"
            , node
                ~variable:
                  (node
                    ~meths:
                      [ `PUT, Room_endpoint.room_typing
                      ]
                    ())
                ())
          ; ("read_markers"
            , node
                ~meths:
                  [ `POST, Room_endpoint.read_markers
                  ]
                ())
          ; ("invite"
            , node
                ~meths:
                  [ `POST, Room_endpoint.invite_to_room
                  ]
                ())
          ; ("join"
            , node
                ~meths:
                  [ `POST, Room_endpoint.join_room
                  ]
                ())
          ; ("leave"
            , node
                ~meths:
                  [ `POST, Room_endpoint.leave_room
                  ]
                ())
          ; ("forget"
            , node
                ~meths:
                  [ `POST, Room_endpoint.forget_room
                  ]
                ())
          ; ("kick"
            , node
                ~meths:
                  [ `POST, Room_endpoint.kick_room
                  ]
                ())
          ; ("ban"
            , node
                ~meths:
                  [ `POST, Room_endpoint.ban_room
                  ]
                ())
          ; ("unban"
            , node
                ~meths:
                  [ `POST, Room_endpoint.unban_room
                  ]
                ())
          ; ("initialSync"
            , node
                ~meths:
                  [ `GET, Endpoint.deprecated
                  ]
                ())
          ; ("send"
            , node
                ~variable:
                  (node
                    ~variable:
                      (node
                        ~meths:
                          [ `PUT, Room_endpoint.send_message
                          ]
                        ())
                    ())
                ())
          ]
        ())
    ()

let join =
  node
    ~variable:
      (node
        ~meths:
          [ `POST, Room_endpoint.join_room
          ]
        ())
    ()

let public_rooms =
  node
    ~meths:
      [ `POST, Room_endpoint.public_rooms
      ]
    ()

let thirdparty =
  node
    ~paths:
      [ ("protocols"
        , node
            ~meths:
              [ `GET, Endpoint.thirdparty_protocols
              ]
            ())
      ]
    ()

let directory =
  node
    ~paths:
      [ ("room"
        , node
            ~variable:
              (node
                ~meths:
                  [ `GET, Room_endpoint.get_room_alias
                  ]
                ())
            ())
      ]
    ()

let user_directory =
  node
    ~paths:
      [ ("search"
        , node
            ~meths:
              [ `POST, Endpoint.user_search
              ]
            ())
      ]
    ()

let publicised_groups =
  node
    ~meths:
      [ `POST, Endpoint.publicised_groups
      ]
    ()

let r0 =
  node
    ~paths:
      [ ("register"
        , register)
      ; ("login"
        , login)
      ; ("logout"
        , logout)
      ; ("presence"
        , presence)
      ; ("pushrules"
        , pushrules)
      ; ("user"
        , user)
      ; ("sync"
        , sync)
      ; ("voip"
        , voip)
      ; ("profile"
        , profile)
      ; ("joined_groups"
        , joined_groups)
      ; ("keys"
        , keys)
      ; ("pushers"
        , pushers)
      ; ("createRoom"
        , create_room)
      ; ("capabilities"
        , capabilities)
      ; ("rooms"
        , rooms)
      ; ("publicRooms"
        , public_rooms)
      ; ("join"
        , join)
      ; ("thirdparty"
        , thirdparty)
      ; ("directory"
        , directory)
      ; ("user_directory"
        , user_directory)
      ; ("publicised_groups"
        , publicised_groups)
      ]
    ()

let unstable =
  node
    ~paths:
      [ ("room_keys"
        , node
            ~paths:
              [ ("version"
              , node
                  ~meths:
                    [ `GET, Endpoint.placeholder
                    ]
                  ())
              ]
          ())
      ]
    ()

let matrix =
  node
    ~paths:
      [ ("client"
        , node
            ~paths:
              [ ("versions"
                , versions)
              ; ("r0"
                , r0)
              ; ("unstable" (* should not be here, not in the spec but whatever *)
                , unstable)
              ]
            ())
      ]
    ()

let well_known =
  node
    ~paths:
      [ ("matrix"
        , node
            ~paths:
              [ ("client"
                , node
                    ~meths:
                      [ `GET, Endpoint.well_known
                      ]
                    ())
              ]
            ())
      ]
    ()

let routes =
  node
    ~paths:
      [ (""
        , node
            ~paths:
              [ ("_matrix"
                , matrix)
              ; (".well-known"
                , well_known)
              ]
            ())
      ]
    ()

(*
server: [ERROR] Uri '//localhost:8000/_matrix/client/r0/directory/room/%23e:ocaml-matrix' with method 'OPTIONS': Not found
server: [ERROR] Uri '//localhost:8000/_matrix/client/r0/directory/room/%23essaysibs:ocaml-matrix' with method 'OPTIONS': Not found
*)