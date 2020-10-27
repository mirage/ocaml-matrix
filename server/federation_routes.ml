open Routing

(* /make_join/!1bb1ff19-a1c5-4796-a19a-5435a5e2d2d5:ocaml-matrix/@gwen:my.domain.name *)

let public_rooms =
  meths
    [ `GET, Federation_endpoints.Listing.Public_rooms.get
    ; `POST, Federation_endpoints.Listing.Public_rooms.post
    ]

let make_join =
  variable
    (variable
      (meths
        [ `GET, Federation_endpoints.Join.get
        ]))

let send_join_v1 =
  variable
    (variable
      (meths
        [ `PUT, Federation_endpoints.Join.put_v1
        ]))

let send_join_v2 =
  variable
    (variable
      (meths
        [ `PUT, Federation_endpoints.Join.put_v2
        ]))

let v1 =
  paths
    [ "publicRooms"
      , public_rooms
    ; "make_join"
      , make_join
    ; "send_join"
      , send_join_v1
    ]

let v2 =
  paths
    [ "send_join"
      , send_join_v2
    ]

let matrix =
  paths
    [ "federation"
      , paths
          [ "v1"
            , v1
          ; "v2"
            , v2
          ]
    ]

let routes: unit t =
  paths
    [ ""
      , paths
          [ "_matrix"
            , matrix
          ]
    ]
