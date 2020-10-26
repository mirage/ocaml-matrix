open Routing

let public_rooms =
  meths
    [ `GET, Federation_endpoints.public_rooms
    ]

let v1 =
  paths
    [ "publicRooms"
      , public_rooms
    ]

let matrix =
  paths
    [ "federation"
      , paths
          [ "v1"
            , v1
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
