open Routing

let upload =
  meths
    [ `POST, Media_endpoints.upload
    ]

let download =
  variable
    (variable
      (node
        ~meths:
          [ `GET, Media_endpoints.download
          ]
        ~variable:
          (meths
            [ `GET, Media_endpoints.download_filename
            ])
        ()))

let thumbnail =
  variable
    (variable
      (meths
        [ `GET, Media_endpoints.thumbnail
        ]))

let preview_url =
  meths
    [ `GET, Endpoint.placeholder
    ]

let config =
  meths
    [ `GET, Media_endpoints.config
    ]

let routes =
  paths
    [ "r0"
      , paths
          [ "upload"
            , upload
          ; "download"
            , download
          ; "thumbnail"
            , thumbnail
          ; "preview_url"
            , preview_url
          ; "config"
            , config
          ]
      ]
