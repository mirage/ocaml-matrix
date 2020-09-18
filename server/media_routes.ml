open Routing

let upload =
  node
    ~meths:
      [ `POST, Media_endpoints.upload
      ]
    ()

let download =
  node
    ~variable:
      (node
        ~variable:
          (node
            ~meths:
              [ `GET, Media_endpoints.download
              ]
            ~variable:
              (node
                ~meths:
                  [ `GET, Media_endpoints.download_filename
                  ]
                ())
            ())
        ())
    ()

let thumbnail =
  node
    ~variable:
      (node
        ~variable:
          (node
            ~meths:
              [ `GET, Media_endpoints.thumbnail
              ]
            ())
        ())
    ()

let preview_url =
  node
    ~meths:
      [ `GET, Endpoint.placeholder
      ]
    ()

let config =
  node
    ~meths:
      [ `GET, Media_endpoints.config
      ]
    ()

let routes =
  node
    ~paths:
      [ ("r0"
        , node
            ~paths:
              [ ("upload"
                , upload)
              ; ("download"
                , download)
              ; ("thumbnail"
                , thumbnail)
              ; ("preview_url"
                , preview_url)
              ; ("config"
                , config)
              ]
            ())
      ]
    ()
