open Json_encoding

module Query = Empty.Query

let path = "/.well-known/matrix/client"

module Response =
struct
  type t =
    { homeserver: string
    ; identity_server: string option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      (t.homeserver, t.identity_server)
    in
    let of_tuple v =
      let (homeserver, identity_server) = v in
      {homeserver; identity_server}
    in
    let with_tuple =
      obj2
        (req "m.homeserver"
          (obj1
            (req "base_url"
              string)))
        (opt "m.identity_server"
          (obj1
            (req "base_url"
              string)))
    in
    conv to_tuple of_tuple with_tuple

  let pp ppf t =
    Fmt.(pf ppf "{ homeserver: %s ; identity_server: %a }"
      t.homeserver
      Dump.(option string) t.identity_server)
end

let needs_auth = false
