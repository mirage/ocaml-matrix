module Query =
struct
  type t =
    { redirect_url: string
    } [@@deriving accessor]

  let args t =
    ["redirect_url", [t.redirect_url]]
end

let path = "/_matrix/client/r0/login/sso"

module Response = Empty.Json

let needs_auth = false
