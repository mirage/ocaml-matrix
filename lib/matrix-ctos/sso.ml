open Matrix_common

module Query = struct
  type t = {redirect_url: string} [@@deriving accessor]

  let args t = ["redirect_url", [t.redirect_url]]
end

module Response = Empty.Json
