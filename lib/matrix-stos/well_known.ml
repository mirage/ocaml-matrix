open Json_encoding
open Matrix_common
module Query = Empty.Query

let path = "/.well-known/matrix/server"

module Response = struct
  type t = {server: string option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.server in
    let of_tuple v =
      let server = v in
      {server} in
    let with_tuple = obj1 (opt "m.server" string) in
    conv to_tuple of_tuple with_tuple

  let pp ppf t = Fmt.(pf ppf "{ server: %a }" Dump.(option string) t.server)
end
