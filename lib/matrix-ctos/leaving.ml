open Json_encoding
open Matrix_common

module Leave = struct
  module Query = Empty.Query

  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/leave"

  module Request = Empty.Json
  module Response = Empty.Json

  let needs_auth = true
end

module Forget = struct
  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/forget"

  module Request = Empty.Json
  module Response = Empty.Json

  let needs_auth = true
end

module Kick = struct
  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/kick"

  module Request = struct
    type t = {reason: string option; user_id: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.reason, t.user_id in
      let of_tuple v =
        let reason, user_id = v in
        {reason; user_id} in
      let with_tuple = obj2 (opt "reason" string) (req "user_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end
