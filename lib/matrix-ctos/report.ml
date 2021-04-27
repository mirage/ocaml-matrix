open Json_encoding
open Matrix_common
module Query = Empty.Query

let path room_id event_id =
  "_matrix/client/r0/rooms/" ^ room_id ^ "/report/" ^ event_id

module Request = struct
  type t = {score: int; reason: string} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.score, t.reason in
    let of_tuple v =
      let score, reason = v in
      {score; reason} in
    let with_tuple = obj2 (req "score" int) (req "reason" string) in
    conv to_tuple of_tuple with_tuple
end

module Response = Empty.Json

let needs_auth = true
