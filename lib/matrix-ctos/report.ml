open Json_encoding
open Matrix_common
module Query = Empty.Query

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
