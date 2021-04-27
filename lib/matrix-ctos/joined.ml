open Json_encoding
open Matrix_common
module Query = Empty.Query

module Response = struct
  type t = {joined: string list} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.joined in
    let of_tuple v =
      let joined = v in
      {joined} in
    let with_tuple = obj1 (req "joined_rooms" (list string)) in
    conv to_tuple of_tuple with_tuple
end
