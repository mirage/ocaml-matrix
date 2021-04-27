open Json_encoding
open Matrix_common
module Query = Empty.Query

module Request = struct
  type t = {messages: (string * (string * Repr.value) list) list option}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.messages in
    let of_tuple v =
      let messages = v in
      {messages} in
    let with_tuple = obj1 (opt "messages" (assoc (assoc any))) in
    conv to_tuple of_tuple with_tuple
end

module Response = Empty.Json
