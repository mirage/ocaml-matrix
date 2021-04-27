open Json_encoding
open Matrix_common
module Query = Empty.Query

module Request = struct
  type t = {typing: bool; timeout: int option} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.typing, t.timeout in
    let of_tuple v =
      let typing, timeout = v in
      {typing; timeout} in
    let with_tuple = obj2 (req "typing" bool) (opt "timeout" int) in
    conv to_tuple of_tuple with_tuple
end

module Response = Empty.Json
