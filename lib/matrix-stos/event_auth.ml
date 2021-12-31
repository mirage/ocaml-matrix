open Json_encoding
open Matrix_common
module Query = Empty.Query

module Response = struct
  type t = {auth_chain: Pdu.t list} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.auth_chain in
    let of_tuple v =
      let auth_chain = v in
      {auth_chain} in
    let with_tuple = obj1 (req "auth_chain" (list Pdu.encoding)) in
    conv to_tuple of_tuple with_tuple
end
