open Json_encoding

module Query = Empty.Query

let path = "/_matrix/client/r0/joined_rooms"

module Response =
struct
  type t =
    { joined: string list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.joined
    in
    let of_tuple v =
      let joined = v in
      {joined}
    in
    let with_tuple =
      obj1
        (req "joined_rooms" (list string))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
