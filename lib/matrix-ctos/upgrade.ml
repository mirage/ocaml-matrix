open Json_encoding
open Matrix_common

module Query = Empty.Query

let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/upgrade"

module Request =
struct
  type t =
    { new_version: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.new_version
    in
    let of_tuple v =
      let new_version = v in
      { new_version }
    in
    let with_tuple =
      obj1
        (req "new_version" string)
    in
    conv to_tuple of_tuple with_tuple
end

module Response =
struct
  type t =
    { replacement_room: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.replacement_room
    in
    let of_tuple v =
      let replacement_room = v in
      { replacement_room }
    in
    let with_tuple =
      obj1
        (req "replacement_room" string)
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
