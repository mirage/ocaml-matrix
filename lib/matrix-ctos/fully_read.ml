open Json_encoding
open Matrix_common
module Query = Empty.Query

let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/read_markers"

module Request = struct
  type t = {
      fully_read: string option
          (* supposed to be required by the documentation but apparently can be null with Element *)
    ; read: string option
    ; hidden: bool option (* not in the documentation *)
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.fully_read, t.read, t.hidden in
    let of_tuple v =
      let fully_read, read, hidden = v in
      {fully_read; read; hidden} in
    let with_tuple =
      obj3
        (req "m.fully_read" Null.string)
        (opt "m.read" string) (opt "m.hidden" bool) in
    conv to_tuple of_tuple with_tuple
end

module Response = Empty.Json

let needs_auth = true
