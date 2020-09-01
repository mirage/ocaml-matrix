open Json_encoding

module V1 =
struct
  module Query = Empty.Query

  let path room_id event_id = "/_matrix/federation/v1/invite/" ^ room_id ^ "/" ^ event_id

  module Request =
  struct
    module Unsigned =
    struct
      type t =
        { whatever: Repr.value
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.whatever
        in
        let of_tuple v =
          let whatever = v in
          {whatever}
        in
        let with_tuple =
          any
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { sender: string
      ; origin: string
      ; origin_server_ts: int
      ; event_type: string
      ; state_key: string
      ; unsigned: Unsigned.t option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.sender, t.origin, t.origin_server_ts, t.event_type, t.state_key, (), t.unsigned
      in
      let of_tuple v =
        let sender, origin, origin_server_ts, event_type, state_key, (), unsigned = v in
        { sender; origin; origin_server_ts; event_type; state_key; unsigned }
      in
      let with_tuple =
        obj7
          (req "sender" string)
          (req "origin" string)
          (req "origin_server_ts" int)
          (req "event_type" string)
          (req "state_key" string)
          (req "content" (obj1
            (req "membership" (constant "invite"))))
          (opt "unsigned" Unsigned.encoding)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response =
  struct
    
  end
end

module V2 =
struct
  
end
