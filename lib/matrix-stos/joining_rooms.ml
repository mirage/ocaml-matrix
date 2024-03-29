open Json_encoding
open Matrix_common

module Make_join = struct
  module Query = struct
    type t = {ver: string list option} [@@deriving accessor]

    let args t = match t.ver with None -> [] | Some ver -> ["ver", ver]
  end

  module Response = struct
    type t = {room_version: string option; event_template: Pdu.t option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.room_version, t.event_template in
      let of_tuple v =
        let room_version, event_template = v in
        {room_version; event_template} in
      let with_tuple =
        obj2 (opt "room_version" string) (opt "event" Pdu.encoding) in
      conv to_tuple of_tuple with_tuple
  end
end

module Send_join = (* à revoir avec un room_event peut-être *)
struct
  module V1 = struct
    module Query = Empty.Query

    module Request = struct
      type t = {
        sender: string;
        origin: string;
        origin_server_ts: int;
        event_type: string;
        state_key: string;
        content: Events.Event_content.Member.t;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          ( t.sender,
            t.origin,
            t.origin_server_ts,
            t.event_type,
            t.state_key,
            t.content ) in
        let of_tuple v =
          let sender, origin, origin_server_ts, event_type, state_key, content =
            v in
          {sender; origin; origin_server_ts; event_type; state_key; content}
        in
        let with_tuple =
          obj6 (req "sender" string) (req "origin" string)
            (req "origin_server_ts" int)
            (req "type" string) (req "state_key" string)
            (req "content" Events.Event_content.Member.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Response = struct
      module Room_state = struct
        type t = {
          origin: string;
          auth_chain: Events.Pdu.t list;
          state: Events.Pdu.t list;
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.origin, t.auth_chain, t.state in
          let of_tuple v =
            let origin, auth_chain, state = v in
            {origin; auth_chain; state} in
          let with_tuple =
            obj3 (req "origin" string)
              (req "auth_chain" (list Events.Pdu.encoding))
              (req "state" (list Events.Pdu.encoding)) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {
        (* not sure about what the spec wants, needs experimentation to differentiate from v1 *)
        room_state: Room_state.t option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.room_state in
        let of_tuple v =
          let room_state = v in
          {room_state} in
        let with_tuple = obj1 (opt "room_state" Room_state.encoding) in
        conv to_tuple of_tuple with_tuple
    end
  end

  module V2 = struct
    module Query = Empty.Query
    module Request = Matrix_common.Events.Pdu

    module Response = struct
      type t = {
        origin: string;
        auth_chain: Events.Pdu.t list;
        state: Events.Pdu.t list;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.origin, t.auth_chain, t.state in
        let of_tuple v =
          let origin, auth_chain, state = v in
          {origin; auth_chain; state} in
        let with_tuple =
          obj3 (req "origin" string)
            (req "auth_chain" (list Events.Pdu.encoding))
            (req "state" (list Events.Pdu.encoding)) in
        conv to_tuple of_tuple with_tuple
    end
  end
end
