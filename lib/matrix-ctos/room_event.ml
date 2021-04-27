open Json_encoding
open Matrix_common

module Get = struct
  module Event = struct
    module Query = Empty.Query
    module Request = Empty.Json
    module Response = Events.Room_event
  end

  module State_key = struct
    module Query = Empty.Query
    module Request = Empty.Json

    module Response = struct
      type t = Repr.value

      let encoding = any
    end
  end

  module State = struct
    module Query = Empty.Query
    module Request = Empty.Json

    module Response = struct
      type t = {events: Events.State_event.t list} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.events in
        let of_tuple v =
          let events = v in
          {events} in
        let with_tuple = list Events.State_event.encoding in
        conv to_tuple of_tuple with_tuple
    end
  end

  module Members = struct
    module Query = struct
      type t = {
          at: string option
        ; membership: Events.Event_content.Membership.t option
        ; not_membership: Events.Event_content.Membership.t option
      }
      [@@deriving accessor]

      let args t =
        let l = [] in
        let l = match t.at with None -> l | Some at -> ("at", [at]) :: l in
        let l =
          match t.membership with
          | None -> l
          | Some membership ->
            ( "membership"
            , [Events.Event_content.Membership.to_string membership] )
            :: l in
        match t.not_membership with
        | None -> l
        | Some not_membership ->
          ( "not_membership"
          , [Events.Event_content.Membership.to_string not_membership] )
          :: l
    end

    module Request = Empty.Json

    module Response = struct
      type t = {chunk: Events.State_event.t list} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.chunk in
        let of_tuple v =
          let chunk = v in
          {chunk} in
        let with_tuple = obj1 (req "chunk" (list Events.State_event.encoding)) in
        conv to_tuple of_tuple with_tuple
    end
  end

  module Joined_members = struct
    module Query = Empty.Query
    module Request = Empty.Json

    module Response = struct
      module User = struct
        type t = {display_name: string option; avatar_url: string option}
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.display_name, t.avatar_url in
          let of_tuple v =
            let display_name, avatar_url = v in
            {display_name; avatar_url} in
          let with_tuple =
            obj2 (opt "display_name" string) (opt "avatar_url" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {joined: (string * User.t) list option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.joined in
        let of_tuple v =
          let joined = v in
          {joined} in
        let with_tuple = obj1 (opt "joined" (assoc User.encoding)) in
        conv to_tuple of_tuple with_tuple
    end
  end
end

module Put = struct
  module State_event = struct
    module Query = Empty.Query

    module Request = struct
      type t = {event: Events.Event_content.t} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.event in
        let of_tuple v =
          let event = v in
          {event} in
        let with_tuple = Events.Event_content.encoding in
        conv to_tuple of_tuple with_tuple
    end

    module Response = struct
      type t = {event_id: string} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.event_id in
        let of_tuple v =
          let event_id = v in
          {event_id} in
        let with_tuple = obj1 (req "event_id" string) in
        conv to_tuple of_tuple with_tuple
    end
  end

  module Message_event = struct
    module Query = Empty.Query

    module Request = struct
      type t = {event: Events.Event_content.Message.t} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.event in
        let of_tuple v =
          let event = v in
          {event} in
        let with_tuple = Events.Event_content.Message.encoding in
        conv to_tuple of_tuple with_tuple
    end

    module Response = struct
      type t = {event_id: string} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.event_id in
        let of_tuple v =
          let event_id = v in
          {event_id} in
        let with_tuple = obj1 (req "event_id" string) in
        conv to_tuple of_tuple with_tuple
    end
  end
end
