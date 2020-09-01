open Json_encoding

module Get =
struct
  module Event =
  struct
    module Query = Empty.Query

    let path room_id event_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/event/" ^ event_id

    module Request = Empty.Json

    module Response = Room_events

    let needs_auth = true
  end

  module State_key =
  struct
    module Query = Empty.Query

    let path room_id event_type state_key = "_matrix/client/r0/rooms/" ^ room_id ^ "/state/" ^ event_type ^ state_key

    module Request = Empty.Json

    module Response =
    struct
      type t = Repr.value

      let encoding = any
    end
  end

  module State =
  struct
    module Query = Empty.Query

    let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/state"

    module Request = Empty.Json

    module Response =
    struct
      type t =
        { events: State_events.t list
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.events
        in
        let of_tuple v =
          let events = v in
          { events }
        in
        let with_tuple =
          list State_events.encoding
        in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end

  module Members =
  struct
    module Query =
    struct
      type t =
        { at: string option
        ; membership: Room_events.Membership.t option
        ; not_membership: Room_events.Membership.t option
        } [@@deriving accessor]

      let args t =
        let l = [] in
        let l =
          match t.at with
            | None -> l
            | Some at -> ("at", [at])::l
        in
        let l =
          match t.membership with
            | None -> l
            | Some membership -> ("membership", [Room_events.Membership.to_string membership])::l
        in
        match t.not_membership with
          | None -> l
          | Some not_membership -> ("not_membership", [Room_events.Membership.to_string not_membership])::l
    end

    let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/members"

    module Request = Empty.Json

    module Response =
    struct
      type t =
        { chunk: State_events.t list
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.chunk
        in
        let of_tuple v =
          let chunk = v in
          { chunk }
        in
        let with_tuple =
          obj1
            (req "chunk" (list State_events.encoding))
        in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end

  module Joined_members =
  struct
    module Query = Empty.Query

    let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/joined_members"

    module Request = Empty.Json

    module Response =
    struct
      module User =
      struct
        type t =
          { display_name: string option
          ; avatar_url: string option
          } [@@deriving accessor]

        let encoding =
          let to_tuple t =
            t.display_name, t.avatar_url
          in
          let of_tuple v =
            let display_name, avatar_url = v in
            { display_name; avatar_url }
          in
          let with_tuple =
            obj2
              (opt "display_name" string)
              (opt "avatar_url" string)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t =
        { joined: (string * User.t) list option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.joined
        in
        let of_tuple v =
          let joined = v in
          { joined }
        in
        let with_tuple =
          obj1
            (opt "joined" (assoc User.encoding))
        in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end
end

module Put =
struct
  module State_event =
  struct
    module Query = Empty.Query

    let path room_id event_type state_key = "_matrix/client/r0/rooms/" ^ room_id ^ "/state/" ^ event_type ^ "/" ^ state_key

    module Request =
    struct
      type t =
        { event: Room_events.Room_event.t
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.event
        in
        let of_tuple v =
          let event = v in
          { event }
        in
        let with_tuple =
          Room_events.Room_event.encoding
        in
        conv to_tuple of_tuple with_tuple
    end

    module Response =
    struct
      type t =
        { event_id: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.event_id
        in
        let of_tuple v =
          let event_id = v in
          { event_id }
        in
        let with_tuple =
          obj1
            (req "event_id" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end

  module Message_event =
  struct
    module Query = Empty.Query

    let path room_id event_type txn_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/send/" ^ event_type ^ "/" ^ txn_id

    module Request =
    struct
      type t =
        { event: Message_event.Message_event.t
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.event
        in
        let of_tuple v =
          let event = v in
          { event }
        in
        let with_tuple =
          Message_event.Message_event.encoding
        in
        conv to_tuple of_tuple with_tuple
    end

    module Response =
    struct
      type t =
        { event_id: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.event_id
        in
        let of_tuple v =
          let event_id = v in
          { event_id }
        in
        let with_tuple =
          obj1
            (req "event_id" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    let needs_auth = true
  end
end
