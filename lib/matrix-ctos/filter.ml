open Json_encoding
open Matrix_common

module Event_filter = struct
  type t = {
      limit: int option
    ; not_senders: string list option
    ; not_types: string list option
    ; senders: string list option
    ; types: string list option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.limit, t.not_senders, t.not_types, t.senders, t.types in
    let of_tuple v =
      let limit, not_senders, not_types, senders, types = v in
      {limit; not_senders; not_types; senders; types} in
    let with_tuple =
      obj5 (opt "limit" int)
        (opt "not_senders" (list string))
        (opt "not_types" (list string))
        (opt "senders" (list string))
        (opt "types" (list string)) in
    conv to_tuple of_tuple with_tuple
end

module State_filter = struct
  type t = {
      limit: int option
    ; not_senders: string list option
    ; not_types: string list option
    ; senders: string list option
    ; types: string list option
    ; lazy_load_members: bool option
    ; include_redundant_members: bool option
    ; not_rooms: string list option
    ; rooms: string list option
    ; contains_url: bool option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.limit
      , t.not_senders
      , t.not_types
      , t.senders
      , t.types
      , t.lazy_load_members
      , t.include_redundant_members
      , t.not_rooms
      , t.rooms
      , t.contains_url ) in
    let of_tuple v =
      let ( limit
          , not_senders
          , not_types
          , senders
          , types
          , lazy_load_members
          , include_redundant_members
          , not_rooms
          , rooms
          , contains_url ) =
        v in
      {
        limit
      ; not_senders
      ; not_types
      ; senders
      ; types
      ; lazy_load_members
      ; include_redundant_members
      ; not_rooms
      ; rooms
      ; contains_url
      } in
    let with_tuple =
      obj10 (opt "limit" int)
        (opt "not_senders" (list string))
        (opt "not_types" (list string))
        (opt "senders" (list string))
        (opt "types" (list string))
        (opt "lazy_load_members" bool)
        (opt "include_redundant_members" bool)
        (opt "not_rooms" (list string))
        (opt "rooms" (list string))
        (opt "contains_url" bool) in
    conv to_tuple of_tuple with_tuple
end

module Room_event_filter = State_filter

module Room_filter = struct
  type t = {
      not_rooms: string list option
    ; rooms: string list option
    ; ephemeral: Room_event_filter.t option
    ; include_leave: bool option
    ; state: State_filter.t option
    ; timeline: Room_event_filter.t option
    ; account_data: Room_event_filter.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.not_rooms
      , t.rooms
      , t.ephemeral
      , t.include_leave
      , t.state
      , t.timeline
      , t.account_data ) in
    let of_tuple v =
      let ( not_rooms
          , rooms
          , ephemeral
          , include_leave
          , state
          , timeline
          , account_data ) =
        v in
      {
        not_rooms
      ; rooms
      ; ephemeral
      ; include_leave
      ; state
      ; timeline
      ; account_data
      } in
    let with_tuple =
      obj7
        (opt "not_rooms" (list string))
        (opt "rooms" (list string))
        (opt "ephemeral" Room_event_filter.encoding)
        (opt "include_leave" bool)
        (opt "state" State_filter.encoding)
        (opt "timeline" Room_event_filter.encoding)
        (opt "account_data" Room_event_filter.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Filter = struct
  module Event_format = struct
    type t = Client | Federation

    let encoding = string_enum ["client", Client; "federation", Federation]
  end

  type t = {
      event_fields: string list option
    ; event_format: Event_format.t option
    ; presence: Event_filter.t option
    ; account_data: Event_filter.t option
    ; room: Room_filter.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.event_fields, t.event_format, t.presence, t.account_data, t.room in
    let of_tuple v =
      let event_fields, event_format, presence, account_data, room = v in
      {event_fields; event_format; presence; account_data; room} in
    let with_tuple =
      obj5
        (opt "event_fields" (list string))
        (opt "event_format" Event_format.encoding)
        (opt "presence" Event_filter.encoding)
        (opt "account_data" Event_filter.encoding)
        (opt "room" Room_filter.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Post = struct
  module Query = Empty.Query

  let path user_id = "/_matrix/client/r0/user/" ^ user_id ^ "/filter"

  module Request = Filter

  module Response = struct
    type t = {filter_id: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.filter_id in
      let of_tuple v =
        let filter_id = v in
        {filter_id} in
      let with_tuple = obj1 (req "filter_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Get = struct
  module Query = Empty.Query

  let path user_id filter_id =
    "/_matrix/client/r0/user/" ^ user_id ^ "/filter/" ^ filter_id

  module Response = Filter

  let needs_auth = true
end
