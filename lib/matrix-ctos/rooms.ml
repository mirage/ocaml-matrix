open Json_encoding
open Matrix_common

module Room_summary = struct
  type t = {
      heroes: string list option
    ; joined_member_count: int option
    ; invited_member_count: int option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.heroes, t.joined_member_count, t.invited_member_count in
    let of_tuple v =
      let heroes, joined_member_count, invited_member_count = v in
      {heroes; joined_member_count; invited_member_count} in
    let with_tuple =
      obj3
        (opt "m.heroes" (list string))
        (opt "m.joined_member_count" int)
        (opt "m.invited_member_count" int) in
    conv to_tuple of_tuple with_tuple
end

module Timeline = struct
  type t = {
      events: Events.Room_event.t list option
    ; limited: bool option
    ; prev_batch: string option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.events, t.limited, t.prev_batch in
    let of_tuple v =
      let events, limited, prev_batch = v in
      {events; limited; prev_batch} in
    let with_tuple =
      obj3
        (opt "events" (list Events.Room_event.encoding))
        (opt "limited" bool) (opt "prev_batch" string) in
    conv to_tuple of_tuple with_tuple
end

module Joined_room = struct
  module Unread_notifications = struct
    type t = {highlight_count: int option; notification_count: int option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.highlight_count, t.notification_count in
      let of_tuple v =
        let highlight_count, notification_count = v in
        {highlight_count; notification_count} in
      let with_tuple =
        obj2 (opt "highlight_count" int) (opt "notification_count" int) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
      summary: Room_summary.t option
    ; state: Events.State_event.t list option
    ; timeline: Timeline.t option
    ; ephemeral: Events.Event.t list option
    ; account_data: Events.Event.t list option
    ; unread_notifications: Unread_notifications.t option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.summary
      , t.state
      , t.timeline
      , t.ephemeral
      , t.account_data
      , t.unread_notifications ) in
    let of_tuple v =
      let ( summary
          , state
          , timeline
          , ephemeral
          , account_data
          , unread_notifications ) =
        v in
      {summary; state; timeline; ephemeral; account_data; unread_notifications}
    in
    let with_tuple =
      obj6
        (opt "summary" Room_summary.encoding)
        (opt "state" (obj1 (req "events" (list Events.State_event.encoding))))
        (opt "timeline" Timeline.encoding)
        (opt "ephemeral" (obj1 (req "events" (list Events.Event.encoding))))
        (opt "account_data" (obj1 (req "events" (list Events.Event.encoding))))
        (opt "unread_notifications" Unread_notifications.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Invited_room = struct
  type t = {invite_state: Events.State_event.t list option}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.invite_state in
    let of_tuple v =
      let invite_state = v in
      {invite_state} in
    let with_tuple =
      obj1
        (opt "invite_state"
           (obj1 (req "events" (list Events.State_event.encoding)))) in
    conv to_tuple of_tuple with_tuple
end

module Left_room = struct
  type t = {
      state: Events.State_event.t list option
    ; timeline: Timeline.t option
    ; account_data: Events.Room_event.t list option
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.state, t.timeline, t.account_data in
    let of_tuple v =
      let state, timeline, account_data = v in
      {state; timeline; account_data} in
    let with_tuple =
      obj3
        (opt "state" (obj1 (req "events" (list Events.State_event.encoding))))
        (opt "timeline" Timeline.encoding)
        (opt "account_data"
           (obj1 (req "events" (list Events.Room_event.encoding)))) in
    conv to_tuple of_tuple with_tuple
end

type t = {
    join: (string * Joined_room.t) list
  ; invite: (string * Invited_room.t) list
  ; leave: (string * Left_room.t) list
}
[@@deriving accessor]

let encoding =
  let to_tuple t = t.join, t.invite, t.leave in
  let of_tuple v =
    let join, invite, leave = v in
    {join; invite; leave} in
  let with_tuple =
    obj3
      (req "join" (assoc Joined_room.encoding))
      (req "invite" (assoc Invited_room.encoding))
      (req "leave" (assoc Left_room.encoding)) in
  conv to_tuple of_tuple with_tuple
