open Json_encoding
open Matrix_common

module Query = struct
  type t = {from: string option; limit: int option; only: bool option}
  [@@deriving accessor]

  let args t =
    let l = [] in
    let l = match t.from with None -> l | Some from -> ("from", [from]) :: l in
    let l =
      match t.limit with
      | None -> l
      | Some limit ->
        let limit = Int.to_string limit in
        ("limit", [limit]) :: l in
    let l =
      match t.only with
      | None -> l
      | Some only ->
        let only = Bool.to_string only in
        ("only", [only]) :: l in
    l
end

module Response = struct
  module Notification = struct
    type t = {
      actions: Push_rule.Action.t;
      event: Events.Room_event.t;
      profile_tag: string option;
      read: bool;
      room_id: string;
      ts: int;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.actions, t.event, t.profile_tag, t.read, t.room_id, t.ts in
      let of_tuple v =
        let actions, event, profile_tag, read, room_id, ts = v in
        {actions; event; profile_tag; read; room_id; ts} in
      let with_tuple =
        obj6
          (req "actions" Push_rule.Action.encoding)
          (req "event" Events.Room_event.encoding)
          (opt "profile_tag" string) (req "read" bool) (req "room_id" string)
          (req "ts" int) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
    next_token: string option option;
    notifications: Notification.t list;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.next_token, t.notifications in
    let of_tuple v =
      let next_token, notifications = v in
      {next_token; notifications} in
    let with_tuple =
      obj2
        (opt "next_token" Null.string)
        (req "notifications" (list Notification.encoding)) in
    conv to_tuple of_tuple with_tuple
end
