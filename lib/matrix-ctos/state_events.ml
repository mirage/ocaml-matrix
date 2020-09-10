open Json_encoding

module State_event =
struct
  module Aliases =
  struct
    type t =
      { homeserver: string
      ; event: Room_events.Room_event.Aliases.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.homeserver, t.event
      in
      let of_tuple v =
        let homeserver, event = v in
        {homeserver; event}
      in
      let with_tuple =
        obj2
          (req "state_key" string)
          (req "content" Room_events.Room_event.Aliases.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key e = e.homeserver
  end

  module Canonical_alias =
  struct
    type t =
      { event: Room_events.Room_event.Canonical_alias.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Canonical_alias.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Create =
  struct
    type t =
      { event: Room_events.Room_event.Create.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Create.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Join_rules =
  struct
    type t =
      { event: Room_events.Room_event.Join_rules.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Join_rules.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Member =
  struct
    type t =
      { user_id: string
      ; event: Room_events.Room_event.Member.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.user_id, t.event
      in
      let of_tuple v =
        let user_id, event = v in
        {user_id; event}
      in
      let with_tuple =
        obj2
          (req "state_key" string)
          (req "content" Room_events.Room_event.Member.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key e = e.user_id
  end

  module Power_levels =
  struct
    type t =
      { event: Room_events.Room_event.Power_levels.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Power_levels.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module History_visibility =
  struct
    type t =
      { event: Room_events.Room_event.History_visibility.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.History_visibility.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Third_party_invite =
  struct
    type t =
      { to_sign: string
      ; event: Room_events.Room_event.Third_party_invite.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.to_sign, t.event
      in
      let of_tuple v =
        let to_sign, event = v in
        {to_sign; event}
      in
      let with_tuple =
        obj2
          (req "state_key" string)
          (req "content" Room_events.Room_event.Third_party_invite.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key e = e.to_sign
  end

  module Guest_access =
  struct
    type t =
      { event: Room_events.Room_event.Guest_access.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Guest_access.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Server_acl =
  struct
    type t =
      { event: Room_events.Room_event.Server_acl.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Server_acl.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Tombstone =
  struct
    type t =
      { event: Room_events.Room_event.Tombstone.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Tombstone.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Encryption =
  struct
    type t =
      { event: Room_events.Room_event.Encryption.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Room_events.Room_event.Encryption.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Name =
  struct
    type t =
      { event: Message_event.Message_event.Name.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Message_event.Message_event.Name.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Topic =
  struct
    type t =
      { event: Message_event.Message_event.Topic.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Message_event.Message_event.Topic.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Avatar =
  struct
    type t =
      { event: Message_event.Message_event.Avatar.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Message_event.Message_event.Avatar.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  module Pinned_events =
  struct
    type t =
      { event: Message_event.Message_event.Pinned_events.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (), t.event
      in
      let of_tuple v =
        let (), event = v in
        {event}
      in
      let with_tuple =
        obj2
          (req "state_key" (constant ""))
          (req "content" Message_event.Message_event.Pinned_events.encoding)
      in
      conv to_tuple of_tuple with_tuple

    let get_state_key _ = ""
  end

  type t =
    | Aliases of Aliases.t
    | Canonical_alias of Canonical_alias.t
    | Create of Create.t
    | Join_rules of Join_rules.t
    | Member of Member.t
    | Power_levels of Power_levels.t
    | History_visibility of History_visibility.t
    | Third_party_invite of Third_party_invite.t
    | Guest_access of Guest_access.t
    | Server_acl of Server_acl.t
    | Tombstone of Tombstone.t
    | Encryption of Encryption.t
    | Name of Name.t
    | Topic of Topic.t
    | Avatar of Avatar.t
    | Pinned_events of Pinned_events.t

  let encoding =
    let to_tuple t =
      let get_type = function
        | Aliases _ -> "m.room.aliases" (* Historical event, should be ignored *)
        | Canonical_alias _ -> "m.room.canonical_alias"
        | Create _ -> "m.room.create"
        | Join_rules _ -> "m.room.join_rules"
        | Member _ -> "m.room.member"
        | Power_levels _ -> "m.room.power_levels"
        | History_visibility _ -> "m.room.history_visibility"
        | Third_party_invite _ -> "m.room.third_party_invite"
        | Guest_access _ -> "m.room.guest_access"
        | Server_acl _ -> "m.room.server_acl"
        | Tombstone _ -> "m.room.tombstone"
        | Encryption _ -> "m.room.encryption"
        | Name _ -> "m.room.name"
        | Topic _ -> "m.room.topic"
        | Avatar _ -> "m.room.avatar"
        | Pinned_events _ -> "m.room.pinned_events"
      in
      get_type t, t
    in
    let of_tuple v =
      let _, t = v in t
    in
    let with_tuple =
    cond
      (obj1 (req "type" string))
      [ "m.room.aliases", case Aliases.encoding (function Aliases t -> Some t | _ -> None) (fun t -> Aliases t)
      ; "m.room.canonical_alias", case Canonical_alias.encoding (function Canonical_alias t -> Some t | _ -> None) (fun t -> Canonical_alias t)
      ; "m.room.create", case Create.encoding (function Create t -> Some t | _ -> None) (fun t -> Create t)
      ; "m.room.join_rules", case Join_rules.encoding (function Join_rules t -> Some t | _ -> None) (fun t -> Join_rules t)
      ; "m.room.member", case Member.encoding (function Member t -> Some t | _ -> None) (fun t -> Member t)
      ; "m.room.power_levels", case Power_levels.encoding (function Power_levels t -> Some t | _ -> None) (fun t -> Power_levels t)
      ; "m.room.history_visibility", case History_visibility.encoding (function History_visibility t -> Some t | _ -> None) (fun t -> History_visibility t)
      ; "m.room.third_party_invite", case Third_party_invite.encoding (function Third_party_invite t -> Some t | _ -> None) (fun t -> Third_party_invite t)
      ; "m.room.guest_access", case Guest_access.encoding (function Guest_access t -> Some t | _ -> None) (fun t -> Guest_access  t)
      ; "m.room.server_acl", case Server_acl.encoding (function Server_acl t -> Some t | _ -> None) (fun t -> Server_acl  t)
      ; "m.room.tombstone", case Tombstone.encoding (function Tombstone t -> Some t | _ -> None) (fun t -> Tombstone  t)
      ; "m.room.encryption", case Encryption.encoding (function Encryption t -> Some t | _ -> None) (fun t -> Encryption t)
      ; "m.room.name", case Name.encoding (function Name t -> Some t | _ -> None) (fun t -> Name t)
      ; "m.room.topic", case Topic.encoding (function Topic t -> Some t | _ -> None) (fun t -> Topic t)
      ; "m.room.avatar", case Avatar.encoding (function Avatar t -> Some t | _ -> None) (fun t -> Avatar t)
      ; "m.room.pinned_events", case Pinned_events.encoding (function Pinned_events t -> Some t | _ -> None) (fun t -> Pinned_events t)]
    in
      conv to_tuple of_tuple with_tuple

  let get_state_key = function
    | Aliases e -> Aliases.get_state_key e
    | Canonical_alias e -> Canonical_alias.get_state_key e
    | Create e -> Create.get_state_key e
    | Join_rules e -> Join_rules.get_state_key e
    | Member e -> Member.get_state_key e
    | Power_levels e -> Power_levels.get_state_key e
    | History_visibility e -> History_visibility.get_state_key e
    | Third_party_invite e -> Third_party_invite.get_state_key e
    | Guest_access e -> Guest_access.get_state_key e
    | Server_acl e -> Server_acl.get_state_key e
    | Tombstone e -> Tombstone.get_state_key e
    | Encryption e -> Encryption.get_state_key e
    | Name e -> Name.get_state_key e
    | Topic e -> Topic.get_state_key e
    | Avatar e -> Avatar.get_state_key e
    | Pinned_events e -> Pinned_events.get_state_key e
end

type t =
  { event: State_event.t
  ; event_id: string option (* Only optionnal when coming from client *)
  ; sender: string option (* Only optionnal when coming from client *)
  ; origin_server_ts: int option (* Only optionnal when coming from client *)
  ; unsigned: Room_events.Unsigned.t option
  ; room_id: string option (* Is required everywhere except when syncing *)
  } [@@deriving accessor]

let encoding =
  let to_tuple t =
    t.event, (t.event_id, t.sender, t.origin_server_ts, t.unsigned, t.room_id)
  in
  let of_tuple v =
    let event, (event_id, sender, origin_server_ts, unsigned, room_id) = v in
    {event; event_id; sender; origin_server_ts; unsigned; room_id}
  in
  let with_tuple =
    merge_objs
      State_event.encoding
      (obj5
        (opt "event_id" string)
        (opt "sender" string)
        (opt "origin_server_ts" int)
        (opt "unsigned" Room_events.Unsigned.encoding)
        (opt "room_id" string))
  in
  conv to_tuple of_tuple with_tuple

let get_state_key e = State_event.get_state_key e.event
