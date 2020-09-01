open Json_encoding

module Membership =
struct
  type t =
    | Invite
    | Join
    | Knock
    | Leave
    | Ban

  let encoding =
    string_enum
      [ ("invite", Invite)
      ; ("join", Join)
      ; ("knock", Knock)
      ; ("leave", Leave)
      ; ("ban", Ban) ]

  let to_string = function
    | Invite -> "invite"
    | Join -> "join"
    | Knock -> "knock"
    | Leave -> "leave"
    | Ban -> "ban"
end

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

module Room_event =
struct
  module Aliases =
  struct
    type t =
      { aliases: string list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.aliases
      in
      let of_tuple v =
        let aliases = v in
        {aliases}
      in
      let with_tuple =
        (obj1
          (req "aliases"
            (list string)))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Canonical_alias =
  struct
    type t =
      { alias: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.alias
      in
      let of_tuple v =
        let alias = v in
        {alias}
      in
      let with_tuple =
        (obj1
          (req "alias"
            string))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Create =
  struct
    module Previous_room =
    struct
      type t =
        { room_id: string
        ; event_id: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.room_id, t.event_id
        in
        let of_tuple v =
          let room_id, event_id = v in
          {room_id; event_id}
        in
        let with_tuple =
          (obj2
            (req "room_id"
              string)
            (req "event_id"
              string))
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { creator: string
      ; federate: bool option
      ; room_version: string option
      ; predecessor: Previous_room.t option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.creator, t.federate, t.room_version, t.predecessor
      in
      let of_tuple v =
        let creator, federate, room_version, predecessor = v in
        {creator; federate; room_version; predecessor}
      in
      let with_tuple =
        (obj4
          (req "creator"
            string)
          (opt "m.federate"
            bool)
          (opt "room_version"
            string)
          (opt "predecessor"
            Previous_room.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Join_rules =
  struct
    type rule =
      | Public
      | Knock
      | Invite
      | Private

    type t =
      { join_rule: rule
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.join_rule
      in
      let of_tuple v =
        let join_rule = v in
        {join_rule}
      in
      let with_tuple =
        let rule_encoding =
          string_enum
            [ ("public", Public)
            ; ("knock", Knock)
            ; ("invite", Invite)
            ; ("private", Private) ]
        in
        (obj1
          (req "join_rule"
            rule_encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Member =
  struct
    module Third_party_invite =
    struct
      module Signed =
      struct
        type t =
          { mxid: string
          ; signature: unit (* to do *)
          ; token: string
          } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.mxid, t.signature, t.token
        in
        let of_tuple v =
          let mxid, signature, token = v in
          {mxid; signature; token}
        in
        let with_tuple =
          (obj3
            (req "mxid"
              string)
            (req "signature"
              unit)
            (req "token"
              string))
        in
        conv to_tuple of_tuple with_tuple
      end

      type t =
        { display_name: string
        ; signed: Signed.t
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.display_name, t.signed
        in
        let of_tuple v =
          let display_name, signed = v in
          {display_name; signed}
        in
        let with_tuple =
          (obj2
            (req "display_name"
              string)
            (req "signed"
              Signed.encoding))
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { avatar_url: string option option
      ; displayname: string option option
      ; membership: Membership.t
      ; is_direct: bool option
      ; reason: string option
      ; third_party_invite: Third_party_invite.t option
      ; unsigned: Unsigned.t option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.avatar_url, t.displayname, t.membership, t.is_direct, t.reason, t.third_party_invite, t.unsigned
      in
      let of_tuple v =
        let avatar_url, displayname, membership, is_direct, reason, third_party_invite, unsigned = v in
        {avatar_url; displayname; membership; is_direct; reason; third_party_invite; unsigned}
      in
      let with_tuple =
        (obj7
          (opt "avatar_url"
            Null.string)
          (opt "displayname"
            Null.string)
          (req "membership"
            Membership.encoding)
          (opt "is_direct"
            bool)
          (opt "reason"
            string)
          (opt "third_party_invite"
            Third_party_invite.encoding)
          (opt "unsigned"
            Unsigned.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Power_levels =
  struct
    module Notifications =
    struct
      type t =
        { room: int
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.room
        in
        let of_tuple v =
          let room = v in
          {room}
        in
        let with_tuple =
          obj1
            (req "room"
              int)
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { ban: int option
      ; events: (string * int) list option
      ; events_default: int option
      ; invite: int option
      ; kick: int option
      ; redact: int option
      ; state_default: int option
      ; users: (string * int) list option
      ; users_default: int option
      ; notifications: Notifications.t option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.ban, t.events, t.events_default, t.invite, t.kick, t.redact, t.state_default, t.users, t.users_default, t.notifications
      in
      let of_tuple v =
        let ban, events, events_default, invite, kick, redact, state_default, users, users_default, notifications = v in
        { ban; events; events_default; invite; kick; redact; state_default; users; users_default; notifications }
      in
      let with_tuple =
        obj10
          (opt "ban"
            int)
          (opt "events"
            (assoc int))
          (opt "events_default"
            int)
          (opt "invite"
            int)
          (opt "kick"
            int)
          (opt "redact"
            int)
          (opt "state_default"
            int)
          (opt "users"
            (assoc int))
          (opt "users_default"
            int)
          (opt "notifications"
            Notifications.encoding)
      in
      conv to_tuple of_tuple with_tuple
  end

  module History_visibility =
  struct
    type visibility =
      | Invited
      | Joined
      | Shared
      | World_readable

    type t =
      { visibility: visibility
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.visibility
      in
      let of_tuple v =
        let visibility = v in
        { visibility }
      in
      let with_tuple =
        let rule_encoding =
          string_enum
            [ ("invited", Invited)
            ; ("joined", Joined)
            ; ("shared", Shared)
            ; ("world_readable", World_readable) ]
        in
        (obj1
          (req "history_visibility"
            rule_encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Third_party_invite =
  struct
    module Public_key =
    struct
      type t =
        { key_validity_url: string option
        ; public_key: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.key_validity_url, t.public_key
        in
        let of_tuple v =
          let key_validity_url, public_key = v in
          { key_validity_url; public_key }
        in
        let with_tuple =
          obj2
            (opt "key_validity_url" string)
            (req "public_key" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { display_name: string
      ; key_validity_url: string
      ; public_key: string
      ; public_keys: Public_key.t list option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.display_name, t.key_validity_url, t.public_key, t.public_keys
      in
      let of_tuple v =
        let display_name, key_validity_url, public_key, public_keys = v in
        { display_name; key_validity_url; public_key; public_keys }
      in
      let with_tuple =
        obj4
          (req "display_name" string)
          (req "key_validity_url" string)
          (req "public_key" string)
          (opt "public_keys" (list Public_key.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Guest_access =
  struct
    module Access =
    struct
      type t =
        | Can_join
        | Forbidden

      let encoding =
        string_enum
          [ ("can_join", Can_join)
          ; ("forbidden", Forbidden) ]
    end

    type t =
      { guest_access: Access.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.guest_access
      in
      let of_tuple v =
        let guest_access = v in
        { guest_access }
      in
      let with_tuple =
        obj1
          (req "guest_access" Access.encoding)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Server_acl =
  struct
    type t =
      { allow_ip_literals: bool option
      ; allow: string list option
      ; deny: string list option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.allow_ip_literals, t.allow, t.deny
      in
      let of_tuple v =
        let allow_ip_literals, allow, deny = v in
        { allow_ip_literals; allow; deny }
      in
      let with_tuple =
        obj3
          (opt "allow_ip_literals" bool)
          (opt "allow" (list string))
          (opt "deny" (list string))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Tombstone =
  struct
    type t =
      { body: string
      ; replacement_room: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.body, t.replacement_room
      in
      let of_tuple v =
        let body, replacement_room = v in
        { body; replacement_room }
      in
      let with_tuple =
        obj2
          (req "body" string)
          (req "replacement_room" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Encryption =
  struct
    type t =
      { algorithm: string
      ; rotation_period_ms: int option
      ; rotation_period_msgs: int option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.algorithm, t.rotation_period_ms, t.rotation_period_msgs
      in
      let of_tuple v =
        let algorithm, rotation_period_ms, rotation_period_msgs = v in
        { algorithm; rotation_period_ms; rotation_period_msgs }
      in
      let with_tuple =
        obj3
          (req "algorithm" string)
          (opt "rotation_period_ms" int)
          (opt "rotation_period_msgs" int)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Encrypted =
  struct
    module Algorithm =
    struct
      type t =
        | Curve_sha2
        | Aes_sha2

      let encoding =
        string_enum
          [ "m.olm.v1.curve25519-aes-sha2", Curve_sha2
          ; "m.megolm.v1.aes-sha2", Aes_sha2 ]
    end

    module Cyphertext =
    struct
      module Cyphertext_info =
      struct
        type t =
          { body: string
          ; msg_type: int
          } [@@deriving accessor]

        let encoding =
          let to_tuple t =
            t.body, t.msg_type
          in
          let of_tuple v =
            let body, msg_type = v in
            { body; msg_type }
          in
          let with_tuple =
            obj2
              (req "body" string)
              (req "msg_type" int)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t =
        | Megolm of string
        | Olm of (string * Cyphertext_info.t) list

      let encoding =
        union
          [ case string (function Megolm t -> Some t | _ -> None) (fun t -> Megolm t)
          ; case (assoc Cyphertext_info.encoding) (function Olm t -> Some t | _ -> None) (fun t -> Olm t) ]
    end

    type t =
      { algorithm: Algorithm.t
      ; cyphertext: Cyphertext.t
      ; sender_key: string
      ; device_id: string option
      ; session_id: string option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.algorithm, t.cyphertext, t.sender_key, t.device_id, t.session_id
      in
      let of_tuple v =
        let algorithm, cyphertext, sender_key, device_id, session_id = v in
        { algorithm; cyphertext; sender_key; device_id; session_id }
      in
      let with_tuple =
        obj5
          (req "algorithm" Algorithm.encoding)
          (req "cyphertext" Cyphertext.encoding)
          (req "sender_key" string)
          (opt "device_id" string)
          (opt "session_id" string)
      in
      conv to_tuple of_tuple with_tuple
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
    | Encrypted of Encrypted.t

  let encoding =
    let to_tuple t =
      let get_type = function
        | Aliases _ -> "m.room.aliases"
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
        | Encryption _ -> "m.room.encription"
        | Encrypted _ -> "m.room.encrypted"
      in
      get_type t, t
    in
    let of_tuple v =
      let _, t = v in t
    in
    let with_tuple =
    cond
      (obj1 (req "type" string))
      [ "m.room.aliases", case (Content.content Aliases.encoding) (function Aliases t -> Some t | _ -> None) (fun t -> Aliases t)
      ; "m.room.canonical_alias", case (Content.content Canonical_alias.encoding) (function Canonical_alias t -> Some t | _ -> None) (fun t -> Canonical_alias t)
      ; "m.room.create", case (Content.content Create.encoding) (function Create t -> Some t | _ -> None) (fun t -> Create t)
      ; "m.room.join_rules", case (Content.content Join_rules.encoding) (function Join_rules t -> Some t | _ -> None) (fun t -> Join_rules t)
      ; "m.room.member", case (Content.content Member.encoding) (function Member t -> Some t | _ -> None) (fun t -> Member t)
      ; "m.room.power_levels", case (Content.content Power_levels.encoding) (function Power_levels t -> Some t | _ -> None) (fun t -> Power_levels t)
      ; "m.room.history_visibility", case (Content.content History_visibility.encoding) (function History_visibility t -> Some t | _ -> None) (fun t -> History_visibility t)
      ; "m.room.third_party_invite", case (Content.content Third_party_invite.encoding) (function Third_party_invite t -> Some t | _ -> None) (fun t -> Third_party_invite t)
      ; "m.room.guest_access", case (Content.content Guest_access.encoding) (function Guest_access t -> Some t | _ -> None) (fun t -> Guest_access t)
      ; "m.room.server_acl", case (Content.content Server_acl.encoding) (function Server_acl t -> Some t | _ -> None) (fun t -> Server_acl t)
      ; "m.room.tombstone", case (Content.content Tombstone.encoding) (function Tombstone t -> Some t | _ -> None) (fun t -> Tombstone t)
      ; "m.room.encription", case (Content.content Encryption.encoding) (function Encryption t -> Some t | _ -> None) (fun t -> Encryption t)
      ; "m.room.encrypted", case (Content.content Encrypted.encoding) (function Encrypted t -> Some t | _ -> None) (fun t -> Encrypted t) ]
    in
      conv to_tuple of_tuple with_tuple
end

type t =
  { event: Room_event.t
  ; event_id: string
  ; sender: string
  ; origin_server_ts: int
  ; unsigned: Unsigned.t option
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
      Room_event.encoding
      (obj5
        (req "event_id" string)
        (req "sender" string)
        (req "origin_server_ts" int)
        (opt "unsigned" Unsigned.encoding)
        (opt "room_id" string))
  in
  conv to_tuple of_tuple with_tuple
