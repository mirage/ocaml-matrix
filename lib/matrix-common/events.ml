open Json_encoding

module Event_content = struct
  module Membership = struct
    type t = Invite | Join | Knock | Leave | Ban

    let encoding =
      string_enum
        [
          "invite", Invite; "join", Join; "knock", Knock; "leave", Leave;
          "ban", Ban;
        ]

    let to_string = function
      | Invite -> "invite"
      | Join -> "join"
      | Knock -> "knock"
      | Leave -> "leave"
      | Ban -> "ban"
  end

  module Aliases = struct
    type t = {aliases: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.aliases in
      let of_tuple v =
        let aliases = v in
        {aliases} in
      let with_tuple = obj1 (req "aliases" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Canonical_alias = struct
    type t = {alias: string option option; alt_aliases: string list option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.alias, t.alt_aliases in
      let of_tuple v =
        let alias, alt_aliases = v in
        {alias; alt_aliases} in
      let with_tuple =
        obj2 (opt "alias" Null.string) (opt "alt_aliases" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Create = struct
    module Previous_room = struct
      type t = {room_id: string; event_id: string} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.room_id, t.event_id in
        let of_tuple v =
          let room_id, event_id = v in
          {room_id; event_id} in
        let with_tuple = obj2 (req "room_id" string) (req "event_id" string) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      creator: string;
      federate: bool option;
      room_version: string option;
      predecessor: Previous_room.t option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.creator, t.federate, t.room_version, t.predecessor in
      let of_tuple v =
        let creator, federate, room_version, predecessor = v in
        {creator; federate; room_version; predecessor} in
      let with_tuple =
        obj4 (req "creator" string) (opt "m.federate" bool)
          (opt "room_version" string)
          (opt "predecessor" Previous_room.encoding) in
      conv to_tuple of_tuple with_tuple

    let redact =
      let to_tuple t = t.creator in
      let of_tuple _ = assert false in
      let with_tuple = obj1 (req "creator" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Join_rules = struct
    type rule = Public | Knock | Invite | Private
    type t = {join_rule: rule} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.join_rule in
      let of_tuple v =
        let join_rule = v in
        {join_rule} in
      let with_tuple =
        let rule_encoding =
          string_enum
            [
              "public", Public; "knock", Knock; "invite", Invite;
              "private", Private;
            ] in
        obj1 (req "join_rule" rule_encoding) in
      conv to_tuple of_tuple with_tuple

    let redact = encoding
  end

  module Member = struct
    module Third_party_invite = struct
      module Signed = struct
        type t = {mxid: string; signature: unit; (* to do *) token: string}
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.mxid, t.signature, t.token in
          let of_tuple v =
            let mxid, signature, token = v in
            {mxid; signature; token} in
          let with_tuple =
            obj3 (req "mxid" string) (req "signature" unit) (req "token" string)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t = {display_name: string; signed: Signed.t} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.display_name, t.signed in
        let of_tuple v =
          let display_name, signed = v in
          {display_name; signed} in
        let with_tuple =
          obj2 (req "display_name" string) (req "signed" Signed.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      avatar_url: string option option;
      displayname: string option option;
      membership: Membership.t;
      is_direct: bool option;
      reason: string option;
      third_party_invite: Third_party_invite.t option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.avatar_url,
          t.displayname,
          t.membership,
          t.is_direct,
          t.reason,
          t.third_party_invite ) in
      let of_tuple v =
        let ( avatar_url,
              displayname,
              membership,
              is_direct,
              reason,
              third_party_invite ) =
          v in
        {
          avatar_url;
          displayname;
          membership;
          is_direct;
          reason;
          third_party_invite;
        } in
      let with_tuple =
        obj6
          (opt "avatar_url" Null.string)
          (opt "displayname" Null.string)
          (req "membership" Membership.encoding)
          (opt "is_direct" bool) (opt "reason" string)
          (opt "third_party_invite" Third_party_invite.encoding) in
      conv to_tuple of_tuple with_tuple

    let redact =
      let to_tuple t = t.membership in
      let of_tuple _ = assert false in
      let with_tuple = obj1 (req "membership" Membership.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  module Power_levels = struct
    module Notifications = struct
      type t = {room: int} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.room in
        let of_tuple v =
          let room = v in
          {room} in
        let with_tuple = obj1 (req "room" int) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      ban: int option;
      events: (string * int) list option;
      events_default: int option;
      invite: int option;
      kick: int option;
      redact: int option;
      state_default: int option;
      users: (string * int) list option;
      users_default: int option;
      notifications: Notifications.t option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.ban,
          t.events,
          t.events_default,
          t.invite,
          t.kick,
          t.redact,
          t.state_default,
          t.users,
          t.users_default,
          t.notifications ) in
      let of_tuple v =
        let ( ban,
              events,
              events_default,
              invite,
              kick,
              redact,
              state_default,
              users,
              users_default,
              notifications ) =
          v in
        {
          ban;
          events;
          events_default;
          invite;
          kick;
          redact;
          state_default;
          users;
          users_default;
          notifications;
        } in
      let with_tuple =
        obj10 (opt "ban" int)
          (opt "events" (assoc int))
          (opt "events_default" int) (opt "invite" int) (opt "kick" int)
          (opt "redact" int) (opt "state_default" int)
          (opt "users" (assoc int))
          (opt "users_default" int)
          (opt "notifications" Notifications.encoding) in
      conv to_tuple of_tuple with_tuple

    let redact =
      let to_tuple t =
        ( t.ban,
          t.events,
          t.events_default,
          t.kick,
          t.redact,
          t.state_default,
          t.users,
          t.users_default ) in
      let of_tuple _ = assert false in
      let with_tuple =
        obj8 (opt "ban" int)
          (opt "events" (assoc int))
          (opt "events_default" int) (opt "kick" int) (opt "redact" int)
          (opt "state_default" int)
          (opt "users" (assoc int))
          (opt "users_default" int) in
      conv to_tuple of_tuple with_tuple
  end

  module History_visibility = struct
    type visibility = Invited | Joined | Shared | World_readable
    type t = {visibility: visibility} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.visibility in
      let of_tuple v =
        let visibility = v in
        {visibility} in
      let with_tuple =
        let rule_encoding =
          string_enum
            [
              "invited", Invited; "joined", Joined; "shared", Shared;
              "world_readable", World_readable;
            ] in
        obj1 (req "history_visibility" rule_encoding) in
      conv to_tuple of_tuple with_tuple

    let redact = encoding
  end

  module Third_party_invite = struct
    module Public_key = struct
      type t = {key_validity_url: string option; public_key: string}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.key_validity_url, t.public_key in
        let of_tuple v =
          let key_validity_url, public_key = v in
          {key_validity_url; public_key} in
        let with_tuple =
          obj2 (opt "key_validity_url" string) (req "public_key" string) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      display_name: string;
      key_validity_url: string;
      public_key: string;
      public_keys: Public_key.t list option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.display_name, t.key_validity_url, t.public_key, t.public_keys in
      let of_tuple v =
        let display_name, key_validity_url, public_key, public_keys = v in
        {display_name; key_validity_url; public_key; public_keys} in
      let with_tuple =
        obj4
          (req "display_name" string)
          (req "key_validity_url" string)
          (req "public_key" string)
          (opt "public_keys" (list Public_key.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  module Guest_access = struct
    module Access = struct
      type t = Can_join | Forbidden

      let encoding = string_enum ["can_join", Can_join; "forbidden", Forbidden]
    end

    type t = {guest_access: Access.t} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.guest_access in
      let of_tuple v =
        let guest_access = v in
        {guest_access} in
      let with_tuple = obj1 (req "guest_access" Access.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  module Server_acl = struct
    type t = {
      allow_ip_literals: bool option;
      allow: string list option;
      deny: string list option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.allow_ip_literals, t.allow, t.deny in
      let of_tuple v =
        let allow_ip_literals, allow, deny = v in
        {allow_ip_literals; allow; deny} in
      let with_tuple =
        obj3
          (opt "allow_ip_literals" bool)
          (opt "allow" (list string))
          (opt "deny" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Tombstone = struct
    type t = {body: string; replacement_room: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.body, t.replacement_room in
      let of_tuple v =
        let body, replacement_room = v in
        {body; replacement_room} in
      let with_tuple =
        obj2 (req "body" string) (req "replacement_room" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Encryption = struct
    type t = {
      algorithm: string;
      rotation_period_ms: int option;
      rotation_period_msgs: int option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.algorithm, t.rotation_period_ms, t.rotation_period_msgs in
      let of_tuple v =
        let algorithm, rotation_period_ms, rotation_period_msgs = v in
        {algorithm; rotation_period_ms; rotation_period_msgs} in
      let with_tuple =
        obj3 (req "algorithm" string)
          (opt "rotation_period_ms" int)
          (opt "rotation_period_msgs" int) in
      conv to_tuple of_tuple with_tuple
  end

  module Encrypted = struct
    module Algorithm = struct
      type t = Curve_sha2 | Aes_sha2

      let encoding =
        string_enum
          [
            "m.olm.v1.curve25519-aes-sha2", Curve_sha2;
            "m.megolm.v1.aes-sha2", Aes_sha2;
          ]
    end

    module Cyphertext = struct
      module Cyphertext_info = struct
        type t = {body: string; msg_type: int} [@@deriving accessor]

        let encoding =
          let to_tuple t = t.body, t.msg_type in
          let of_tuple v =
            let body, msg_type = v in
            {body; msg_type} in
          let with_tuple = obj2 (req "body" string) (req "msg_type" int) in
          conv to_tuple of_tuple with_tuple
      end

      type t = Megolm of string | Olm of (string * Cyphertext_info.t) list

      let encoding =
        union
          [
            case string
              (function Megolm t -> Some t | _ -> None)
              (fun t -> Megolm t);
            case
              (assoc Cyphertext_info.encoding)
              (function Olm t -> Some t | _ -> None)
              (fun t -> Olm t);
          ]
    end

    type t = {
      algorithm: Algorithm.t;
      cyphertext: Cyphertext.t;
      sender_key: string;
      device_id: string option;
      session_id: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.algorithm, t.cyphertext, t.sender_key, t.device_id, t.session_id in
      let of_tuple v =
        let algorithm, cyphertext, sender_key, device_id, session_id = v in
        {algorithm; cyphertext; sender_key; device_id; session_id} in
      let with_tuple =
        obj5
          (req "algorithm" Algorithm.encoding)
          (req "cyphertext" Cyphertext.encoding)
          (req "sender_key" string) (opt "device_id" string)
          (opt "session_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Message = struct
    module Text = struct
      type t = {
        body: string;
        format: string option;
        formatted_body: string option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.format, t.formatted_body in
        let of_tuple v =
          let body, format, formatted_body = v in
          {body; format; formatted_body} in
        let with_tuple =
          obj3 (req "body" string) (opt "format" string)
            (opt "formatted_body" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Emote = struct
      type t = {
        body: string;
        format: string option;
        formatted_body: string option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.format, t.formatted_body in
        let of_tuple v =
          let body, format, formatted_body = v in
          {body; format; formatted_body} in
        let with_tuple =
          obj3 (req "body" string) (opt "format" string)
            (opt "formatted_body" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Notice = struct
      type t = {body: string} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body in
        let of_tuple v =
          let body = v in
          {body} in
        let with_tuple = obj1 (req "body" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Image = struct
      module Info = struct
        type t = {
          h: int option;
          w: int option;
          mimetype: string option;
          size: int option;
          thumbnail_url: string option;
          thumbnail_file: Message.Encrypted_file.t option;
          thumbnail_info: Message.Thumbnail.t option;
        }
        [@@deriving accessor]

        let encoding =
          let to_tuple t =
            ( t.h,
              t.w,
              t.mimetype,
              t.size,
              t.thumbnail_url,
              t.thumbnail_file,
              t.thumbnail_info ) in
          let of_tuple v =
            let ( h,
                  w,
                  mimetype,
                  size,
                  thumbnail_url,
                  thumbnail_file,
                  thumbnail_info ) =
              v in
            {
              h;
              w;
              mimetype;
              size;
              thumbnail_url;
              thumbnail_file;
              thumbnail_info;
            } in
          let with_tuple =
            obj7 (opt "h" int) (opt "w" int) (opt "mimetype" string)
              (opt "size" int)
              (opt "thumbnail_url" string)
              (opt "thumbnail_file" Message.Encrypted_file.encoding)
              (opt "thumbnail_info" Message.Thumbnail.encoding) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {
        body: string;
        info: Message.Image.t option;
        url: string;
        file: Message.Encrypted_file.t option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.info, t.url, t.file in
        let of_tuple v =
          let body, info, url, file = v in
          {body; info; url; file} in
        let with_tuple =
          obj4 (req "body" string)
            (opt "info" Message.Image.encoding)
            (req "url" string)
            (opt "file" Message.Encrypted_file.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module File = struct
      type t = {
        body: string;
        filename: string option;
        info: Message.File.t option;
        url: string;
        file: Message.Encrypted_file.t option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.filename, t.info, t.url, t.file in
        let of_tuple v =
          let body, filename, info, url, file = v in
          {body; filename; info; url; file} in
        let with_tuple =
          obj5 (req "body" string) (opt "filename" string)
            (opt "info" Message.File.encoding)
            (req "url" string)
            (opt "file" Message.Encrypted_file.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Audio = struct
      type t = {
        body: string;
        info: Message.Audio.t option;
        url: string;
        file: Message.Encrypted_file.t option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.info, t.url, t.file in
        let of_tuple v =
          let body, info, url, file = v in
          {body; info; url; file} in
        let with_tuple =
          obj4 (req "body" string)
            (opt "info" Message.Audio.encoding)
            (req "url" string)
            (opt "file" Message.Encrypted_file.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Location = struct
      type t = {body: string; info: Message.Location.t option; geo_uri: string}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.info, t.geo_uri in
        let of_tuple v =
          let body, info, geo_uri = v in
          {body; info; geo_uri} in
        let with_tuple =
          obj3 (req "body" string)
            (opt "info" Message.Location.encoding)
            (req "geo_uri" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Video = struct
      type t = {
        body: string;
        info: Message.Video.t option;
        url: string;
        file: Message.Encrypted_file.t option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.info, t.url, t.file in
        let of_tuple v =
          let body, info, url, file = v in
          {body; info; url; file} in
        let with_tuple =
          obj4 (req "body" string)
            (opt "info" Message.Video.encoding)
            (req "url" string)
            (opt "file" Message.Encrypted_file.encoding) in
        conv to_tuple of_tuple with_tuple
    end

    module Sticker =
    (* Might not be at the good place: Should be taken out of message *)
    struct
      type t = {body: string; info: Message.Image.t; url: string}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.body, t.info, t.url in
        let of_tuple v =
          let body, info, url = v in
          {body; info; url} in
        let with_tuple =
          obj3 (req "body" string)
            (req "info" Message.Image.encoding)
            (req "url" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Server_notice = struct
      type t = {
        body: string;
        server_notice_type: string;
        admin_contact: string option;
        limit_type: string option;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.server_notice_type, t.admin_contact, t.limit_type in
        let of_tuple v =
          let body, server_notice_type, admin_contact, limit_type = v in
          {body; server_notice_type; admin_contact; limit_type} in
        let with_tuple =
          obj4 (req "body" string)
            (req "server_notice_type" string)
            (opt "admin_contact" string)
            (opt "limit_type" string) in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      | Text of Text.t
      | Emote of Emote.t
      | Notice of Notice.t
      | Image of Image.t
      | File of File.t
      | Audio of Audio.t
      | Location of Location.t
      | Video of Video.t
      | Sticker of Sticker.t
      | Server_notice of Server_notice.t

    let encoding =
      let to_tuple t =
        let get_type = function
          | Text _ -> "m.text"
          | Emote _ -> "m.emote"
          | Notice _ -> "m.notice"
          | Image _ -> "m.image"
          | File _ -> "m.file"
          | Audio _ -> "m.audio"
          | Location _ -> "m.location"
          | Video _ -> "m.video"
          | Sticker _ -> "m.sticker"
          | Server_notice _ -> "m.server_notice" in
        get_type t, t in
      let of_tuple v =
        let _, t = v in
        t in
      let with_tuple =
        cond
          (obj1 (req "msgtype" string))
          [
            ( "m.text",
              case Text.encoding
                (function Text t -> Some t | _ -> None)
                (fun t -> Text t) );
            ( "m.emote",
              case Emote.encoding
                (function Emote t -> Some t | _ -> None)
                (fun t -> Emote t) );
            ( "m.notice",
              case Notice.encoding
                (function Notice t -> Some t | _ -> None)
                (fun t -> Notice t) );
            ( "m.image",
              case Image.encoding
                (function Image t -> Some t | _ -> None)
                (fun t -> Image t) );
            ( "m.file",
              case File.encoding
                (function File t -> Some t | _ -> None)
                (fun t -> File t) );
            ( "m.audio",
              case Audio.encoding
                (function Audio t -> Some t | _ -> None)
                (fun t -> Audio t) );
            ( "m.location",
              case Location.encoding
                (function Location t -> Some t | _ -> None)
                (fun t -> Location t) );
            ( "m.video",
              case Video.encoding
                (function Video t -> Some t | _ -> None)
                (fun t -> Video t) );
            ( "m.sticker",
              case Sticker.encoding
                (function Sticker t -> Some t | _ -> None)
                (fun t -> Sticker t) );
            ( "m.server_notice",
              case Server_notice.encoding
                (function Server_notice t -> Some t | _ -> None)
                (fun t -> Server_notice t) );
          ] in
      conv to_tuple of_tuple with_tuple
  end

  module Name = struct
    type t = {name: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.name in
      let of_tuple v =
        let name = v in
        {name} in
      let with_tuple = obj1 (req "name" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Topic = struct
    type t = {topic: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.topic in
      let of_tuple v =
        let topic = v in
        {topic} in
      let with_tuple = obj1 (req "topic" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Avatar = struct
    type t = {info: Message.Image.t option; url: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.info, t.url in
      let of_tuple v =
        let info, url = v in
        {info; url} in
      let with_tuple =
        obj2 (opt "info" Message.Image.encoding) (req "url" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Pinned_events = struct
    type t = {pinned: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.pinned in
      let of_tuple v =
        let pinned = v in
        {pinned} in
      let with_tuple = obj1 (req "pinned" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Call = struct
    module Invite = struct
      module Offer = struct
        type t = {sdp: string} [@@deriving accessor]

        let encoding =
          let to_tuple t = (), t.sdp in
          let of_tuple v =
            let (), sdp = v in
            {sdp} in
          let with_tuple =
            obj2 (req "type" (constant "offer")) (req "sdp" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {call_id: string; offer: Offer.t; version: int; lifetime: int}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.call_id, t.offer, t.version, t.lifetime in
        let of_tuple v =
          let call_id, offer, version, lifetime = v in
          {call_id; offer; version; lifetime} in
        let with_tuple =
          obj4 (req "call_id" string)
            (req "offer" Offer.encoding)
            (req "version" int) (req "lifetime" int) in
        conv to_tuple of_tuple with_tuple
    end

    module Candidates = struct
      module Candidate = struct
        type t = {sdpMid: string; sdpMLineIndex: int; candidate: string}
        [@@deriving accessor]

        let encoding =
          let to_tuple t = t.sdpMid, t.sdpMLineIndex, t.candidate in
          let of_tuple v =
            let sdpMid, sdpMLineIndex, candidate = v in
            {sdpMid; sdpMLineIndex; candidate} in
          let with_tuple =
            obj3 (req "sdpMid" string) (req "sdpMLineIndex" int)
              (req "candidate" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {call_id: string; candidates: Candidate.t list; version: int}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.call_id, t.candidates, t.version in
        let of_tuple v =
          let call_id, candidates, version = v in
          {call_id; candidates; version} in
        let with_tuple =
          obj3 (req "call_id" string)
            (req "candidates" (list Candidate.encoding))
            (req "version" int) in
        conv to_tuple of_tuple with_tuple
    end

    module Answer = struct
      module Answer = struct
        type t = {sdp: string} [@@deriving accessor]

        let encoding =
          let to_tuple t = (), t.sdp in
          let of_tuple v =
            let (), sdp = v in
            {sdp} in
          let with_tuple =
            obj2 (req "type" (constant "answer")) (req "sdp" string) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {call_id: string; answer: Answer.t; version: int}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.call_id, t.answer, t.version in
        let of_tuple v =
          let call_id, answer, version = v in
          {call_id; answer; version} in
        let with_tuple =
          obj3 (req "call_id" string)
            (req "offer" Answer.encoding)
            (req "version" int) in
        conv to_tuple of_tuple with_tuple
    end

    module Hangup = struct
      module Reason = struct
        type t = Ice_failed | Invite_timeout

        let encoding =
          string_enum
            ["ice_failed", Ice_failed; "invite_timeout", Invite_timeout]
      end

      type t = {call_id: string; version: int; reason: Reason.t option}
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.call_id, t.version, t.reason in
        let of_tuple v =
          let call_id, version, reason = v in
          {call_id; version; reason} in
        let with_tuple =
          obj3 (req "call_id" string) (req "version" int)
            (opt "reason" Reason.encoding) in
        conv to_tuple of_tuple with_tuple
    end
  end

  module Presence = struct
    module Presence = struct
      type t = Online | Offline | Unavailable

      let encoding =
        string_enum
          ["online", Online; "offline", Offline; "unavailable", Unavailable]
    end

    type t = {
      avatar_url: string option;
      displayname: string option;
      last_active_ago: int option;
      presence: Presence.t;
      currently_active: bool option;
      status_msg: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.avatar_url,
          t.displayname,
          t.last_active_ago,
          t.presence,
          t.currently_active,
          t.status_msg ) in
      let of_tuple v =
        let ( avatar_url,
              displayname,
              last_active_ago,
              presence,
              currently_active,
              status_msg ) =
          v in
        {
          avatar_url;
          displayname;
          last_active_ago;
          presence;
          currently_active;
          status_msg;
        } in
      let with_tuple =
        obj6 (opt "avatar_url" string) (opt "displayname" string)
          (opt "last_active_ago" int)
          (req "presence" Presence.encoding)
          (opt "currently_active" bool)
          (opt "status_msg" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Push_rules = struct
    type t = {
      content: Push_rule.t list option;
      override: Push_rule.t list option;
      room: Push_rule.t list option;
      sender: Push_rule.t list option;
      underride: Push_rule.t list option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.content, t.override, t.room, t.sender, t.underride in
      let of_tuple v =
        let content, override, room, sender, underride = v in
        {content; override; room; sender; underride} in
      let with_tuple =
        obj1
          (req "global"
             (obj5
                (opt "content" (list Push_rule.encoding))
                (opt "override" (list Push_rule.encoding))
                (opt "room" (list Push_rule.encoding))
                (opt "sender" (list Push_rule.encoding))
                (opt "underride" (list Push_rule.encoding)))) in
      conv to_tuple of_tuple with_tuple
  end

  module Typing = struct
    type t = {users_id: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.users_id in
      let of_tuple v =
        let users_id = v in
        {users_id} in
      let with_tuple = obj1 (req "user_ids" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Receipt = struct
    module Receipts = struct
      module Timestamp = struct
        type t = {ts: int} [@@deriving accessor]

        let encoding =
          let to_tuple t = t.ts in
          let of_tuple v =
            let ts = v in
            {ts} in
          let with_tuple = obj1 (req "ts" int) in
          conv to_tuple of_tuple with_tuple
      end

      type t = {users: (string * Timestamp.t) list} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.users in
        let of_tuple v =
          let users = v in
          {users} in
        let with_tuple = obj1 (req "m.read" (assoc Timestamp.encoding)) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {receipts: (string * Receipts.t) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.receipts in
      let of_tuple v =
        let receipts = v in
        {receipts} in
      let with_tuple = obj1 (req "content" (assoc Receipts.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  module Fully_read = struct
    type t = {event_id: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.event_id in
      let of_tuple v =
        let event_id = v in
        {event_id} in
      let with_tuple = obj1 (req "event_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Tag = struct
    module Tag = struct
      type t = {order: float option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.order in
        let of_tuple v =
          let order = v in
          {order} in
        let with_tuple = obj1 (opt "order" float) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {tags: (string * Tag.t) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.tags in
      let of_tuple v =
        let tags = v in
        {tags} in
      let with_tuple = obj1 (req "tags" (assoc Tag.encoding)) in
      conv to_tuple of_tuple with_tuple
  end

  module Direct = struct
    type t = {directs: (string * string list) list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.directs in
      let of_tuple v =
        let directs = v in
        {directs} in
      let with_tuple = assoc (list string) in
      conv to_tuple of_tuple with_tuple
  end

  module Ignored_users_list = struct
    type t = {users: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t =
        let f u = u, () in
        List.map f t.users in
      let of_tuple v =
        let f (u, ()) = u in
        let users = v in
        let users = List.map f users in
        {users} in
      let with_tuple = obj1 (req "ignored_users" (assoc unit)) in
      conv to_tuple of_tuple with_tuple
  end

  module Room_key = struct
    type t = {
      algorithm: string;
      room_id: string;
      session_id: string;
      session_key: string;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.algorithm, t.room_id, t.session_id, t.session_key in
      let of_tuple v =
        let algorithm, room_id, session_id, session_key = v in
        {algorithm; room_id; session_id; session_key} in
      let with_tuple =
        obj4 (req "algorithm" string) (req "room_id" string)
          (req "session_id" string) (req "session_key" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Room_key_request = struct
    module Request_key_info = struct
      type t = {
        algorithm: string;
        room_id: string;
        sender_key: string;
        session_key: string;
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t = t.algorithm, t.room_id, t.sender_key, t.session_key in
        let of_tuple v =
          let algorithm, room_id, sender_key, session_key = v in
          {algorithm; room_id; sender_key; session_key} in
        let with_tuple =
          obj4 (req "algorithm" string) (req "room_id" string)
            (req "sender_key" string) (req "session_key" string) in
        conv to_tuple of_tuple with_tuple
    end

    module Action = struct
      type t = Request | Cancel_request

      let encoding =
        string_enum ["request", Request; "cancel_request", Cancel_request]
    end

    type t = {
      body: Request_key_info.t;
      action: Action.t;
      requesting_device_id: string;
      request_id: string;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.body, t.action, t.requesting_device_id, t.request_id in
      let of_tuple v =
        let body, action, requesting_device_id, request_id = v in
        {body; action; requesting_device_id; request_id} in
      let with_tuple =
        obj4
          (req "body" Request_key_info.encoding)
          (req "action" Action.encoding)
          (req "requesting_device_id" string)
          (req "request_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Forwarded_room_key = struct
    type t = {
      algorithm: string;
      room_id: string;
      sender_key: string;
      session_id: string;
      session_key: string;
      sender_claimed_ed25519_key: string;
      forwarding_curve25519_key_chain: string list;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.algorithm,
          t.room_id,
          t.sender_key,
          t.session_id,
          t.session_key,
          t.sender_claimed_ed25519_key,
          t.forwarding_curve25519_key_chain ) in
      let of_tuple v =
        let ( algorithm,
              room_id,
              sender_key,
              session_id,
              session_key,
              sender_claimed_ed25519_key,
              forwarding_curve25519_key_chain ) =
          v in
        {
          algorithm;
          room_id;
          sender_key;
          session_id;
          session_key;
          sender_claimed_ed25519_key;
          forwarding_curve25519_key_chain;
        } in
      let with_tuple =
        obj7 (req "algorithm" string) (req "room_id" string)
          (req "sender_key" string) (req "session_id" string)
          (req "session_key" string)
          (req "sender_claimed_ed25519_key" string)
          (req "forwarding_curve25519_key_chain" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Dummy = struct
    type t = unit [@@deriving accessor]

    let encoding = obj1 (req "content" unit)
  end

  module Custom = struct
    type t = {content: Ezjsonm.value} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.content in
      let of_tuple v =
        let content = v in
        {content} in
      let with_tuple = any in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    (* Room events *)
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
    | Message of Message.t
    | Name of Name.t
    | Topic of Topic.t
    | Avatar of Avatar.t
    | Pinned of Pinned_events.t
    (* Call events *)
    | Invite of Call.Invite.t
    | Candidates of Call.Candidates.t
    | Answer of Call.Answer.t
    | Hangup of Call.Hangup.t
    (* Other events *)
    | Presence of Presence.t
    | Push_rules of Push_rules.t
    | Typing of Typing.t
    | Receipt of Receipt.t
    | Fully_read of Fully_read.t
    | Tag of Tag.t
    | Direct of Direct.t
    | Room_key of Room_key.t
    | Room_key_request of Room_key_request.t
    | Forwarded_room_key of Forwarded_room_key.t
    | Dummy of Dummy.t

  let encoding =
    union
      [
        case Aliases.encoding
          (function Aliases t -> Some t | _ -> None)
          (fun t -> Aliases t);
        case Canonical_alias.encoding
          (function Canonical_alias t -> Some t | _ -> None)
          (fun t -> Canonical_alias t);
        case Create.encoding
          (function Create t -> Some t | _ -> None)
          (fun t -> Create t);
        case Join_rules.encoding
          (function Join_rules t -> Some t | _ -> None)
          (fun t -> Join_rules t);
        case Member.encoding
          (function Member t -> Some t | _ -> None)
          (fun t -> Member t);
        case Power_levels.encoding
          (function Power_levels t -> Some t | _ -> None)
          (fun t -> Power_levels t);
        case History_visibility.encoding
          (function History_visibility t -> Some t | _ -> None)
          (fun t -> History_visibility t);
        case Third_party_invite.encoding
          (function Third_party_invite t -> Some t | _ -> None)
          (fun t -> Third_party_invite t);
        case Guest_access.encoding
          (function Guest_access t -> Some t | _ -> None)
          (fun t -> Guest_access t);
        case Server_acl.encoding
          (function Server_acl t -> Some t | _ -> None)
          (fun t -> Server_acl t);
        case Tombstone.encoding
          (function Tombstone t -> Some t | _ -> None)
          (fun t -> Tombstone t);
        case Encryption.encoding
          (function Encryption t -> Some t | _ -> None)
          (fun t -> Encryption t);
        case Encrypted.encoding
          (function Encrypted t -> Some t | _ -> None)
          (fun t -> Encrypted t);
        case Message.encoding
          (function Message t -> Some t | _ -> None)
          (fun t -> Message t);
        case Name.encoding
          (function Name t -> Some t | _ -> None)
          (fun t -> Name t);
        case Topic.encoding
          (function Topic t -> Some t | _ -> None)
          (fun t -> Topic t);
        case Avatar.encoding
          (function Avatar t -> Some t | _ -> None)
          (fun t -> Avatar t);
        case Pinned_events.encoding
          (function Pinned t -> Some t | _ -> None)
          (fun t -> Pinned t);
        case Call.Invite.encoding
          (function Invite t -> Some t | _ -> None)
          (fun t -> Invite t);
        case Call.Candidates.encoding
          (function Candidates t -> Some t | _ -> None)
          (fun t -> Candidates t);
        case Call.Answer.encoding
          (function Answer t -> Some t | _ -> None)
          (fun t -> Answer t);
        case Call.Hangup.encoding
          (function Hangup t -> Some t | _ -> None)
          (fun t -> Hangup t);
        case Presence.encoding
          (function Presence t -> Some t | _ -> None)
          (fun t -> Presence t);
        case Push_rules.encoding
          (function Push_rules t -> Some t | _ -> None)
          (fun t -> Push_rules t);
        case Typing.encoding
          (function Typing t -> Some t | _ -> None)
          (fun t -> Typing t);
        case Receipt.encoding
          (function Receipt t -> Some t | _ -> None)
          (fun t -> Receipt t);
        case Fully_read.encoding
          (function Fully_read t -> Some t | _ -> None)
          (fun t -> Fully_read t);
        case Tag.encoding
          (function Tag t -> Some t | _ -> None)
          (fun t -> Tag t);
        case Direct.encoding
          (function Direct t -> Some t | _ -> None)
          (fun t -> Direct t);
        case Room_key.encoding
          (function Room_key t -> Some t | _ -> None)
          (fun t -> Room_key t);
        case Room_key_request.encoding
          (function Room_key_request t -> Some t | _ -> None)
          (fun t -> Room_key_request t);
        case Forwarded_room_key.encoding
          (function Forwarded_room_key t -> Some t | _ -> None)
          (fun t -> Forwarded_room_key t);
        case Dummy.encoding
          (function Dummy t -> Some t | _ -> None)
          (fun t -> Dummy t);
      ]

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
    | Encryption _ -> "m.room.encryption"
    | Encrypted _ -> "m.room.encrypted"
    | Message _ -> "m.room.message"
    | Name _ -> "m.room.name"
    | Topic _ -> "m.room.topic"
    | Avatar _ -> "m.room.avatar"
    | Pinned _ -> "m.room.pinned_events"
    | Invite _ -> "m.call.invite"
    | Candidates _ -> "m.call.candidates"
    | Answer _ -> "m.call.answer"
    | Hangup _ -> "m.call.hangup"
    | Presence _ -> "m.presence"
    | Push_rules _ -> "m.push_rules"
    | Typing _ -> "m.typing"
    | Receipt _ -> "m.receipt"
    | Fully_read _ -> "m.fully_read"
    | Tag _ -> "m.tag"
    | Direct _ -> "m.direct"
    | Room_key _ -> "m.room_key"
    | Room_key_request _ -> "m.room_key_request"
    | Forwarded_room_key _ -> "m.forwarded_room_key"
    | Dummy _ -> "m.dummy"

  let redact =
    union
      [
        case Create.redact
          (function Create t -> Some t | _ -> None)
          (fun t -> Create t);
        case Join_rules.redact
          (function Join_rules t -> Some t | _ -> None)
          (fun t -> Join_rules t);
        case Member.redact
          (function Member t -> Some t | _ -> None)
          (fun t -> Member t);
        case Power_levels.redact
          (function Power_levels t -> Some t | _ -> None)
          (fun t -> Power_levels t);
        case History_visibility.redact
          (function History_visibility t -> Some t | _ -> None)
          (fun t -> History_visibility t);
        case empty
          (function
            | Aliases _ -> Some ()
            | Canonical_alias _ -> Some ()
            | Third_party_invite _ -> Some ()
            | Guest_access _ -> Some ()
            | Server_acl _ -> Some ()
            | Tombstone _ -> Some ()
            | Encryption _ -> Some ()
            | Encrypted _ -> Some ()
            | Message _ -> Some ()
            | Name _ -> Some ()
            | Topic _ -> Some ()
            | Avatar _ -> Some ()
            | Pinned _ -> Some ()
            | Invite _ -> Some ()
            | Candidates _ -> Some ()
            | Answer _ -> Some ()
            | Hangup _ -> Some ()
            | Presence _ -> Some ()
            | Push_rules _ -> Some ()
            | Typing _ -> Some ()
            | Receipt _ -> Some ()
            | Fully_read _ -> Some ()
            | Tag _ -> Some ()
            | Direct _ -> Some ()
            | Room_key _ -> Some ()
            | Room_key_request _ -> Some ()
            | Forwarded_room_key _ -> Some ()
            | Dummy _ -> Some ()
            | _ -> None)
          (fun _ -> assert false);
      ]
end

(* Notes:
   - Temporary patch of the signatures by adding them here as optional.
     This way, we do not loose the signatures when the event is sent by an other server.
     A better solution should be found in the long term. (maybe creatin a "signed event" object around the events)
*)
module Event = struct
  type t = {
    event_content: Event_content.t;
    event_type: string;
    signatures: (string * (string * string) list) list option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = (t.event_type, t.event_content), t.signatures in
    let of_tuple v =
      let (event_type, event_content), signatures = v in
      {event_type; event_content; signatures} in
    let with_tuple =
      merge_objs
        (cond
           (obj1 (req "type" string))
           [
             ( "m.room.aliases",
               case
                 (Content.content Event_content.Aliases.encoding)
                 (function Event_content.Aliases t -> Some t | _ -> None)
                 (fun t -> Aliases t) );
             ( "m.room.canonical_alias",
               case
                 (Content.content Event_content.Canonical_alias.encoding)
                 (function
                   | Event_content.Canonical_alias t -> Some t | _ -> None)
                 (fun t -> Canonical_alias t) );
             ( "m.room.create",
               case
                 (Content.content Event_content.Create.encoding)
                 (function Event_content.Create t -> Some t | _ -> None)
                 (fun t -> Create t) );
             ( "m.room.join_rules",
               case
                 (Content.content Event_content.Join_rules.encoding)
                 (function Event_content.Join_rules t -> Some t | _ -> None)
                 (fun t -> Join_rules t) );
             ( "m.room.member",
               case
                 (Content.content Event_content.Member.encoding)
                 (function Event_content.Member t -> Some t | _ -> None)
                 (fun t -> Member t) );
             ( "m.room.power_levels",
               case
                 (Content.content Event_content.Power_levels.encoding)
                 (function Event_content.Power_levels t -> Some t | _ -> None)
                 (fun t -> Power_levels t) );
             ( "m.room.history_visibility",
               case
                 (Content.content Event_content.History_visibility.encoding)
                 (function
                   | Event_content.History_visibility t -> Some t | _ -> None)
                 (fun t -> History_visibility t) );
             ( "m.room.third_party_invite",
               case
                 (Content.content Event_content.Third_party_invite.encoding)
                 (function
                   | Event_content.Third_party_invite t -> Some t | _ -> None)
                 (fun t -> Third_party_invite t) );
             ( "m.room.guest_access",
               case
                 (Content.content Event_content.Guest_access.encoding)
                 (function Event_content.Guest_access t -> Some t | _ -> None)
                 (fun t -> Guest_access t) );
             ( "m.room.server_acl",
               case
                 (Content.content Event_content.Server_acl.encoding)
                 (function Event_content.Server_acl t -> Some t | _ -> None)
                 (fun t -> Server_acl t) );
             ( "m.room.tombstone",
               case
                 (Content.content Event_content.Tombstone.encoding)
                 (function Event_content.Tombstone t -> Some t | _ -> None)
                 (fun t -> Tombstone t) );
             ( "m.room.encryption",
               case
                 (Content.content Event_content.Encryption.encoding)
                 (function Event_content.Encryption t -> Some t | _ -> None)
                 (fun t -> Encryption t) );
             ( "m.room.encrypted",
               case
                 (Content.content Event_content.Encrypted.encoding)
                 (function Event_content.Encrypted t -> Some t | _ -> None)
                 (fun t -> Encrypted t) );
             ( "m.room.message",
               case
                 (Content.content Event_content.Message.encoding)
                 (function Event_content.Message t -> Some t | _ -> None)
                 (fun t -> Message t) );
             ( "m.room.name",
               case
                 (Content.content Event_content.Name.encoding)
                 (function Event_content.Name t -> Some t | _ -> None)
                 (fun t -> Name t) );
             ( "m.room.topic",
               case
                 (Content.content Event_content.Topic.encoding)
                 (function Event_content.Topic t -> Some t | _ -> None)
                 (fun t -> Topic t) );
             ( "m.room.avatar",
               case
                 (Content.content Event_content.Avatar.encoding)
                 (function Event_content.Avatar t -> Some t | _ -> None)
                 (fun t -> Avatar t) );
             ( "m.room.pinned_events",
               case
                 (Content.content Event_content.Pinned_events.encoding)
                 (function Event_content.Pinned t -> Some t | _ -> None)
                 (fun t -> Pinned t) );
             ( "m.call.invite",
               case
                 (Content.content Event_content.Call.Invite.encoding)
                 (function Event_content.Invite t -> Some t | _ -> None)
                 (fun t -> Invite t) );
             ( "m.call.candidates",
               case
                 (Content.content Event_content.Call.Candidates.encoding)
                 (function Event_content.Candidates t -> Some t | _ -> None)
                 (fun t -> Candidates t) );
             ( "m.call.answer",
               case
                 (Content.content Event_content.Call.Answer.encoding)
                 (function Event_content.Answer t -> Some t | _ -> None)
                 (fun t -> Answer t) );
             ( "m.call.hangup",
               case
                 (Content.content Event_content.Call.Hangup.encoding)
                 (function Event_content.Hangup t -> Some t | _ -> None)
                 (fun t -> Hangup t) );
             ( "m.presence",
               case
                 (Content.content Event_content.Presence.encoding)
                 (function Event_content.Presence t -> Some t | _ -> None)
                 (fun t -> Presence t) );
             ( "m.push_rules",
               case
                 (Content.content Event_content.Push_rules.encoding)
                 (function Event_content.Push_rules t -> Some t | _ -> None)
                 (fun t -> Push_rules t) );
             ( "m.typing",
               case
                 (Content.content Event_content.Typing.encoding)
                 (function Event_content.Typing t -> Some t | _ -> None)
                 (fun t -> Typing t) );
             ( "m.receipt",
               case
                 (Content.content Event_content.Receipt.encoding)
                 (function Event_content.Receipt t -> Some t | _ -> None)
                 (fun t -> Receipt t) );
             ( "m.fully_read",
               case
                 (Content.content Event_content.Fully_read.encoding)
                 (function Event_content.Fully_read t -> Some t | _ -> None)
                 (fun t -> Fully_read t) );
             ( "m.tag",
               case
                 (Content.content Event_content.Tag.encoding)
                 (function Event_content.Tag t -> Some t | _ -> None)
                 (fun t -> Tag t) );
             ( "m.direct",
               case
                 (Content.content Event_content.Direct.encoding)
                 (function Event_content.Direct t -> Some t | _ -> None)
                 (fun t -> Direct t) );
             ( "m.room_key",
               case
                 (Content.content Event_content.Room_key.encoding)
                 (function Event_content.Room_key t -> Some t | _ -> None)
                 (fun t -> Room_key t) );
             ( "m.room_key_request",
               case
                 (Content.content Event_content.Room_key_request.encoding)
                 (function
                   | Event_content.Room_key_request t -> Some t | _ -> None)
                 (fun t -> Room_key_request t) );
             ( "m.forwarded_room_key",
               case
                 (Content.content Event_content.Forwarded_room_key.encoding)
                 (function
                   | Event_content.Forwarded_room_key t -> Some t | _ -> None)
                 (fun t -> Forwarded_room_key t) );
             ( "m.dummy",
               case
                 (Content.content Event_content.Dummy.encoding)
                 (function Event_content.Dummy t -> Some t | _ -> None)
                 (fun t -> Dummy t) );
           ])
        (obj1 (opt "signatures" (assoc (assoc string)))) in
    conv to_tuple of_tuple with_tuple
end

(* Room *)
module Room_event = struct
  module Unsigned = struct
    type t = {
      age: int option;
      redacted_because: Event.t option;
      transaction_id: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.age, t.redacted_because, t.transaction_id in
      let of_tuple v =
        let age, redacted_because, transaction_id = v in
        {age; redacted_because; transaction_id} in
      let with_tuple =
        obj3 (opt "age" int)
          (opt "redacted_because" Event.encoding)
          (opt "transaction_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
    event: Event.t;
    event_id: string option;
    sender: string option;
    origin: string option;
    origin_server_ts: int option;
    unsigned: Unsigned.t option;
    room_id: string option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.event,
        ( t.event_id,
          t.sender,
          t.origin,
          t.origin_server_ts,
          t.unsigned,
          t.room_id ) ) in
    let of_tuple v =
      let event, (event_id, sender, origin, origin_server_ts, unsigned, room_id)
          =
        v in
      {event; event_id; sender; origin; origin_server_ts; unsigned; room_id}
    in
    let with_tuple =
      merge_objs Event.encoding
        (obj6 (opt "event_id" string) (opt "sender" string)
           (opt "origin" string)
           (opt "origin_server_ts" int)
           (opt "unsigned" Unsigned.encoding)
           (opt "room_id" string)) in
    conv to_tuple of_tuple with_tuple

  let get_event_content t = Event.get_event_content t.event
end

(* State event *)
module State_event = struct
  type t = {room_event: Room_event.t; state_key: string} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.room_event, t.state_key in
    let of_tuple v =
      let room_event, state_key = v in
      {room_event; state_key} in
    let with_tuple =
      merge_objs Room_event.encoding (obj1 (dft "state_key" string "")) in
    conv to_tuple of_tuple with_tuple

  let get_event t = Room_event.get_event t.room_event
  let get_event_content t = Room_event.get_event_content t.room_event
end

type event =
  [ `Event of Event.t
  | `Room_event of Room_event.t
  | `State_event of State_event.t ]

let encoding : event encoding =
  union
    [
      case State_event.encoding
        (function `State_event t -> Some t | _ -> None)
        (fun t -> `State_event t);
      case Room_event.encoding
        (function `Room_event t -> Some t | _ -> None)
        (fun t -> `Room_event t);
      case Event.encoding
        (function `Event t -> Some t | _ -> None)
        (fun t -> `Event t);
    ]

(* Persistent data unit *)
(* This is a newer version of the PDU: Simply wrapping the events inside a PDU
   was not the answer due to some keys not being required in PDUs. For now, this
   should suffice, but adding some conversion functions asking for the additional
   arguments could be usefull. *)
module Pdu = struct
  module Hashes = struct
    type t = {sha256: string} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.sha256 in
      let of_tuple v =
        let sha256 = v in
        {sha256} in
      let with_tuple = obj1 (req "sha256" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Unsigned = struct
    type t = {
      age: int option;
      prev_object: Event_content.t option;
      prev_sender: string option;
      redacted_because: string option;
      replaces_state: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.age,
          t.prev_object,
          t.prev_sender,
          t.redacted_because,
          t.replaces_state ) in
      let of_tuple v =
        let age, prev_object, prev_sender, redacted_because, replaces_state =
          v in
        {age; prev_object; prev_sender; redacted_because; replaces_state} in
      let with_tuple =
        obj5 (opt "age" int)
          (opt "prev_object" Event_content.encoding)
          (opt "prev_sender" string)
          (opt "redacted_because" string)
          (opt "replaces_state" string) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
    auth_events: string list;
    event_content: Event_content.t;
    depth: int;
    hashes: Hashes.t option;
    origin: string;
    origin_server_ts: int;
    prev_events: string list;
    prev_state: string list;
    redacts: string option;
    room_id: string;
    sender: string;
    signatures: (string * (string * string) list) list;
    state_key: string option;
    event_type: string;
    unsigned: Unsigned.t option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( (t.event_type, t.event_content),
        ( ( t.auth_events,
            t.depth,
            t.hashes,
            t.origin,
            t.origin_server_ts,
            t.prev_events,
            t.prev_state,
            t.redacts,
            t.room_id,
            t.sender ),
          (t.signatures, t.state_key, t.unsigned) ) ) in
    let of_tuple v =
      let ( (event_type, event_content),
            ( ( auth_events,
                depth,
                hashes,
                origin,
                origin_server_ts,
                prev_events,
                prev_state,
                redacts,
                room_id,
                sender ),
              (signatures, state_key, unsigned) ) ) =
        v in
      {
        auth_events;
        event_content;
        depth;
        hashes;
        origin;
        origin_server_ts;
        prev_events;
        prev_state;
        redacts;
        room_id;
        sender;
        signatures;
        state_key;
        event_type;
        unsigned;
      } in
    let with_tuple =
      merge_objs
        (cond
           (obj1 (req "type" string))
           [
             ( "m.room.aliases",
               case
                 (Content.content Event_content.Aliases.encoding)
                 (function Event_content.Aliases t -> Some t | _ -> None)
                 (fun t -> Aliases t) );
             ( "m.room.canonical_alias",
               case
                 (Content.content Event_content.Canonical_alias.encoding)
                 (function
                   | Event_content.Canonical_alias t -> Some t | _ -> None)
                 (fun t -> Canonical_alias t) );
             ( "m.room.create",
               case
                 (Content.content Event_content.Create.encoding)
                 (function Event_content.Create t -> Some t | _ -> None)
                 (fun t -> Create t) );
             ( "m.room.join_rules",
               case
                 (Content.content Event_content.Join_rules.encoding)
                 (function Event_content.Join_rules t -> Some t | _ -> None)
                 (fun t -> Join_rules t) );
             ( "m.room.member",
               case
                 (Content.content Event_content.Member.encoding)
                 (function Event_content.Member t -> Some t | _ -> None)
                 (fun t -> Member t) );
             ( "m.room.power_levels",
               case
                 (Content.content Event_content.Power_levels.encoding)
                 (function Event_content.Power_levels t -> Some t | _ -> None)
                 (fun t -> Power_levels t) );
             ( "m.room.history_visibility",
               case
                 (Content.content Event_content.History_visibility.encoding)
                 (function
                   | Event_content.History_visibility t -> Some t | _ -> None)
                 (fun t -> History_visibility t) );
             ( "m.room.third_party_invite",
               case
                 (Content.content Event_content.Third_party_invite.encoding)
                 (function
                   | Event_content.Third_party_invite t -> Some t | _ -> None)
                 (fun t -> Third_party_invite t) );
             ( "m.room.guest_access",
               case
                 (Content.content Event_content.Guest_access.encoding)
                 (function Event_content.Guest_access t -> Some t | _ -> None)
                 (fun t -> Guest_access t) );
             ( "m.room.server_acl",
               case
                 (Content.content Event_content.Server_acl.encoding)
                 (function Event_content.Server_acl t -> Some t | _ -> None)
                 (fun t -> Server_acl t) );
             ( "m.room.tombstone",
               case
                 (Content.content Event_content.Tombstone.encoding)
                 (function Event_content.Tombstone t -> Some t | _ -> None)
                 (fun t -> Tombstone t) );
             ( "m.room.encryption",
               case
                 (Content.content Event_content.Encryption.encoding)
                 (function Event_content.Encryption t -> Some t | _ -> None)
                 (fun t -> Encryption t) );
             ( "m.room.encrypted",
               case
                 (Content.content Event_content.Encrypted.encoding)
                 (function Event_content.Encrypted t -> Some t | _ -> None)
                 (fun t -> Encrypted t) );
             ( "m.room.message",
               case
                 (Content.content Event_content.Message.encoding)
                 (function Event_content.Message t -> Some t | _ -> None)
                 (fun t -> Message t) );
             ( "m.room.name",
               case
                 (Content.content Event_content.Name.encoding)
                 (function Event_content.Name t -> Some t | _ -> None)
                 (fun t -> Name t) );
             ( "m.room.topic",
               case
                 (Content.content Event_content.Topic.encoding)
                 (function Event_content.Topic t -> Some t | _ -> None)
                 (fun t -> Topic t) );
             ( "m.room.avatar",
               case
                 (Content.content Event_content.Avatar.encoding)
                 (function Event_content.Avatar t -> Some t | _ -> None)
                 (fun t -> Avatar t) );
             ( "m.room.pinned_events",
               case
                 (Content.content Event_content.Pinned_events.encoding)
                 (function Event_content.Pinned t -> Some t | _ -> None)
                 (fun t -> Pinned t) );
             ( "m.call.invite",
               case
                 (Content.content Event_content.Call.Invite.encoding)
                 (function Event_content.Invite t -> Some t | _ -> None)
                 (fun t -> Invite t) );
             ( "m.call.candidates",
               case
                 (Content.content Event_content.Call.Candidates.encoding)
                 (function Event_content.Candidates t -> Some t | _ -> None)
                 (fun t -> Candidates t) );
             ( "m.call.answer",
               case
                 (Content.content Event_content.Call.Answer.encoding)
                 (function Event_content.Answer t -> Some t | _ -> None)
                 (fun t -> Answer t) );
             ( "m.call.hangup",
               case
                 (Content.content Event_content.Call.Hangup.encoding)
                 (function Event_content.Hangup t -> Some t | _ -> None)
                 (fun t -> Hangup t) );
             ( "m.presence",
               case
                 (Content.content Event_content.Presence.encoding)
                 (function Event_content.Presence t -> Some t | _ -> None)
                 (fun t -> Presence t) );
             ( "m.push_rules",
               case
                 (Content.content Event_content.Push_rules.encoding)
                 (function Event_content.Push_rules t -> Some t | _ -> None)
                 (fun t -> Push_rules t) );
             ( "m.typing",
               case
                 (Content.content Event_content.Typing.encoding)
                 (function Event_content.Typing t -> Some t | _ -> None)
                 (fun t -> Typing t) );
             ( "m.receipt",
               case
                 (Content.content Event_content.Receipt.encoding)
                 (function Event_content.Receipt t -> Some t | _ -> None)
                 (fun t -> Receipt t) );
             ( "m.fully_read",
               case
                 (Content.content Event_content.Fully_read.encoding)
                 (function Event_content.Fully_read t -> Some t | _ -> None)
                 (fun t -> Fully_read t) );
             ( "m.tag",
               case
                 (Content.content Event_content.Tag.encoding)
                 (function Event_content.Tag t -> Some t | _ -> None)
                 (fun t -> Tag t) );
             ( "m.direct",
               case
                 (Content.content Event_content.Direct.encoding)
                 (function Event_content.Direct t -> Some t | _ -> None)
                 (fun t -> Direct t) );
             ( "m.room_key",
               case
                 (Content.content Event_content.Room_key.encoding)
                 (function Event_content.Room_key t -> Some t | _ -> None)
                 (fun t -> Room_key t) );
             ( "m.room_key_request",
               case
                 (Content.content Event_content.Room_key_request.encoding)
                 (function
                   | Event_content.Room_key_request t -> Some t | _ -> None)
                 (fun t -> Room_key_request t) );
             ( "m.forwarded_room_key",
               case
                 (Content.content Event_content.Forwarded_room_key.encoding)
                 (function
                   | Event_content.Forwarded_room_key t -> Some t | _ -> None)
                 (fun t -> Forwarded_room_key t) );
             ( "m.dummy",
               case
                 (Content.content Event_content.Dummy.encoding)
                 (function Event_content.Dummy t -> Some t | _ -> None)
                 (fun t -> Dummy t) );
           ])
        (merge_objs
           (obj10
              (req "auth_events" (list string))
              (req "depth" int)
              (opt "hashes" Hashes.encoding)
              (req "origin" string)
              (req "origin_server_ts" int)
              (req "prev_events" (list string))
              (req "prev_state" (list string))
              (opt "redacts" string) (req "room_id" string)
              (req "sender" string))
           (obj3
              (req "signatures" (assoc (assoc string)))
              (opt "state_key" string)
              (opt "unsigned" Unsigned.encoding))) in
    conv to_tuple of_tuple with_tuple

  let redact =
    let to_tuple t =
      ( (t.event_type, t.event_content),
        ( ( t.auth_events,
            t.depth,
            t.hashes,
            t.origin,
            t.origin_server_ts,
            t.prev_events,
            t.prev_state,
            t.redacts,
            t.room_id,
            t.sender ),
          (t.signatures, t.state_key, t.unsigned) ) ) in
    let of_tuple v =
      let ( (event_type, event_content),
            ( ( auth_events,
                depth,
                hashes,
                origin,
                origin_server_ts,
                prev_events,
                prev_state,
                redacts,
                room_id,
                sender ),
              (signatures, state_key, unsigned) ) ) =
        v in
      {
        auth_events;
        event_content;
        depth;
        hashes;
        origin;
        origin_server_ts;
        prev_events;
        prev_state;
        redacts;
        room_id;
        sender;
        signatures;
        state_key;
        event_type;
        unsigned;
      } in
    let with_tuple =
      merge_objs
        (cond
           (obj1 (req "type" string))
           [
             ( "m.room.aliases",
               case (Content.content unit)
                 (function Event_content.Aliases _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.canonical_alias",
               case (Content.content unit)
                 (function
                   | Event_content.Canonical_alias _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.create",
               case
                 (Content.content Event_content.Create.redact)
                 (function Event_content.Create t -> Some t | _ -> None)
                 (fun t -> Create t) );
             ( "m.room.join_rules",
               case
                 (Content.content Event_content.Join_rules.redact)
                 (function Event_content.Join_rules t -> Some t | _ -> None)
                 (fun t -> Join_rules t) );
             ( "m.room.member",
               case
                 (Content.content Event_content.Member.redact)
                 (function Event_content.Member t -> Some t | _ -> None)
                 (fun t -> Member t) );
             ( "m.room.power_levels",
               case
                 (Content.content Event_content.Power_levels.redact)
                 (function Event_content.Power_levels t -> Some t | _ -> None)
                 (fun t -> Power_levels t) );
             ( "m.room.history_visibility",
               case
                 (Content.content Event_content.History_visibility.redact)
                 (function
                   | Event_content.History_visibility t -> Some t | _ -> None)
                 (fun t -> History_visibility t) );
             ( "m.room.third_party_invite",
               case (Content.content unit)
                 (function
                   | Event_content.Third_party_invite _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.guest_access",
               case (Content.content unit)
                 (function
                   | Event_content.Guest_access _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.server_acl",
               case (Content.content unit)
                 (function Event_content.Server_acl _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.tombstone",
               case (Content.content unit)
                 (function Event_content.Tombstone _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.encryption",
               case (Content.content unit)
                 (function Event_content.Encryption _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.encrypted",
               case (Content.content unit)
                 (function Event_content.Encrypted _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.message",
               case (Content.content unit)
                 (function Event_content.Message _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.name",
               case (Content.content unit)
                 (function Event_content.Name _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.topic",
               case (Content.content unit)
                 (function Event_content.Topic _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.avatar",
               case (Content.content unit)
                 (function Event_content.Avatar _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room.pinned_events",
               case (Content.content unit)
                 (function Event_content.Pinned _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.call.invite",
               case (Content.content unit)
                 (function Event_content.Invite _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.call.candidates",
               case (Content.content unit)
                 (function Event_content.Candidates _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.call.answer",
               case (Content.content unit)
                 (function Event_content.Answer _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.call.hangup",
               case (Content.content unit)
                 (function Event_content.Hangup _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.presence",
               case (Content.content unit)
                 (function Event_content.Presence _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.push_rules",
               case (Content.content unit)
                 (function Event_content.Push_rules _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.typing",
               case (Content.content unit)
                 (function Event_content.Typing _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.receipt",
               case (Content.content unit)
                 (function Event_content.Receipt _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.fully_read",
               case (Content.content unit)
                 (function Event_content.Fully_read _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.tag",
               case (Content.content unit)
                 (function Event_content.Tag _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.direct",
               case (Content.content unit)
                 (function Event_content.Direct _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room_key",
               case (Content.content unit)
                 (function Event_content.Room_key _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.room_key_request",
               case (Content.content unit)
                 (function
                   | Event_content.Room_key_request _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.forwarded_room_key",
               case (Content.content unit)
                 (function
                   | Event_content.Forwarded_room_key _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
             ( "m.dummy",
               case (Content.content unit)
                 (function Event_content.Dummy _ -> Some () | _ -> None)
                 (fun _ -> assert false) );
           ])
        (merge_objs
           (obj10
              (req "auth_events" (list string))
              (req "depth" int)
              (opt "hashes" Hashes.encoding)
              (req "origin" string)
              (req "origin_server_ts" int)
              (req "prev_events" (list string))
              (req "prev_state" (list string))
              (opt "redacts" string) (req "room_id" string)
              (req "sender" string))
           (obj3
              (req "signatures" (assoc (assoc string)))
              (opt "state_key" string)
              (opt "unsigned" Unsigned.encoding))) in
    conv to_tuple of_tuple with_tuple

  let get_event_content t = t.event_content
end
