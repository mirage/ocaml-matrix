open Json_encoding

module Null:
sig
  val string: string option encoding
  val int: int option encoding
end

module Empty:
sig
  module type JSON  =
  sig
    type%accessor t = unit
    val encoding: t encoding
    val pp: t Fmt.t
  end

  module type QUERY =
  sig
    type%accessor t = unit
    val args: t -> (string * string list) list
  end

  module Json: JSON

  module Query: QUERY
end

module Push_rule:
sig
  module Action:
  sig
    module Tweak:
    sig
      type%accessor t =
        { tweak: string
        ; value: Repr.value option
        }
      val encoding: t encoding
    end
    type t = Notify | Dont_notify | Coalesce | Tweak of Tweak.t
    val encoding: t encoding
  end
  module Push_condition:
  sig
    type%accessor t =
      { kind: string
      ; key: string option
      ; pattern: string option
      ; is: string option
      }
    val encoding: t encoding
  end
  type%accessor t =
    { actions: Action.t list
    ; default: bool
    ; enabled: bool
    ; rule_id: string
    ; conditions: Push_condition.t list option
    ; pattern: string option
    }
  val encoding: t encoding
end

module Message:
sig
  module Encrypted_file:
  sig
    module Jwk:
    sig
      type%accessor t =
        { kty: string
        ; key_ops: string list
        ; alg: string
        ; k: string
        ; ext: bool
        }
      val encoding: t encoding
    end
      type%accessor t =
      { url: string
      ; key: Jwk.t
      ; iv: string
      ; hashes: (string * string) list
      ; v: string
      }
    val encoding: t encoding
  end
  module Thumbnail:
  sig
    type%accessor t =
      { h: int option
      ; w: int option
      ; mimetype: string option
      ; size: int option
      }
    val encoding: t encoding
  end
  module Image:
  sig
    type%accessor t =
      { h: int option
      ; w: int option
      ; mimetype: string option
      ; size: int option
      ; thumbnail_url: string option
      ; thumbnail_file: Encrypted_file.t option
      ; thumbnail_info: Thumbnail.t option
      }
    val encoding: t encoding
  end
  module Video:
  sig
    type%accessor t =
      { duration: int option
      ; h: int option
      ; w: int option
      ; mimetype: string option
      ; size: int option
      ; thumbnail_url: string option
      ; thumbnail_file: Encrypted_file.t option
      ; thumbnail_info: Thumbnail.t option
      }
    val encoding: t encoding
  end
  module Location:
  sig
    type%accessor t =
      { thumbnail_url: string option
      ; thumbnail_file: Encrypted_file.t option
      ; thumbnail_info: Thumbnail.t option
      }
    val encoding: t encoding
  end
  module File:
  sig
    type%accessor t =
      { mimetype: string option
      ; size: int option
      ; thumbnail_url: string option
      ; thumbnail_file: Encrypted_file.t option
      ; thumbnail_info: Thumbnail.t option
      }
    val encoding: t encoding
  end
  module Audio:
  sig
    type%accessor t =
      { mimetype: string option
      ; duration: int option
      ; size: int option
      }
    val encoding: t encoding
  end
end

module Events:
sig
  module Event_content:
  sig
    module Membership:
    sig
      type t = Invite | Join | Knock | Leave | Ban
      val encoding: t encoding
      val to_string: t -> string
    end
    module Aliases:
    sig
      type%accessor t = { aliases: string list  }
      val encoding: t encoding
    end
    module Canonical_alias:
    sig
      type%accessor t =
        { alias: string option option
        ; alt_aliases: string list option
        }
      val encoding: t encoding
    end
    module Create:
    sig
      module Previous_room:
      sig
        type%accessor t = { room_id: string; event_id: string  }
        val encoding: t encoding
      end
      type%accessor t =
        { creator: string
        ; federate: bool option
        ; room_version: string option
        ; predecessor: Previous_room.t option
        }
      val encoding: t encoding
    end
    module Join_rules:
    sig
      type rule = Public | Knock | Invite | Private
      type%accessor t = { join_rule: rule  }
      val encoding: t encoding
    end
    module Member:
    sig
      module Third_party_invite:
      sig
        module Signed:
        sig
          type%accessor t =
            { mxid: string
            ; signature: unit
            ; token: string
            }
          val encoding: t encoding
        end
        type%accessor t = { display_name: string; signed: Signed.t  }
        val encoding: t encoding
      end
      type%accessor t =
        { avatar_url: string option option
        ; displayname: string option option
        ; membership: Membership.t
        ; is_direct: bool option
        ; reason: string option
        ; third_party_invite: Third_party_invite.t option
        }
      val encoding: t encoding
    end
    module Power_levels:
    sig
      module Notifications:
      sig
        type%accessor t = { room: int  }
        val encoding: t encoding
      end
      type%accessor t =
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
        }
      val encoding: t encoding
    end
    module History_visibility:
    sig
      type visibility = Invited | Joined | Shared | World_readable
      type%accessor t = { visibility: visibility  }
      val encoding: t encoding
    end
    module Third_party_invite:
    sig
      module Public_key:
      sig
        type%accessor t =
          { key_validity_url: string option
          ; public_key: string
          }
        val encoding: t encoding
      end
      type%accessor t =
        { display_name: string
        ; key_validity_url: string
        ; public_key: string
        ; public_keys: Public_key.t list option
        }
      val encoding: t encoding
    end
    module Guest_access:
    sig
      module Access:
      sig
        type t = Can_join | Forbidden
        val encoding: t encoding
      end
      type%accessor t = { guest_access: Access.t  }
      val encoding: t encoding
    end
    module Server_acl:
    sig
      type%accessor t =
        { allow_ip_literals: bool option
        ; allow: string list option
        ; deny: string list option
        }
      val encoding: t encoding
    end
    module Tombstone:
    sig
      type%accessor t = { body: string; replacement_room: string  }
      val encoding: t encoding
    end
    module Encryption:
    sig
      type%accessor t =
        { algorithm: string
        ; rotation_period_ms: int option
        ; rotation_period_msgs: int option
        }
      val encoding: t encoding
    end
    module Encrypted:
    sig
      module Algorithm:
      sig
        type t = Curve_sha2 | Aes_sha2
        val encoding: t encoding
        end
      module Cyphertext:
      sig
        module Cyphertext_info:
        sig
          type%accessor t = { body: string; msg_type: int  }
          val encoding: t encoding
        end
        type t =
            Megolm of string
          | Olm of (string * Cyphertext_info.t) list
        val encoding: t encoding
      end
      type%accessor t =
        { algorithm: Algorithm.t
        ; cyphertext: Cyphertext.t
        ; sender_key: string
        ; device_id: string option
        ; session_id: string option
        }
      val encoding: t encoding
    end
    module Message:
    sig
      module Text:
      sig
        type%accessor t =
          { body: string
          ; format: string option
          ; formatted_body: string option
          }
        val encoding: t encoding
      end
      module Emote:
      sig
        type%accessor t =
          { body: string
          ; format: string option
          ; formatted_body: string option
          }
        val encoding: t encoding
      end
      module Notice:
      sig
        type%accessor t = { body: string  }
        val encoding: t encoding
      end
      module Image:
      sig
        module Info:
        sig
          type%accessor t =
            { h: int option
            ; w: int option
            ; mimetype: string option
            ; size: int option
            ; thumbnail_url: string option
            ; thumbnail_file: Message.Encrypted_file.t option
            ; thumbnail_info: Message.Thumbnail.t option
            }
          val encoding: t encoding
        end
        type%accessor t =
          { body: string
          ; info: Message.Image.t option
          ; url: string
          ; file: Message.Encrypted_file.t option
          }
        val encoding: t encoding
      end
      module File:
      sig
        type%accessor t =
          { body: string
          ; filename: string option
          ; info: Message.File.t option
          ; url: string
          ; file: Message.Encrypted_file.t option
          }
        val encoding: t encoding
      end
      module Audio:
      sig
        type%accessor t =
          { body: string
          ; info: Message.Audio.t option
          ; url: string
          ; file: Message.Encrypted_file.t option
          }
        val encoding: t encoding
      end
      module Location:
      sig
        type%accessor t =
          { body: string
          ; info: Message.Location.t option
          ; geo_uri: string
          }
        val encoding: t encoding
      end
      module Video:
      sig
        type%accessor t =
          { body: string
          ; info: Message.Video.t option
          ; url: string
          ; file: Message.Encrypted_file.t option
          }
        val encoding: t encoding
      end
      module Sticker:
      sig
        type%accessor t =
          { body: string
          ; info: Message.Image.t
          ; url: string
          }
        val encoding: t encoding
      end
      module Server_notice:
      sig
        type%accessor t =
          { body: string
          ; server_notice_type: string
          ; admin_contact: string option
          ; limit_type: string option
          }
        val encoding: t encoding
      end
      type t =
          Text of Text.t
        | Emote of Emote.t
        | Notice of Notice.t
        | Image of Image.t
        | File of File.t
        | Audio of Audio.t
        | Location of Location.t
        | Video of Video.t
        | Sticker of Sticker.t
        | Server_notice of Server_notice.t
      val encoding: t encoding
    end
    module Name:
    sig
      type%accessor t = { name: string  }
      val encoding: t encoding
    end
    module Topic:
    sig
      type%accessor t = { topic: string  }
      val encoding: t encoding
    end
    module Avatar:
    sig
      type%accessor t = { info: Message.Image.t option; url: string  }
      val encoding: t encoding
    end
    module Pinned_events:
    sig
      type%accessor t = { pinned: string list  }
      val encoding: t encoding
    end
    module Call:
    sig
      module Invite:
      sig
        module Offer:
        sig
          type%accessor t = { sdp: string  }
          val encoding: t encoding
        end
        type%accessor t =
          { call_id: string
          ; offer: Offer.t
          ; version: int
          ; lifetime: int
          }
        val encoding: t encoding
      end
      module Candidates:
      sig
        module Candidate:
        sig
          type%accessor t =
            { sdpMid: string
            ; sdpMLineIndex: int
            ; candidate: string
            }
          val encoding: t encoding
        end
        type%accessor t =
          { call_id: string
          ; candidates: Candidate.t list
          ; version: int
          }
        val encoding: t encoding
      end
      module Answer:
      sig
        module Answer:
        sig
          type%accessor t = { sdp: string  }
          val encoding: t encoding
        end
        type%accessor t =
          { call_id: string
          ; answer: Answer.t
          ; version: int
          }
        val encoding: t encoding
      end
      module Hangup:
      sig
        module Reason:
        sig
          type t = Ice_failed | Invite_timeout
          val encoding: t encoding
        end
        type%accessor t =
          { call_id: string
          ; version: int
          ; reason: Reason.t option
          }
        val encoding: t encoding
      end
    end
    module Presence:
    sig
      module Presence:
      sig
        type t = Online | Offline | Unavailable
        val encoding: t encoding
      end
      type%accessor t =
        { avatar_url: string option
        ; displayname: string option
        ; last_active_ago: int option
        ; presence: Presence.t
        ; currently_active: bool option
        ; status_msg: string option
        }
      val encoding: t encoding
    end
    module Push_rules:
    sig
      type%accessor t =
        { content: Push_rule.t list option
        ; override: Push_rule.t list option
        ; room: Push_rule.t list option
        ; sender: Push_rule.t list option
        ; underride: Push_rule.t list option
        }
      val encoding: t encoding
    end
    module Typing:
    sig
      type%accessor t = { users_id: string list  }
      val encoding: t encoding
    end
    module Receipt:
    sig
      module Receipts:
      sig
        module Timestamp:
        sig
          type%accessor t = { ts: int  }
          val encoding: t encoding
        end
        type%accessor t = { users: (string * Timestamp.t) list  }
        val encoding: t encoding
      end
      type%accessor t = { receipts: (string * Receipts.t) list  }
      val encoding: t encoding
    end
    module Fully_read:
    sig
      type%accessor t = { event_id: string  }
      val encoding: t encoding
    end
    module Tag:
    sig
      module Tag:
      sig
        type%accessor t = { order: float option  }
        val encoding: t encoding
      end
      type%accessor t = { tags: (string * Tag.t) list  }
      val encoding: t encoding
    end
    module Direct:
    sig
      type%accessor t = { directs: (string * string list) list  }
      val encoding: t encoding
    end
    module Ignored_users_list:
    sig
      type%accessor t = { users: string list  }
      val encoding: t encoding
    end
    module Room_key:
    sig
      type%accessor t =
        { algorithm: string
        ; room_id: string
        ; session_id: string
        ; session_key: string
        }
      val encoding: t encoding
    end
    module Room_key_request:
    sig
      module Request_key_info:
      sig
        type%accessor t =
          { algorithm: string
          ; room_id: string
          ; sender_key: string
          ; session_key: string
          }
        val encoding: t encoding
      end
      module Action:
      sig
        type t = Request | Cancel_request
        val encoding: t encoding
      end
      type%accessor t =
        { body: Request_key_info.t
        ; action: Action.t
        ; requesting_device_id: string
        ; request_id: string
        }
      val encoding: t encoding
    end
    module Forwarded_room_key:
    sig
      type%accessor t =
        { algorithm: string
        ; room_id: string
        ; sender_key: string
        ; session_id: string
        ; session_key: string
        ; sender_claimed_ed25519_key: string
        ; forwarding_curve25519_key_chain: string list
        }
      val encoding: t encoding
    end
    module Dummy:
    sig
      type%accessor t = unit
      val encoding: t encoding
    end
    module Custom:
    sig
      type%accessor t = { content: Repr.value  }
      val encoding: t encoding
    end
    type t =
        Aliases of Aliases.t
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
      | Invite of Call.Invite.t
      | Candidates of Call.Candidates.t
      | Answer of Call.Answer.t
      | Hangup of Call.Hangup.t
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
      | Dummy of unit
    val encoding: t encoding
  end
  module Event:
  sig
    type%accessor t = { event_content: Event_content.t  }
    val encoding: t encoding
  end
  module Room_event:
  sig
    module Unsigned:
    sig
      type%accessor t =
        { age: int option
        ; redacted_because: Event.t option
        ; transaction_id: string option
        }
      val encoding: t encoding
    end
    type%accessor t =
      { event: Event.t
      ; event_id: string option
      ; sender: string option
      ; origin_server_ts: int option
      ; unsigned: Unsigned.t option
      ; room_id: string option
      }
    val encoding: t encoding
    val get_event_content: t -> Event_content.t
  end
  module State_event:
  sig
    type%accessor t = { room_event: Room_event.t; state_key: string  }
    val encoding: t encoding
    val get_event: t -> Event.t
    val get_event_content: t -> Event_content.t
  end
  type event =
    [ `Event of Event.t
    | `Room_event of Room_event.t
    | `State_event of State_event.t
    ]
  module Pdu:
  sig
    type%accessor t =
      { event: event
      ; prev_events: string list
      ; depth: int
      }
    val encoding: t encoding
    val get_event: t -> event
  end
end
