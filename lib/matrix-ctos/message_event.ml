open Json_encoding

module Message_event =
struct
  module Message =
  struct
    module Text =
    struct
      type t =
        { body: string
        ; format: string option
        ; formatted_body: string option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.format, t.formatted_body
        in
        let of_tuple v =
          let body, format, formatted_body = v in
          { body; format; formatted_body }
        in
        let with_tuple =
          obj3
            (req "body" string)
            (opt "format" string)
            (opt "formatted_body" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Emote =
    struct
      type t =
        { body: string
        ; format: string option
        ; formatted_body: string option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.format, t.formatted_body
        in
        let of_tuple v =
          let body, format, formatted_body = v in
          { body; format; formatted_body }
        in
        let with_tuple =
          obj3
            (req "body" string)
            (opt "format" string)
            (opt "formatted_body" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Notice =
    struct
      type t =
        { body: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body
        in
        let of_tuple v =
          let body = v in
          { body }
        in
        let with_tuple =
          obj1
            (req "body" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Image =
    struct
      type t =
        { body: string
        ; info: Image_info.t option
        ; url: string
        ; file: Encrypted_file.t option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.info, t.url, t.file
        in
        let of_tuple v =
          let body, info, url, file = v in
          { body; info; url; file }
        in
        let with_tuple =
          obj4
            (req "body" string)
            (opt "info" Image_info.encoding)
            (req "url" string)
            (opt "file" Encrypted_file.encoding)
        in
        conv to_tuple of_tuple with_tuple
    end

    module File =
    struct
      type t =
        { body: string
        ; filename: string option
        ; info: File_info.t option
        ; url: string
        ; file: Encrypted_file.t option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.filename, t.info, t.url, t.file
        in
        let of_tuple v =
          let body, filename, info, url, file = v in
          { body; filename; info; url; file }
        in
        let with_tuple =
          obj5
            (req "body" string)
            (opt "filename" string)
            (opt "info" File_info.encoding)
            (req "url" string)
            (opt "file" Encrypted_file.encoding)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Audio =
    struct
      type t =
        { body: string
        ; info: Audio_info.t option
        ; url: string
        ; file: Encrypted_file.t option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.info, t.url, t.file
        in
        let of_tuple v =
          let body, info, url, file = v in
          { body; info; url; file }
        in
        let with_tuple =
          obj4
            (req "body" string)
            (opt "info" Audio_info.encoding)
            (req "url" string)
            (opt "file" Encrypted_file.encoding)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Location =
    struct
      type t =
        { body: string
        ; info: Location_info.t option
        ; geo_uri: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.info, t.geo_uri
        in
        let of_tuple v =
          let body, info, geo_uri = v in
          { body; info; geo_uri }
        in
        let with_tuple =
          obj3
            (req "body" string)
            (opt "info" Location_info.encoding)
            (req "geo_uri" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Video =
    struct
      type t =
        { body: string
        ; info: Video_info.t option
        ; url: string
        ; file: Encrypted_file.t option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.info, t.url, t.file
        in
        let of_tuple v =
          let body, info, url, file = v in
          { body; info; url; file }
        in
        let with_tuple =
          obj4
            (req "body" string)
            (opt "info" Video_info.encoding)
            (req "url" string)
            (opt "file" Encrypted_file.encoding)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Sticker = (* Might not be at the good place: Should be taken out of message *)
    struct
      type t =
        { body: string
        ; info: Image_info.t
        ; url: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.info, t.url
        in
        let of_tuple v =
          let body, info, url = v in
          { body; info; url }
        in
        let with_tuple =
          obj3
            (req "body" string)
            (req "info" Image_info.encoding)
            (req "url" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Server_notice =
    struct
      type t =
        { body: string
        ; server_notice_type: string
        ; admin_contact: string option
        ; limit_type: string option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.body, t.server_notice_type, t.admin_contact, t.limit_type
        in
        let of_tuple v =
          let body, server_notice_type, admin_contact, limit_type = v in
          { body; server_notice_type; admin_contact; limit_type }
        in
        let with_tuple =
          obj4
            (req "body" string)
            (req "server_notice_type" string)
            (opt "admin_contact" string)
            (opt "limit_type" string)
        in
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
          | Server_notice _ -> "m.server_notice"
        in
        get_type t, t
      in
      let of_tuple v =
        let _, t = v in t
      in
      let with_tuple =
      cond
        (obj1 (req "msgtype" string))
        [ "m.text", case Text.encoding (function Text t -> Some t | _ -> None) (fun t -> Text t)
        ; "m.emote", case Emote.encoding (function Emote t -> Some t | _ -> None) (fun t -> Emote t)
        ; "m.notice", case Notice.encoding (function Notice t -> Some t | _ -> None) (fun t -> Notice t)
        ; "m.image", case Image.encoding (function Image t -> Some t | _ -> None) (fun t -> Image t)
        ; "m.file", case File.encoding (function File t -> Some t | _ -> None) (fun t -> File t)
        ; "m.audio", case Audio.encoding (function Audio t -> Some t | _ -> None) (fun t -> Audio t)
        ; "m.location", case Location.encoding (function Location t -> Some t | _ -> None) (fun t -> Location t)
        ; "m.video", case Video.encoding (function Video t -> Some t | _ -> None) (fun t -> Video t)
        ; "m.sticker", case Sticker.encoding (function Sticker t -> Some t | _ -> None) (fun t -> Sticker t)
        ; "m.server_notice", case Server_notice.encoding (function Server_notice t -> Some t | _ -> None) (fun t -> Server_notice t) ]
      in
        conv to_tuple of_tuple with_tuple
  end

  module Feedback =
  struct
    module Feedback_type =
    struct
      type t =
        | Delivered
        | Read

      let encoding =
        string_enum
          [ ("delivered", Delivered)
          ; ("read", Read) ]
    end

    type t =
      { target_event_id: string
      ; feedback_type: Feedback_type.t
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.target_event_id, t.feedback_type
      in
      let of_tuple v =
        let target_event_id, feedback_type = v in
        {target_event_id; feedback_type}
      in
      let with_tuple =
        obj2
          (req "target_event_id" string)
          (req "type" Feedback_type.encoding)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Name =
  struct
    type t =
      { name: string;
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.name
      in
      let of_tuple v =
        let name = v in
        {name}
      in
      let with_tuple =
        obj1
          (req "name" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Topic =
  struct
    type t =
      { topic: string;
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.topic
      in
      let of_tuple v =
        let topic = v in
        {topic}
      in
      let with_tuple =
        obj1
          (req "topic" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Avatar =
  struct
    type t =
      { info: Image_info.t option
      ; url: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.info, t.url
      in
      let of_tuple v =
        let info, url = v in
        {info; url}
      in
      let with_tuple =
        obj2
          (opt "info" Image_info.encoding)
          (req "url" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Pinned_events =
  struct
    type t =
      { pinned: string list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.pinned
      in
      let of_tuple v =
        let pinned = v in
        {pinned}
      in
      let with_tuple =
        obj1
          (req "pinned" (list string))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Call =
  struct
    module Invite =
    struct
      module Offer =
      struct
        type t =
          { sdp: string
          } [@@deriving accessor]

        let encoding =
          let to_tuple t =
            (), t.sdp
          in
          let of_tuple v =
            let (), sdp = v in
            { sdp }
          in
          let with_tuple =
            obj2
              (req "type" (constant "offer"))
              (req "sdp" string)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t =
        { call_id: string
        ; offer: Offer.t
        ; version: int
        ; lifetime: int
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.call_id, t.offer, t.version, t.lifetime
        in
        let of_tuple v =
          let call_id, offer, version, lifetime = v in
          { call_id; offer; version; lifetime }
        in
        let with_tuple =
          obj4
            (req "call_id" string)
            (req "offer" Offer.encoding)
            (req "version" int)
            (req "lifetime" int)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Candidates =
    struct
      module Candidate =
      struct
        type t =
          { sdpMid: string
          ; sdpMLineIndex: int
          ; candidate: string
          } [@@deriving accessor]

        let encoding =
          let to_tuple t =
            t.sdpMid, t.sdpMLineIndex, t.candidate
          in
          let of_tuple v =
            let sdpMid, sdpMLineIndex, candidate = v in
            { sdpMid; sdpMLineIndex; candidate }
          in
          let with_tuple =
            obj3
              (req "sdpMid" string)
              (req "sdpMLineIndex" int)
              (req "candidate" string)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t =
        { call_id: string
        ; candidates: Candidate.t list
        ; version: int
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.call_id, t.candidates, t.version
        in
        let of_tuple v =
          let call_id, candidates, version = v in
          { call_id; candidates; version }
        in
        let with_tuple =
          obj3
            (req "call_id" string)
            (req "candidates" (list Candidate.encoding))
            (req "version" int)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Answer =
    struct
      module Answer =
      struct
        type t =
          { sdp: string
          } [@@deriving accessor]

        let encoding =
          let to_tuple t =
            (), t.sdp
          in
          let of_tuple v =
            let (), sdp = v in
            { sdp }
          in
          let with_tuple =
            obj2
              (req "type" (constant "answer"))
              (req "sdp" string)
          in
          conv to_tuple of_tuple with_tuple
      end

      type t =
        { call_id: string
        ; answer: Answer.t
        ; version: int
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.call_id, t.answer, t.version
        in
        let of_tuple v =
          let call_id, answer, version = v in
          { call_id; answer; version }
        in
        let with_tuple =
          obj3
            (req "call_id" string)
            (req "offer" Answer.encoding)
            (req "version" int)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Hangup =
    struct
      module Reason =
      struct
        type t =
          | Ice_failed
          | Invite_timeout

        let encoding =
          string_enum
            [ ("ice_failed", Ice_failed)
            ; ("invite_timeout", Invite_timeout) ]
      end

      type t =
        { call_id: string
        ; version: int
        ; reason: Reason.t option
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.call_id, t.version, t.reason
        in
        let of_tuple v =
          let call_id, version, reason = v in
          { call_id; version; reason }
        in
        let with_tuple =
          obj3
            (req "call_id" string)
            (req "version" int)
            (opt "reason" Reason.encoding)
        in
        conv to_tuple of_tuple with_tuple
    end
  end

  type t =
    | Message of Message.t
    | Feedback of Feedback.t
    | Name of Name.t
    | Topic of Topic.t
    | Avatar of Avatar.t
    | Pinned of Pinned_events.t
    | Invite of Call.Invite.t
    | Candidates of Call.Candidates.t
    | Answer of Call.Answer.t
    | Hangup of Call.Hangup.t

  let encoding =
    let to_tuple t =
      let get_type = function
        | Message _ -> "m.room.message"
        | Feedback _ -> "m.room.message.feedback"
        | Name _ -> "m.room.name"
        | Topic _ -> "m.room.topic"
        | Avatar _ -> "m.room.avatar"
        | Pinned _ -> "m.room.pinned_events"
        | Invite _ -> "m.call.invite"
        | Candidates _ -> "m.call.candidates"
        | Answer _ -> "m.call.answer"
        | Hangup _ -> "m.call.hangup"
      in
      get_type t, t
    in
    let of_tuple v =
      let _, t = v in t
    in
    let with_tuple =
    cond
      (obj1 (req "type" string))
      [ "m.room.message", case (Content.content Message.encoding) (function Message t -> Some t | _ -> None) (fun t -> Message t)
      ; "m.room.message.feedback", case (Content.content Feedback.encoding) (function Feedback t -> Some t | _ -> None) (fun t -> Feedback t)
      ; "m.room.name", case (Content.content Name.encoding) (function Name t -> Some t | _ -> None) (fun t -> Name t)
      ; "m.room.topic", case (Content.content Topic.encoding) (function Topic t -> Some t | _ -> None) (fun t -> Topic t)
      ; "m.room.avatar", case (Content.content Avatar.encoding) (function Avatar t -> Some t | _ -> None) (fun t -> Avatar t)
      ; "m.room.pinned_events", case (Content.content Pinned_events.encoding) (function Pinned t -> Some t | _ -> None) (fun t -> Pinned t)
      ; "m.call.invite", case (Content.content Call.Invite.encoding) (function Invite t -> Some t | _ -> None) (fun t -> Invite t)
      ; "m.call.candidates", case (Content.content Call.Candidates.encoding) (function Candidates t -> Some t | _ -> None) (fun t -> Candidates t)
      ; "m.call.answer", case (Content.content Call.Answer.encoding) (function Answer t -> Some t | _ -> None) (fun t -> Answer t)
      ; "m.call.hangup", case (Content.content Call.Hangup.encoding) (function Hangup t -> Some t | _ -> None) (fun t -> Hangup t) ]
    in
    conv to_tuple of_tuple with_tuple
end

type t =
  { event: Message_event.t
  ; event_id: string
  ; sender: string
  ; origin_server_ts: int
  ; unsigned: Room_events.Unsigned.t option
  ; room_id: string option
  ; user_id: string option
  ; age: int option
  } [@@deriving accessor]

let encoding =
  let to_tuple t =
    t.event, (t.event_id, t.sender, t.origin_server_ts, t.unsigned, t.room_id, t.user_id, t.age)
  in
  let of_tuple v =
    let event, (event_id, sender, origin_server_ts, unsigned, room_id, user_id, age) = v in
    {event; event_id; sender; origin_server_ts; unsigned; room_id; user_id; age}
  in
  let with_tuple =
    merge_objs
      Message_event.encoding
      (obj7
        (req "event_id" string)
        (req "sender" string)
        (req "origin_server_ts" int)
        (opt "unsigned" Room_events.Unsigned.encoding)
        (opt "room_id" string)
        (opt "user_id" string) (* Not in the documentation *)
        (opt "age" int)) (* Not in the documentation *)
  in
  conv to_tuple of_tuple with_tuple
