open Json_encoding

module Presence =
struct
  module Presence =
  struct
    type t =
      | Online
      | Offline
      | Unavailable

    let encoding =
      string_enum
        [ "online" , Online
        ; "offline" , Offline
        ; "unavailable" , Unavailable ]
  end

  type t =
    { sender: string
    ; avatar_url: string option
    ; displayname: string option
    ; last_active_ago: int option
    ; presence: Presence.t
    ; currently_active: bool option
    ; status_msg: string option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.sender, (t.avatar_url, t.displayname, t.last_active_ago, t.presence, t.currently_active, t.status_msg)
    in
    let of_tuple v =
      let sender, (avatar_url, displayname, last_active_ago, presence, currently_active, status_msg) = v in
      { sender; avatar_url; displayname; last_active_ago; presence; currently_active; status_msg }
    in
    let with_tuple =
      obj2
        (req "sender" string)
        (req "content"
          (obj6
            (opt "avatar_url" string)
            (opt "displayname" string)
            (opt "last_active_ago" int)
            (req "presence" Presence.encoding)
            (opt "currently_active" bool)
            (opt "status_msg" string)))
    in
    conv to_tuple of_tuple with_tuple
end

module Push_rules =
struct
  type t =
    { content: Push_rule.t list option
    ; override: Push_rule.t list option
    ; room: Push_rule.t list option
    ; sender: Push_rule.t list option
    ; underride: Push_rule.t list option
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      (t.content, t.override, t.room, t.sender, t.underride), ()
    in
    let of_tuple v =
      let (content, override, room, sender, underride), () = v in
      { content; override; room; sender; underride }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj2
            (req "global"
              (obj5
                (opt "content" (list Push_rule.encoding))
                (opt "override" (list Push_rule.encoding))
                (opt "room" (list Push_rule.encoding))
                (opt "sender" (list Push_rule.encoding))
                (opt "underride" (list Push_rule.encoding))))
            (req "device" unit))) (* Not really in the documentation, seems to be a feature to be *)
    in
    conv to_tuple of_tuple with_tuple
end

module Typing =
struct
  type t =
    { users_id: string list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.users_id
    in
    let of_tuple v =
      let users_id = v in
      { users_id }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj1
            (req "user_ids" (list string))))
    in
    conv to_tuple of_tuple with_tuple
end

module Receipt =
struct
  module Receipts =
  struct
    module Timestamp =
    struct
      type t =
        { ts: int
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.ts
        in
        let of_tuple v =
          let ts = v in
          { ts }
        in
        let with_tuple =
          obj1
            (req "ts" int)
        in
        conv to_tuple of_tuple with_tuple
    end

    type t =
      { users: (string * Timestamp.t) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.users
      in
      let of_tuple v =
        let users = v in
        { users }
      in
      let with_tuple =
        obj1
          (req "m.read" (assoc Timestamp.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    { receipts: (string * Receipts.t) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.receipts
    in
    let of_tuple v =
      let receipts = v in
      { receipts }
    in
    let with_tuple =
      obj1
        (req "content" (assoc Receipts.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

module Fully_read =
struct
  type t =
    { event_id: string
    ; room_id: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.event_id, t.room_id
    in
    let of_tuple v =
      let event_id, room_id = v in
      { event_id; room_id }
    in
    let with_tuple =
      obj2
        (req "content"
          (obj1 (req "event_id" string)))
        (req "room_id" string)
    in
    conv to_tuple of_tuple with_tuple
end

module Tag =
struct
  module Tag =
  struct
    type t =
      { order: float option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.order
      in
      let of_tuple v =
        let order = v in
        { order }
      in
      let with_tuple =
        obj1
          (opt "order" float)
      in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    { tags: (string * Tag.t) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.tags
    in
    let of_tuple v =
      let tags = v in
      { tags }
    in
    let with_tuple =
      obj1
        (req "tags" (assoc Tag.encoding))
    in
    conv to_tuple of_tuple with_tuple
end

module Direct =
struct
  type t =
    { directs: (string * (string list)) list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.directs
    in
    let of_tuple v =
      let directs = v in
      { directs }
    in
    let with_tuple =
      obj1
        (req "content" (assoc (list string)))
    in
    conv to_tuple of_tuple with_tuple
end

module Ignored_users_list =
struct
  type t =
    { users: string list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      let f u = u, () in
      List.map f t.users
    in
    let of_tuple v =
      let f (u, ()) = u in
      let users = v in
      let users = List.map f users in
      { users }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj1
            (req "ignored_users" (assoc unit))))
    in
    conv to_tuple of_tuple with_tuple
end

(* module Any = (* ourf *)
struct
  type t =
    { content: Repr.value
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.content
    in
    let of_tuple v =
      let content = v in
      { content }
    in
    let with_tuple =
      obj1
        (req "content" any)
    in
    conv to_tuple of_tuple with_tuple
end *)

module Room_key =
struct
  type t =
    { algorithm: string
    ; room_id: string
    ; session_id: string
    ; session_key: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.algorithm, t.room_id, t.session_id, t.session_key
    in
    let of_tuple v =
      let algorithm, room_id, session_id, session_key = v in
      { algorithm; room_id; session_id; session_key }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj4
            (req "algorithm" string)
            (req "room_id" string)
            (req "session_id" string)
            (req "session_key" string)))
    in
    conv to_tuple of_tuple with_tuple
end

module Room_key_request =
struct
  module Request_key_info =
  struct
    type t =
      { algorithm: string
      ; room_id: string
      ; sender_key: string
      ; session_key: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.algorithm, t.room_id, t.sender_key, t.session_key
      in
      let of_tuple v =
        let algorithm, room_id, sender_key, session_key = v in
        { algorithm; room_id; sender_key; session_key }
      in
      let with_tuple =
        obj4
          (req "algorithm" string)
          (req "room_id" string)
          (req "sender_key" string)
          (req "session_key" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Action =
  struct
    type t =
      | Request
      | Cancel_request

    let encoding =
      string_enum
        [ "request", Request
        ; "cancel_request", Cancel_request ]
  end

  type t =
    { body: Request_key_info.t
    ; action: Action.t
    ; requesting_device_id: string
    ; request_id: string
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.body, t.action, t.requesting_device_id, t.request_id
    in
    let of_tuple v =
      let body, action, requesting_device_id, request_id = v in
      { body; action; requesting_device_id; request_id }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj4
            (req "body" Request_key_info.encoding)
            (req "action" Action.encoding)
            (req "requesting_device_id" string)
            (req "request_id" string)))
    in
    conv to_tuple of_tuple with_tuple
end

module Forwarded_room_key =
struct
  type t =
    { algorithm: string
    ; room_id: string
    ; sender_key: string
    ; session_id: string
    ; session_key: string
    ; sender_claimed_ed25519_key: string
    ; forwarding_curve25519_key_chain: string list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      t.algorithm, t.room_id, t.sender_key, t.session_id, t.session_key, t.sender_claimed_ed25519_key, t.forwarding_curve25519_key_chain
    in
    let of_tuple v =
      let algorithm, room_id, sender_key, session_id, session_key, sender_claimed_ed25519_key, forwarding_curve25519_key_chain = v in
      { algorithm; room_id; sender_key; session_id; session_key; sender_claimed_ed25519_key; forwarding_curve25519_key_chain }
    in
    let with_tuple =
      obj1
        (req "content"
          (obj7
            (req "algorithm" string)
            (req "room_id" string)
            (req "sender_key" string)
            (req "session_id" string)
            (req "session_key" string)
            (req "sender_claimed_ed25519_key" string)
            (req "forwarding_curve25519_key_chain" (list string))))
    in
    conv to_tuple of_tuple with_tuple
end

module Dummy =
struct
  type t = unit [@@deriving accessor]

  let encoding =
    obj1
      (req "content" unit)
end

type t =
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
  (* | Any of Any.t *)

let encoding =
  let to_tuple t =
    let get_type = function
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
      (* | Any _ -> "" *)
    in
    get_type t, t
  in
  let of_tuple v =
    let _, t = v in t
  in
  let with_tuple =
    cond
      (obj1 (req "type" string))
      [ "m.presence", case Presence.encoding (function Presence t -> Some t | _ -> None) (fun t -> Presence t)
      ; "m.push_rules", case Push_rules.encoding (function Push_rules t -> Some t | _ -> None) (fun t -> Push_rules t)
      ; "m.typing", case Typing.encoding (function Typing t -> Some t | _ -> None) (fun t -> Typing t)
      ; "m.receipt", case Receipt.encoding (function Receipt t -> Some t | _ -> None) (fun t -> Receipt t)
      ; "m.fully_read", case Fully_read.encoding (function Fully_read t -> Some t | _ -> None) (fun t -> Fully_read t)
      ; "m.tag", case Tag.encoding (function Tag t -> Some t | _ -> None) (fun t -> Tag t)
      ; "m.direct", case Direct.encoding (function Direct t -> Some t | _ -> None) (fun t -> Direct t)
      ; "m.room_key", case Room_key.encoding (function Room_key t -> Some t | _ -> None) (fun t -> Room_key t)
      ; "m.room_key_request", case Room_key_request.encoding (function Room_key_request t -> Some t | _ -> None) (fun t -> Room_key_request t)
      ; "m.forwarded_room_key", case Forwarded_room_key.encoding (function Forwarded_room_key t -> Some t | _ -> None) (fun t -> Forwarded_room_key t)
      ; "m.dummy", case Dummy.encoding (function Dummy t -> Some t | _ -> None) (fun t -> Dummy t) ]
  in
  conv to_tuple of_tuple with_tuple
