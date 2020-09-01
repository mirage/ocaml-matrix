open Json_encoding

module Visibility =
struct
  type t =
    | Public
    | Private

  let encoding =
    string_enum
      [ ("public", Public)
      ; ("private", Private) ]
end

module Create =
struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/createRoom"

  module Request =
  struct
    module Invite_3pid =
    struct
      type t =
        { id_server: string
        ; medium: string
        ; addresss: string
        } [@@deriving accessor]

      let encoding =
        let to_tuple t =
          t.id_server, t.medium, t.addresss
        in
        let of_tuple v =
          let id_server, medium, addresss = v in
          { id_server; medium; addresss }
        in
        let with_tuple =
          obj3
            (req "id_server" string)
            (req "medium" string)
            (req "addresss" string)
        in
        conv to_tuple of_tuple with_tuple
    end

    module Preset =
    struct
      type t =
        | Public
        | Private
        | Trusted_private

      let encoding =
        string_enum
          [ "public_chat", Public
          ; "private_chat", Private
          ; "trusted_private_chat", Trusted_private ]
    end

    type t =
      { visibility: Visibility.t option
      ; room_alias_name: string option
      ; name: string option
      ; topic: string option
      ; invite: string list option
      ; invite_3pid: Invite_3pid.t list option
      ; room_version: string option
      ; creation_content: Room_events.Room_event.Create.t option
      ; initial_state: State_events.t list option
      ; preset: Preset.t option
      ; is_direct: bool option
      ; power_level_content_override: Room_events.Room_event.Power_levels.t option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (t.visibility, t.room_alias_name, t.name, t.topic, t.invite, t.invite_3pid, t.room_version, t.creation_content, t.initial_state, t.preset), (t.is_direct, t.power_level_content_override)
      in
      let of_tuple v =
        let (visibility, room_alias_name, name, topic, invite, invite_3pid, room_version, creation_content, initial_state, preset), (is_direct, power_level_content_override) = v in
        { visibility; room_alias_name; name; topic; invite; invite_3pid; room_version; creation_content; initial_state; preset; is_direct; power_level_content_override }
      in
      let with_tuple =
        merge_objs
          (obj10
            (opt "visibility" Visibility.encoding)
            (opt "room_alias_name" string)
            (opt "name" string)
            (opt "topic" string)
            (opt "invite" (list string))
            (opt "invite_3pid" (list Invite_3pid.encoding))
            (opt "room_version" string)
            (opt "creation_content" Room_events.Room_event.Create.encoding)
            (opt "initial_state" (list State_events.encoding))
            (opt "preset" Preset.encoding))
          (obj2
            (opt "is_direct" bool)
            (opt "power_level_content_override" Room_events.Room_event.Power_levels.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response =
  struct
    type t =
      { room_id: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.room_id
      in
      let of_tuple v =
        let room_id = v in
        { room_id }
      in
      let with_tuple =
        obj1
          (req "room_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Create_alias =
struct
  module Query = Empty.Query

  let path room_alias = "_matrix/client/r0/directory/room/" ^ room_alias

  module Request =
  struct
    type t =
      { room_id: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.room_id
      in
      let of_tuple v =
        let room_id = v in
        { room_id }
      in
      let with_tuple =
        obj1
          (req "room_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Resolve_alias =
struct
  module Query = Empty.Query

  let path room_alias = "_matrix/client/r0/directory/room/" ^ room_alias

  module Request = Empty.Json

  module Response =
  struct
    type t =
      { room_id: string option
      ; servers: string list option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.room_id, t.servers
      in
      let of_tuple v =
        let room_id, servers = v in
        { room_id; servers }
      in
      let with_tuple =
        obj2
          (opt "room_id" string)
          (opt "servers" (list string))
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Delete_alias =
struct
  module Query = Empty.Query

  let path room_alias = "_matrix/client/r0/directory/room/" ^ room_alias

  module Request = Empty.Json

  let needs_auth = true
end
