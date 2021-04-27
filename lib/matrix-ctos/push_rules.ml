open Json_encoding
open Matrix_common

module Kind = struct
  type t = Override | Underride | Sender | Room | Content

  let to_string = function
    | Override -> "override"
    | Underride -> "underride"
    | Sender -> "sender"
    | Room -> "room"
    | Content -> "content"
end

module Get_all = struct
  module Query = Empty.Query

  let path = "/_matrix/client/r0/pushrules/"

  module Response = struct
    type t = {
        content: Push_rule.t list option
      ; override: Push_rule.t list option
      ; room: Push_rule.t list option
      ; sender: Push_rule.t list option
      ; underride: Push_rule.t list option
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        (t.content, t.override, t.room, t.sender, t.underride), () in
      let of_tuple v =
        let (content, override, room, sender, underride), () = v in
        {content; override; room; sender; underride} in
      let with_tuple =
        obj2
          (req "global"
             (obj5
                (opt "content" (list Push_rule.encoding))
                (opt "override" (list Push_rule.encoding))
                (opt "room" (list Push_rule.encoding))
                (opt "sender" (list Push_rule.encoding))
                (opt "underride" (list Push_rule.encoding))))
          (req "device" unit)
        (* Not really in the documentation, seems to be a feature to be *) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Get = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id

  module Response = struct
    type t = {push_rules: Push_rule.t} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.push_rules in
      let of_tuple v =
        let push_rules = v in
        {push_rules} in
      let with_tuple = Push_rule.encoding in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Delete = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id

  module Request = Empty.Json
  module Response = Empty.Json

  let needs_auth = true
end

module Put = struct
  module Query = struct
    type t = {before: string option; after: string option} [@@deriving accessor]

    let args t =
      let l = [] in
      let l =
        match t.before with
        | None -> l
        | Some before -> ("before", [before]) :: l in
      let l =
        match t.after with None -> l | Some after -> ("after", [after]) :: l
      in
      l
  end

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id

  module Request = struct
    module Action = struct
      type t = Notify | Dont_notify | Coalesce | Set_weak

      let encoding =
        string_enum
          [
            "notify", Notify; "dont_notify", Dont_notify; "coalesce", Coalesce
          ; "set_weak", Set_weak
          ]
    end

    type t = {
        actions: Action.t list
      ; conditions: Push_rule.Push_condition.t list
      ; pattern: string
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.actions, t.conditions, t.pattern in
      let of_tuple v =
        let actions, conditions, pattern = v in
        {actions; conditions; pattern} in
      let with_tuple =
        obj3
          (req "actions" (list Action.encoding))
          (req "conditions" (list Push_rule.Push_condition.encoding))
          (req "pattern" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Get_enabled = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id
    ^ "/"
    ^ "enabled"

  module Response = struct
    type t = {enabled: bool} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.enabled in
      let of_tuple v =
        let enabled = v in
        {enabled} in
      let with_tuple = obj1 (req "enabled" bool) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Set_enabled = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id
    ^ "/"
    ^ "enabled"

  module Request = struct
    type t = {enabled: bool} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.enabled in
      let of_tuple v =
        let enabled = v in
        {enabled} in
      let with_tuple = obj1 (req "enabled" bool) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Get_actions = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id
    ^ "/"
    ^ "actions"

  module Response = struct
    type t = {actions: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.actions in
      let of_tuple v =
        let actions = v in
        {actions} in
      let with_tuple = obj1 (req "actions" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Set_actions = struct
  module Query = Empty.Query

  let path scope kind rule_id =
    "_matrix/client/r0/pushrules/"
    ^ scope
    ^ "/"
    ^ Kind.to_string kind
    ^ "/"
    ^ rule_id
    ^ "/"
    ^ "actions"

  module Request = struct
    type t = {actions: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.actions in
      let of_tuple v =
        let actions = v in
        {actions} in
      let with_tuple = obj1 (req "actions" (list string)) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end
