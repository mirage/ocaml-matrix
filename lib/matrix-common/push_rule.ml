open Json_encoding

module Action = struct
  module Tweak = struct
    type t = {tweak: string; value: Ezjsonm.value option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.tweak, t.value in
      let of_tuple v =
        let tweak, value = v in
        {tweak; value} in
      let with_tuple = obj2 (req "set_tweak" string) (opt "value" any) in
      conv to_tuple of_tuple with_tuple
  end

  type t = Notify | Dont_notify | Coalesce | Tweak of Tweak.t

  let encoding =
    union
      [
        case (constant "notify")
          (function Notify -> Some () | _ -> None)
          (fun () -> Notify);
        case (constant "dont_notify")
          (function Dont_notify -> Some () | _ -> None)
          (fun () -> Dont_notify);
        case (constant "coalesce")
          (function Coalesce -> Some () | _ -> None)
          (fun () -> Coalesce);
        case Tweak.encoding
          (function Tweak t -> Some t | _ -> None)
          (fun t -> Tweak t);
      ]
end

module Push_condition = struct
  type t = {
    kind: string;
    key: string option;
    pattern: string option;
    is: string option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.kind, t.key, t.pattern, t.is in
    let of_tuple v =
      let kind, key, pattern, is = v in
      {kind; key; pattern; is} in
    let with_tuple =
      obj4 (req "kind" string) (opt "key" string) (opt "pattern" string)
        (opt "is" string) in
    conv to_tuple of_tuple with_tuple
end

type t = {
  actions: Action.t list;
  default: bool;
  enabled: bool;
  rule_id: string;
  conditions: Push_condition.t list option;
  pattern: string option;
}
[@@deriving accessor]

let encoding =
  let to_tuple t =
    t.actions, t.default, t.enabled, t.rule_id, t.conditions, t.pattern in
  let of_tuple v =
    let actions, default, enabled, rule_id, conditions, pattern = v in
    {actions; default; enabled; rule_id; conditions; pattern} in
  let with_tuple =
    obj6
      (req "actions" (list Action.encoding))
      (req "default" bool) (req "enabled" bool) (req "rule_id" string)
      (opt "conditions" (list Push_condition.encoding))
      (opt "pattern" string) in
  conv to_tuple of_tuple with_tuple
