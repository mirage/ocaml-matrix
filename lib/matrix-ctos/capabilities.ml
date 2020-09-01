open Json_encoding

module Capability =
struct
  module Change_password =
  struct
    type t =
      { enabled: bool
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.enabled
      in
      let of_tuple v =
        let enabled = v in
        {enabled}
      in
      let with_tuple =
        obj1
          (req "enabled" bool)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Room_versions =
  struct
    module Stability =
    struct
      type t =
        | Stable
        | Unstable

      let encoding =
        string_enum
          [ "stable", Stable
          ; "unstable", Unstable ]
    end

    type t =
      { default: string
      ; available: (string * Stability.t) list
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.default, t.available
      in
      let of_tuple v =
        let default, available = v in
        {default; available}
      in
      let with_tuple =
        obj2
          (req "default" string)
          (req "available" (assoc Stability.encoding))
      in
      conv to_tuple of_tuple with_tuple
  end

  module Custom =
  struct
    type t =
      { name: string
      ; content: Repr.value
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.content
      in
      let of_tuple v =
        let content = v in
        {content; name = ""}
      in
      let with_tuple =
        any
      in
      conv to_tuple of_tuple with_tuple
  end

  type t =
    | Change_password of Change_password.t
    | Room_versions of Room_versions.t
    | Custom of Custom.t

  let encoding =
    union
      [ case Change_password.encoding (function Change_password t -> Some t | _ -> None) (fun t -> Change_password t)
      ; case Room_versions.encoding (function Room_versions t -> Some t | _ -> None) (fun t -> Room_versions t)
      ; case Custom.encoding (function Custom t -> Some t | _ -> None) (fun t -> Custom t)]
end

module Query = Empty.Query

let path = "/_matrix/client/r0/capabilities"

module Response =
struct
  type t =
    { capabilities: Capability.t list
    } [@@deriving accessor]

  let encoding =
    let to_tuple t =
      let f = function
        | Capability.Change_password t ->
          let ez = construct Capability.Change_password.encoding t in
          "m.change_password", ez
        | Capability.Room_versions t ->
          let ez = construct Capability.Room_versions.encoding t in
          "m.room_versions", ez
        | Capability.Custom t ->
          let ez = construct Capability.Custom.encoding t in
          t.name, ez
      in
      List.map f t.capabilities
    in
    let of_tuple v =
      let f = function
        | ("m.change_password", ez) ->
          let t = destruct Capability.Change_password.encoding ez in
          Capability.Change_password t
        | ("m.room_versions", ez) ->
          let t = destruct Capability.Room_versions.encoding ez in
          Capability.Room_versions t
        | (s, ez) ->
          let t = destruct Capability.Custom.encoding ez in
          Capability.Custom {t with name = s}
      in
      let capabilities = List.map f v in
      {capabilities}
    in
    let with_tuple =
      obj1
        (req "capabilities"
          (assoc any))
    in
    conv to_tuple of_tuple with_tuple
end

let needs_auth = true
