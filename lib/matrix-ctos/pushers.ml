open Json_encoding
open Matrix_common

module Pusher = struct
  module Pusher_data = struct
    type t = {url: string option; format: string option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.url, t.format in
      let of_tuple v =
        let url, format = v in
        {url; format} in
      let with_tuple = obj2 (opt "url" string) (opt "format" string) in
      conv to_tuple of_tuple with_tuple
  end

  type t = {
      pushkey: string
    ; kind: string
    ; app_id: string
    ; app_display_name: string
    ; device_display_name: string
    ; profile_tag: string option
    ; lang: string
    ; data: Pusher_data.t
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.pushkey
      , t.kind
      , t.app_id
      , t.app_display_name
      , t.device_display_name
      , t.profile_tag
      , t.lang
      , t.data ) in
    let of_tuple v =
      let ( pushkey
          , kind
          , app_id
          , app_display_name
          , device_display_name
          , profile_tag
          , lang
          , data ) =
        v in
      {
        pushkey
      ; kind
      ; app_id
      ; app_display_name
      ; device_display_name
      ; profile_tag
      ; lang
      ; data
      } in
    let with_tuple =
      obj8 (req "pushkey" string) (req "kind" string) (req "app_id" string)
        (req "app_display_name" string)
        (req "device_display_name" string)
        (opt "profile_tag" string) (req "lang" string)
        (req "data" Pusher_data.encoding) in
    conv to_tuple of_tuple with_tuple
end

module Get = struct
  module Query = Empty.Query

  module Response = struct
    type t = {pushers: Pusher.t list option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.pushers in
      let of_tuple v =
        let pushers = v in
        {pushers} in
      let with_tuple = obj1 (opt "pushers" (list Pusher.encoding)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Set = struct
  module Query = Empty.Query

  module Request = struct
    type t = {pusher: Pusher.t; append: bool option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.pusher, t.append in
      let of_tuple v =
        let pusher, append = v in
        {pusher; append} in
      let with_tuple = merge_objs Pusher.encoding (obj1 (opt "append" bool)) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end
