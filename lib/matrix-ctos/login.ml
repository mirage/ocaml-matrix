open Json_encoding
open Matrix_common

module Get = struct
  module Query = Empty.Query

  module Response = struct
    type t = {types: string list option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.types in
      let of_tuple v =
        let types = v in
        {types} in
      let with_tuple = obj1 (opt "flows" (list (obj1 (req "type" string)))) in
      conv to_tuple of_tuple with_tuple

    let pp ppf t =
      Fmt.(pf ppf "{ types: %a }" Dump.(option (list string)) t.types)
  end
end

module Post = struct
  module Query = Empty.Query

  module Request = struct
    type t = {
      auth: Authentication.t;
      device_id: string option;
      initial_device_display_name: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.auth, (t.device_id, t.initial_device_display_name) in
      let of_tuple v =
        let auth, (device_id, initial_device_display_name) = v in
        {auth; device_id; initial_device_display_name} in
      let with_tuple =
        merge_objs Authentication.encoding
          (obj2 (opt "device_id" string)
             (opt "initial_device_display_name" string)) in
      conv to_tuple of_tuple with_tuple

    let pp ppf t =
      Fmt.(
        pf ppf "{ auth: %a; device_id: %a; initial_device_display_name: %a }"
          Authentication.pp t.auth
          Dump.(option string)
          t.device_id
          Dump.(option string)
          t.initial_device_display_name)
  end

  module Response = struct
    type t = {
      user_id: string option;
      access_token: string option;
      home_server: string option;
      device_id: string option;
      well_known: Well_known.Response.t option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.user_id, t.access_token, t.home_server, t.device_id, t.well_known
      in
      let of_tuple v =
        let user_id, access_token, home_server, device_id, well_known = v in
        {user_id; access_token; home_server; device_id; well_known} in
      let with_tuple =
        obj5 (opt "user_id" string)
          (opt "access_token" string)
          (opt "home_server" string) (opt "device_id" string)
          (opt "well_known" Well_known.Response.encoding) in
      conv to_tuple of_tuple with_tuple

    let pp ppf t =
      Fmt.(
        pf ppf
          "{ user_id: %a;  access_token: %a;  home_server: %a;  device_id: \
           %a;  well_known: %a }"
          Dump.(option string)
          t.user_id
          Dump.(option string)
          t.access_token
          Dump.(option string)
          t.home_server
          Dump.(option string)
          t.device_id
          Dump.(option Well_known.Response.pp)
          t.well_known)
  end
end
