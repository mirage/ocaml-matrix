open Json_encoding
open Matrix_common

module Device = struct
  type t = {
    user_id: string option;
    (* Once again not advertised in the documentation *)
    device_id: string;
    display_name: string option;
    last_seen_ip: string option;
    last_seen_ts: int option;
  }
  [@@deriving accessor]

  let encoding =
    let to_tuple t =
      ( t.user_id,
        t.device_id,
        Some t.display_name,
        Some t.last_seen_ip,
        Some t.last_seen_ts ) in
    let of_tuple v =
      let user_id, device_id, display_name, last_seen_ip, last_seen_ts = v in
      let display_name = match display_name with Some d -> d | None -> None in
      let last_seen_ip = match last_seen_ip with Some d -> d | None -> None in
      let last_seen_ts = match last_seen_ts with Some d -> d | None -> None in
      {user_id; device_id; display_name; last_seen_ip; last_seen_ts} in
    let with_tuple =
      obj5 (opt "user_id" string) (req "device_id" string)
        (opt "display_name" Null.string)
        (opt "last_seen_ip" Null.string)
        (opt "last_seen_ts" Null.int) in
    conv to_tuple of_tuple with_tuple
end

module List = struct
  module Query = Empty.Query

  module Response = struct
    type t = {devices: Device.t list option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.devices in
      let of_tuple v =
        let devices = v in
        {devices} in
      let with_tuple = obj1 (opt "devices" (list Device.encoding)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Get = struct module Query = Empty.Query module Response = Device end

module Put = struct
  module Query = Empty.Query

  module Request = struct
    type t = {display_name: string option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.display_name in
      let of_tuple v =
        let display_name = v in
        {display_name} in
      let with_tuple = obj1 (opt "display_name" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Delete = struct
  module Query = Empty.Query

  module Request = struct
    type t = {auth: Authentication.t option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.auth in
      let of_tuple v =
        let auth = v in
        {auth} in
      let with_tuple = obj1 (opt "auth" Authentication.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Delete_list = struct
  module Query = Empty.Query

  module Request = struct
    type t = {devices: string list; auth: Authentication.t option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.devices, t.auth in
      let of_tuple v =
        let devices, auth = v in
        {devices; auth} in
      let with_tuple =
        obj2 (req "devices" (list string)) (opt "auth" Authentication.encoding)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end
