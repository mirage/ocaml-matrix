open Json_encoding

(* do something about the password field, stocking plain password is a big
  yikes *)
module User = struct
  type t = {username: string; password: string; devices: string list}
  [@@deriving accessor]

  let encoding =
    let to_tuple t = t.username, t.password, t.devices in
    let of_tuple v =
      let username, password, devices = v in
      {username; password; devices} in
    let with_tuple =
      obj3 (req "username" string) (req "password" string)
        (req "devices" (list string)) in
    conv to_tuple of_tuple with_tuple
end

module Device = struct
  type t = {user: string; token: string} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.user, t.token in
    let of_tuple v =
      let user, token = v in
      {user; token} in
    let with_tuple = obj2 (req "user" string) (req "token" string) in
    conv to_tuple of_tuple with_tuple
end

module Token = struct
  type t = {device: string; expires_at: float} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.device, t.expires_at in
    let of_tuple v =
      let device, expires_at = v in
      {device; expires_at} in
    let with_tuple = obj2 (req "device" string) (req "expires_at" float) in
    conv to_tuple of_tuple with_tuple
end

module Alias = struct
  type t = {room_id: string} [@@deriving accessor]

  let encoding =
    let to_tuple t = t.room_id in
    let of_tuple v =
      let room_id = v in
      {room_id} in
    let with_tuple = obj1 (req "room_id" string) in
    conv to_tuple of_tuple with_tuple
end
