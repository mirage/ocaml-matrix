open Json_encoding

module Directory = struct
  module Response = struct
    type t = {room_id: string; servers: string list} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.room_id, t.servers in
      let of_tuple v =
        let room_id, servers = v in
        {room_id; servers} in
      let with_tuple =
        obj2 (req "room_id" string) (req "servers" (list string)) in
      conv to_tuple of_tuple with_tuple
  end
end

module Profile = struct
  module Response = struct
    type t = {avatar_url: string option; displayname: string option}
    [@@deriving accessor]

    let encoding =
      let to_tuple t = t.avatar_url, t.displayname in
      let of_tuple v =
        let avatar_url, displayname = v in
        {avatar_url; displayname} in
      let with_tuple =
        obj2 (opt "avatar_url" string) (opt "displayname" string) in
      conv to_tuple of_tuple with_tuple
  end
end
