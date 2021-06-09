open Json_encoding
open Matrix_common

module Get_visibility = struct
  module Query = Empty.Query

  module Response = struct
    type t = {visibility: Room.Visibility.t} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.visibility in
      let of_tuple v =
        let visibility = v in
        {visibility} in
      let with_tuple = obj1 (req "visibility" Room.Visibility.encoding) in
      conv to_tuple of_tuple with_tuple
  end
end

module Set_visibility = struct
  module Query = Empty.Query

  module Request = struct
    type t = {visibility: Room.Visibility.t option} [@@deriving accessor]

    let encoding =
      let to_tuple t = t.visibility in
      let of_tuple v =
        let visibility = v in
        {visibility} in
      let with_tuple = obj1 (opt "visibility" Room.Visibility.encoding) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json
end

module Get_public_rooms = struct
  module Query = struct
    type t = {limit: int option; since: string option; server: string option}
    [@@deriving accessor]

    let args t =
      let l =
        match t.limit with
        | None -> []
        | Some limit -> ["limit", [Int.to_string limit]] in
      let l =
        match t.since with None -> l | Some since -> ("since", [since]) :: l
      in
      match t.server with None -> l | Some server -> ("server", [server]) :: l
  end

  module Response = struct
    module Public_rooms_chunk = struct
      type t = {
        aliases: string list option;
        canonical_alias: string option;
        name: string option;
        num_joined_members: int;
        room_id: string;
        topic: string option;
        world_readable: bool;
        guest_can_join: bool;
        avatar_url: string option;
        federate: bool option;
            (* What ? I was supposed to follow the documentation ? - field present when calling synapse but not in the documentation *)
      }
      [@@deriving accessor]

      let encoding =
        let to_tuple t =
          ( t.aliases,
            t.canonical_alias,
            t.name,
            t.num_joined_members,
            t.room_id,
            t.topic,
            t.world_readable,
            t.guest_can_join,
            t.avatar_url,
            t.federate ) in
        let of_tuple v =
          let ( aliases,
                canonical_alias,
                name,
                num_joined_members,
                room_id,
                topic,
                world_readable,
                guest_can_join,
                avatar_url,
                federate ) =
            v in
          {
            aliases;
            canonical_alias;
            name;
            num_joined_members;
            room_id;
            topic;
            world_readable;
            guest_can_join;
            avatar_url;
            federate;
          } in
        let with_tuple =
          obj10
            (opt "aliases" (list string))
            (opt "canonical_alias" string)
            (opt "name" string)
            (req "num_joined_members" int)
            (req "room_id" string) (opt "topic" string)
            (req "world_readable" bool)
            (req "guest_can_join" bool)
            (opt "avatar_url" string) (opt "m.federate" bool) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      chunk: Public_rooms_chunk.t list;
      next_batch: string option;
      prev_batch: string option;
      total_room_count_estimate: int option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.chunk, t.next_batch, t.prev_batch, t.total_room_count_estimate in
      let of_tuple v =
        let chunk, next_batch, prev_batch, total_room_count_estimate = v in
        {chunk; next_batch; prev_batch; total_room_count_estimate} in
      let with_tuple =
        obj4
          (req "chunk" (list Public_rooms_chunk.encoding))
          (opt "next_batch" string) (opt "prev_batch" string)
          (opt "total_room_count_estimate" int) in
      conv to_tuple of_tuple with_tuple
  end
end

module Filter_public_rooms = struct
  module Query = struct
    type t = {server: string option} [@@deriving accessor]

    let args t =
      match t.server with None -> [] | Some server -> ["server", [server]]
  end

  module Request = struct
    module Filter = struct
      type t = {generic_search_term: string option} [@@deriving accessor]

      let encoding =
        let to_tuple t = t.generic_search_term in
        let of_tuple v =
          let generic_search_term = v in
          {generic_search_term} in
        let with_tuple = obj1 (opt "generic_search_term" string) in
        conv to_tuple of_tuple with_tuple
    end

    type t = {
      limit: int option;
      since: string option;
      filter: Filter.t option;
      include_all_networks: bool option;
      third_party_instance_id: string option;
    }
    [@@deriving accessor]

    let encoding =
      let to_tuple t =
        ( t.limit,
          t.since,
          t.filter,
          t.include_all_networks,
          t.third_party_instance_id ) in
      let of_tuple v =
        let limit, since, filter, include_all_networks, third_party_instance_id
            =
          v in
        {limit; since; filter; include_all_networks; third_party_instance_id}
      in
      let with_tuple =
        obj5 (opt "limit" int) (opt "since" string)
          (opt "filter" Filter.encoding)
          (opt "include_all_networks" bool)
          (opt "third_party_instance_id" string) in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Get_public_rooms.Response
end
