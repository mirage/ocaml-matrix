open Json_encoding

module Invite =
struct
  module Query = Empty.Query

  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/invite"

  module Request =
  struct
    type t =
      { user_id: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.user_id
      in
      let of_tuple v =
        let user_id = v in
        {user_id}
      in
      let with_tuple =
        obj1
          (req "user_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Invite_thirdparty =
struct
  module Query = Empty.Query

  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/invite"

  module Request =
  struct
    type t =
      { id_server: string
      ; medium: string
      ; address: string
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.id_server, t.medium, t.address
      in
      let of_tuple v =
        let id_server, medium, address = v in
        { id_server; medium; address }
      in
      let with_tuple =
        obj3
          (req "id_server" string)
          (req "medium" string)
          (req "address" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  module Response = Empty.Json

  let needs_auth = true
end

module Join_with_id =
struct
  module Query = Empty.Query

  let path room_id = "_matrix/client/r0/rooms/" ^ room_id ^ "/join"

  module Request =
  struct
    type t =
      { third_party_signed: unit option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.third_party_signed
      in
      let of_tuple v =
        let third_party_signed = v in
        {third_party_signed}
      in
      let with_tuple =
        obj1
          (opt "third_party_signed" unit)
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
        {room_id}
      in
      let with_tuple =
        obj1
          (req "room_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end

module Join =
struct
  module Query =
  struct
    type t =
      { server_name:string list option
      } [@@deriving accessor]

    let args t =
      match t.server_name with
        | None -> []
        | Some server_name -> [("server_name", server_name)]
  end

  let path room_id_or_alias = "_matrix/client/r0/join/" ^ room_id_or_alias

  module Request =
  struct
    type t =
      { third_party_signed: unit option
      } [@@deriving accessor]

    let encoding =
      let to_tuple t =
        t.third_party_signed
      in
      let of_tuple v =
        let third_party_signed = v in
        {third_party_signed}
      in
      let with_tuple =
        obj1
          (opt "third_party_signed" unit)
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
        {room_id}
      in
      let with_tuple =
        obj1
          (req "room_id" string)
      in
      conv to_tuple of_tuple with_tuple
  end

  let needs_auth = true
end
